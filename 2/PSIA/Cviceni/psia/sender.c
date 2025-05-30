#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/time.h> 
#include <arpa/inet.h>
#include <string.h>
#include <unistd.h>
#include <zlib.h>
#include "packet.h"
#include "debug_print.h"
#include "sender.h"
#include "queue.h"

#define EH(e) error_handler(e, __LINE__, __FILE__)
#define GF_WINDOW(x) ((sending_packet*) get_from_queue(window, x))
#define GWS get_queue_size(window)

sending_packet* compute_entry_packet(int argc, char* argv[]);
errors send_content(char* file_name, int sending_socket_desc, int ack_socket_desc, struct sockaddr* server_addr, sending_packet* entry);
sending_packet* new_sending_packet();
errors establish_connections(struct sockaddr_in* server_addr, struct sockaddr_in* my_addr, int* sending_socket_desc, int* ack_socket_desc);
errors send_packet(packet_t* data, int data_length, int sending_socket_desc, struct sockaddr* server_addr);
void error_handler(errors err_num, int line, char* file);

int main(int argc, char* argv[]){
    sending_packet* entry = compute_entry_packet(argc, argv);
    DEBUG_PRINT("Entry packet computed\n   - number of expected packets:");
    DEBUG_PRINT_I(entry->packet.entry_p.num_of_packets);

    struct sockaddr_in server_addr;
    struct sockaddr_in my_addr;
    int sending_socket_desc;
    int ack_socket_desc;
    errors e;
    if((e = establish_connections(&server_addr, &my_addr, &sending_socket_desc, &ack_socket_desc))){
        EH(e);
    }
    DEBUG_PRINT("Connection established\n");

    int return_value = send_content(argv[1], sending_socket_desc, ack_socket_desc, (struct sockaddr*) &server_addr, entry);
    close(sending_socket_desc);
    return return_value;
}


sending_packet* compute_entry_packet(int argc, char* argv[]){
    if (argc < 2){EH(WRONG_INPUT);}

    FILE* file = fopen(argv[1], "rb");
    if (!file){EH(FILE_ERROR);}
    DEBUG_PRINT("File exist\n");
    fseek(file, 0, SEEK_END);
    uint64_t file_size = ftell(file);
    fclose(file);
    DEBUG_PRINT("File size: ");
    DEBUG_PRINT_I(file_size);

    sending_packet* first_packet = new_sending_packet();
    first_packet->packet.entry_p.num_of_packets = (file_size / ((uint64_t)  MAX_PAYLOAD_SIZE)) + ((file_size % ((uint64_t) MAX_PAYLOAD_SIZE)) && 1) + 2;  // potreba prizpusobit MAX_PAYLOAD_SIZE
    first_packet->packet.entry_p.packet_num = 0;

    char* file_name = strrchr(argv[1], '/');
    //fprintf(stderr, "%p\n", file_name);
    file_name = file_name == NULL ? argv[1] : file_name + 1;
    //fprintf(stderr, "%i", strlen(file_name));
    if(strlen(file_name) > MAX_FILE_NAME){EH(FILE_NAME_TOO_LONG);}
    strcpy(first_packet->packet.entry_p.file_name, file_name);
    first_packet->packet.entry_p.crc = crc32(first_packet->packet.entry_p.crc, (const Bytef *) first_packet + 4, sizeof(entry_packet) - 4);

    first_packet->size = sizeof(entry_packet);
    //fprintf(stderr, "%s\n", first_packet->file_name);
    return first_packet;
}

errors send_content(char* file_name, int sending_socket_desc, int ack_socket_desc, struct sockaddr* server_addr, sending_packet* entry){
    FILE* file = fopen(file_name, "rb");
    //data_packet data;
    acknowledge_packet ack_packet;
    DEBUG_PRINT("Sending data packets\n");
    uint64_t num_of_packets = entry->packet.entry_p.num_of_packets;

    queue_t* window = create_queue(MAX_PACKET_NUM, false);
    push_to_queue(window, entry);
    //(sending_packet*) malloc(sizeof(sending_packet) * MAX_PACKET_NUM);
    uint64_t first_packet_in_window = 0;
    uint64_t window_size = 1;
    SHA256_CTX sha;
    if (SHA256_Init(&sha) != 1) {
        DEBUG_PRINT("ERROR initializing SHA context!\n");
        EH(SHA_ERROR);
    }

    struct timeval tv;
    bool waiting_for_ack = true;

    sending_packet* send_p = new_sending_packet();
    while(first_packet_in_window != num_of_packets){
        while (GWS < window_size && (send_p->size = fread(send_p->packet.data_p.payload, 1, MAX_PAYLOAD_SIZE, file))){
            if (SHA256_Update(&sha, send_p->packet.data_p.payload, send_p->size) != 1) {
                DEBUG_PRINT("ERROR updating SHA context!\n");
                return EXIT_FAILURE;
            }
            DEBUG_PRINT("New data read, length: ");
            DEBUG_PRINT_I(send_p->size);
            send_p->packet.data_p.packet_num = first_packet_in_window + get_queue_size(window);
            send_p->size += (sizeof(data_packet) - ((uint64_t) MAX_PAYLOAD_SIZE));
            send_p->packet.data_p.crc = crc32(send_p->packet.data_p.crc, (const Bytef *) send_p + 4, send_p->size - 4);

            push_to_queue(window, (void*) send_p);
            send_p = new_sending_packet();
        }
        DEBUG_PRINT("Current window size: ");
        DEBUG_PRINT_I(GWS);
        //DEBUG_PRINT("SHA packet generated\n   - size: ");
        //DEBUG_PRINT_I(send_p->size);
        while(waiting_for_ack){
            if(recvfrom(ack_socket_desc, &ack_packet, sizeof(ack_packet), 0, NULL, 0) > 0){
                DEBUG_PRINT("ACK packet received\n");
                //zkontrolovat ack
                uLong control_crc = crc32(0L, Z_NULL, 0);
                control_crc = crc32(control_crc, (const Bytef *) (&ack_packet) + 4, sizeof(ack_packet) - 4);
                if (control_crc != ack_packet.crc){
                    DEBUG_PRINT("Faulty CRC received!\n   - Packet:");
                    DEBUG_PRINT_I(ack_packet.packet_num);
                    continue;
                }

                if(((ack_packet.packet_num - first_packet_in_window) < 0) || ((ack_packet.packet_num - first_packet_in_window) > (GWS - 1))){
                    DEBUG_PRINT("ERROR: Index out of range -> recovery by ignoring\n   - packet_num: ");
                    DEBUG_PRINT_I(ack_packet.packet_num);
                    DEBUG_PRINT("   - index: ");
                    DEBUG_PRINT_I(ack_packet.packet_num - first_packet_in_window);
                    continue;
                }
                GF_WINDOW(ack_packet.packet_num - first_packet_in_window)->ack = ack_packet.acknowledge;
                DEBUG_PRINT("ACK saved\n   - packet: ");
                DEBUG_PRINT_I(ack_packet.packet_num);
                DEBUG_PRINT("   - ACK value: ");
                DEBUG_PRINT_I(ack_packet.acknowledge);
                while (GWS && ((GF_WINDOW(0)->ack == ACK) || (GF_WINDOW(0)->ack == DOUBLE_ACK))){
                    DEBUG_PRINT("Window shift\n");
                    free(pop_from_queue(window));
                    first_packet_in_window++;
                    DEBUG_PRINT("First packet in window: ");
                    DEBUG_PRINT_I(first_packet_in_window);
                }
                if(ack_packet.packet_num == 0 && ((ack_packet.acknowledge == ACK) || (ack_packet.acknowledge == DOUBLE_ACK))){
                    window_size = MAX_PACKET_NUM;
                }
                waiting_for_ack = false;
                continue;
            }
            //DEBUG_PRINT("SHA packet generated\n   - size: ");
            //DEBUG_PRINT_I(send_p->size);
            for(uint64_t i = 0; i < GWS; i++){
                gettimeofday(&tv,NULL);
                if (GF_WINDOW(i)->number_of_attempts == 0 || GF_WINDOW(i)->ack == FAULTY_CRC || (tv.tv_sec * 1e6 + tv.tv_usec - GF_WINDOW(i)->time_sent.tv_sec * 1e6 - GF_WINDOW(i)->time_sent.tv_usec) > 1e5){
                    if (GF_WINDOW(i)->number_of_attempts >= MAX_DELIVERY_ATTEMPTS){
                        EH(COMMUNICATION_ERROR);
                    }
                    send_packet(&(GF_WINDOW(i)->packet), GF_WINDOW(i)->size, sending_socket_desc, server_addr);
                    GF_WINDOW(i)->ack = NO_RESPONSE;
                    GF_WINDOW(i)->time_sent = tv;
                    GF_WINDOW(i)->number_of_attempts++;
                    DEBUG_PRINT("Sending packet n.:");
                    DEBUG_PRINT_I(GF_WINDOW(i)->packet.data_p.packet_num);
                    DEBUG_PRINT("   - length: ");
                    DEBUG_PRINT_I(GF_WINDOW(i)->size);
                    DEBUG_PRINT("   - attempt n.: ");
                    DEBUG_PRINT_I(GF_WINDOW(i)->number_of_attempts);
                    usleep(100);
                }
            }
        }
        waiting_for_ack = true;
        if(first_packet_in_window == (num_of_packets - 1)){
            DEBUG_PRINT("All data packets sent\n");
            if (SHA256_Final((send_p->packet.hash_p.sender_hash), &sha) != 1) {
                DEBUG_PRINT("ERROR finalizing SHA context!\n");
                EH(SHA_ERROR);
            }
            send_p->packet.hash_p.packet_num = num_of_packets - 1;
            send_p->size = sizeof(hash_packet);
            send_p->packet.hash_p.crc = crc32(send_p->packet.hash_p.crc, (const Bytef *) send_p + 4, sizeof(hash_packet) - 4);
            push_to_queue(window, (void*) send_p);
            DEBUG_PRINT("SHA packet generated\n   - size: ");
            DEBUG_PRINT_I(send_p->size);
            send_p = new_sending_packet();
        }
    }
    free(send_p);
    fclose(file);
    shutdown(sending_socket_desc, 2);
    shutdown(ack_socket_desc, 2);
    return OK;
}

errors send_packet(packet_t* data, int data_length, int sending_socket_desc, struct sockaddr* server_addr){
    if(sendto(sending_socket_desc, data, data_length, 0, server_addr, sizeof(*server_addr)) < 0){
        DEBUG_PRINT("Unable to send message\n");
        EH(COMMUNICATION_ERROR);
    }
    return OK;
}

sending_packet* new_sending_packet(){
    sending_packet* send_p = (sending_packet*) malloc(sizeof(sending_packet));
    send_p->packet.data_p.crc = crc32(0L, Z_NULL, 0);
    send_p->ack = NO_RESPONSE;
    send_p->number_of_attempts = 0;
    return send_p;
}


errors establish_connections(struct sockaddr_in* server_addr, struct sockaddr_in* my_addr, int* sending_socket_desc, int* ack_socket_desc){

    *sending_socket_desc = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    *ack_socket_desc = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if(*sending_socket_desc < 0 || *ack_socket_desc < 0){
        return SOCKET_CREATING_ERROR;
    }
    DEBUG_PRINT("Sending socket descriptor:");
    DEBUG_PRINT_I(*sending_socket_desc);
    DEBUG_PRINT("Ack socket descriptor:");
    DEBUG_PRINT_I(*ack_socket_desc);

    DEBUG_PRINT("Socket created successfully\n");
    server_addr->sin_family = AF_INET;
    server_addr->sin_port = htons(RECEIVER_PORT);
    server_addr->sin_addr.s_addr = inet_addr(RECIVER_ADDR);

    my_addr->sin_family = AF_INET;
    my_addr->sin_port = htons(ACK_PORT);
    my_addr->sin_addr.s_addr = htonl(INADDR_ANY);
    if (bind(*ack_socket_desc, (struct sockaddr*) my_addr, sizeof(struct sockaddr)) < 0){
        perror("bind");
        EH(BINDING_ERROR);
    }

    struct timeval read_timeout;
    read_timeout.tv_sec = 0;
    read_timeout.tv_usec = 10;
    int e = setsockopt(*ack_socket_desc, SOL_SOCKET, SO_RCVTIMEO, &read_timeout, sizeof read_timeout);
    DEBUG_PRINT("Set socket options: ");
    DEBUG_PRINT_I(e);
    return OK;
}

void error_handler(errors err_num, int line, char* file){
    fprintf(stderr, "Error n. %i on line %i int file: %s\n", err_num, line, file);
    exit(err_num);
}
