#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <string.h>
#include <zlib.h>   // potreba pridat -lz pri kompilaci
#include "packet.h"
#include "debug_print.h"
#include "queue.h"

#define LOCAL_PORT 5050
#define LOCAL_PORT_SEND 5055
#define SENDER_PORT_SEND 5051

typedef struct {
    packet_t packet;
    uint64_t size;
} receiving_packet;

int main (int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "ERROR: need to be run with IP address as argument!\n");
        return EXIT_FAILURE;
    }
    struct sockaddr_in receiver_addr, sender_addr;
    socklen_t sender_struct_length = sizeof(sender_addr);

    memset(&receiver_addr, 0, sizeof(receiver_addr));
    int socket_desc = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    int ack_socket_desc = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (socket_desc < 0 || ack_socket_desc < 0) {
        fprintf(stderr, "ERROR creating socket!\n");
        return EXIT_FAILURE;
    }

    receiver_addr.sin_family = AF_INET;
    receiver_addr.sin_port = htons(LOCAL_PORT);
    receiver_addr.sin_addr.s_addr = inet_addr(argv[1]);

    if (bind(socket_desc, (struct sockaddr*)&receiver_addr, sizeof(receiver_addr)) < 0) {
        fprintf(stderr, "Could not bind to the port!\n");
        return EXIT_FAILURE;
    }
    receiver_addr.sin_port = htons(LOCAL_PORT_SEND);
    if (bind(ack_socket_desc, (struct sockaddr*)&receiver_addr, sizeof(receiver_addr)) < 0) {
        fprintf(stderr, "Could not bind to the ack port!\n");
        return EXIT_FAILURE;
    }
    fprintf(stderr, "Socket created successfully\n");

    memset(&sender_addr, 0, sizeof(sender_addr));
    packet_t packet;
    acknowledge_packet ack = {.acknowledge = FAULTY_CRC};
    uint64_t r, first_packet_in_window = 0, debug = 0;
    uint32_t crc;
    FILE *out;

    while (ack.acknowledge != ACK) {    // prvni cyklus pro ziskani entry paketu
        if ((r = recvfrom(socket_desc, &packet, sizeof(packet), 0, (struct sockaddr*)&sender_addr, &sender_struct_length)) < 0) {
            fprintf(stderr, "Could not receive entry packet!\n");
            return EXIT_FAILURE;
        }
        crc = crc32(0L, (const Bytef*)&packet + 4, r - 4);
        // printf("%u %u\n", crc, packet.entry_p.crc);
        if (crc != packet.entry_p.crc) {
            ack.acknowledge = FAULTY_CRC;
            debug++;
            fprintf(stderr, "debug value FAULTY_CRC: %lu\n", debug);
        } else {
            ack.acknowledge = ACK;
            fprintf(stderr, "Filename: %s\nNumber of packets: %lu\n", packet.entry_p.file_name, packet.entry_p.num_of_packets);
            out = fopen(packet.entry_p.file_name, "wb");
            if (out == NULL) {
                fprintf(stderr, "ERROR opening file!\n");
                return EXIT_FAILURE;
            }
            first_packet_in_window++;
        }
        ack.packet_num = packet.entry_p.packet_num;
        ack.crc = crc32(0L, (const Bytef*)&ack + 4, sizeof(ack) - 4);
        sender_addr.sin_port = htons(SENDER_PORT_SEND);
        if (sendto(ack_socket_desc, &ack, sizeof(ack), 0, (struct sockaddr*)&sender_addr, sender_struct_length) < 0) {
            fprintf(stderr, "Unable to send ack/nack packet\n");
            return EXIT_FAILURE;
        }
    }
    DEBUG_PRINT("Entry packet processing done\n");

    SHA256_CTX sha;
    if (SHA256_Init(&sha) != 1) {
        fprintf(stderr, "ERROR initializing SHA context!\n");
        return EXIT_FAILURE;
    }
    uint64_t num_of_packets = packet.entry_p.num_of_packets;
    receiving_packet* receive_p = (receiving_packet*)malloc(sizeof(receiving_packet));
    receiving_packet* saving_p;
    queue_t *window = create_queue(MAX_PACKET_NUM, true);
    for (int i = 0; i < MAX_PACKET_NUM; ++i) {
        push_to_queue(window, NULL);
    }
    
    while (first_packet_in_window < num_of_packets - 1) {    // cyklus pro ziskani data paketu
        if ((receive_p->size = recvfrom(socket_desc, &(receive_p->packet), sizeof(packet), 0, (struct sockaddr*)&sender_addr, &sender_struct_length)) < 0) {
            fprintf(stderr, "Could not receive data packet!\n");
            return EXIT_FAILURE;
        }
        crc = crc32(0L, (const Bytef*)&(receive_p->packet) + 4, receive_p->size - 4);
        DEBUG_PRINT("Packet received:\n   - Number of received characters: ");
        DEBUG_PRINT_I(receive_p->size);
        // printf("%u %u\n", crc, packet.data_p.crc);
        uint64_t idx = receive_p->packet.data_p.packet_num - first_packet_in_window;
        ack.packet_num = receive_p->packet.data_p.packet_num;
        if (crc != receive_p->packet.data_p.crc) {
            ack.acknowledge = FAULTY_CRC;
            debug++;
            fprintf(stderr, "   - debug value FAULTY_CRC: %lu\n", debug);
        } else if (first_packet_in_window > receive_p->packet.data_p.packet_num || (receiving_packet*)get_from_queue(window, idx) != NULL) {
            ack.acknowledge = DOUBLE_ACK;
            debug++;
            fprintf(stderr, "   - debug value DOUBLE_ACK: %lu\n", debug);
        } else {
            DEBUG_PRINT("   - CRC correct\n");
            DEBUG_PRINT("   - packet num: ");
            DEBUG_PRINT_I(receive_p->packet.data_p.packet_num);
            ack.acknowledge = ACK;
            // receive_p->packet = packet;
            // receive_p->size = r;
            if(!set_to_queue(window, idx, (void*)receive_p)){
                DEBUG_PRINT("ERROR setting to queue\n   - idx: ");
                DEBUG_PRINT_I(idx);
                DEBUG_PRINT("   - receive_p pointer: ");
                DEBUG_PRINT_P(receive_p);
                DEBUG_PRINT("   - queue size: ");
                DEBUG_PRINT_I(get_queue_size(window));
            }
            receive_p = (receiving_packet*)malloc(sizeof(receiving_packet));
            while (get_from_queue(window, 0) != NULL) {
                DEBUG_PRINT("Window shift\n");
                saving_p = (receiving_packet*)pop_from_queue(window);
                push_to_queue(window, NULL);
                if (fwrite(saving_p->packet.data_p.payload, sizeof(char), saving_p->size - 3 * sizeof(uint32_t), out) < 0) {
                    fprintf(stderr, "ERROR writing into file!\n");
                    return EXIT_FAILURE;
                }
                if (SHA256_Update(&sha, saving_p->packet.data_p.payload, saving_p->size - 3 * sizeof(uint32_t)) != 1) {
                    fprintf(stderr, "ERROR updating SHA context!\n");
                    return EXIT_FAILURE;
                }
                first_packet_in_window++;
                free(saving_p);
            }
            DEBUG_PRINT("First packet in window: ");
            DEBUG_PRINT_I(first_packet_in_window);
        }
        ack.crc = crc32(0L, (const Bytef*)&ack + 4, sizeof(ack) - 4);
        sender_addr.sin_port = htons(SENDER_PORT_SEND);
        if (sendto(ack_socket_desc, &ack, sizeof(ack), 0, (struct sockaddr*)&sender_addr, sender_struct_length) < 0) {
            fprintf(stderr, "Unable to send ack/nack packet\n");
            return EXIT_FAILURE;
        }
    }

    free(receive_p);
    delete_queue(window);
    fclose(out);
    unsigned char receiver_hash[SHA256_DIGEST_LENGTH];
    if (SHA256_Final(receiver_hash, &sha) != 1) {
        fprintf(stderr, "ERROR finalizing SHA context!\n");
        return EXIT_FAILURE;
    }

    while (first_packet_in_window < num_of_packets) {    // posledni cyklus pro porovnani hash
        if ((r = recvfrom(socket_desc, &packet, sizeof(packet), 0, (struct sockaddr*)&sender_addr, &sender_struct_length)) < 0) {
            fprintf(stderr, "Could not receive hash packet!\n");
            return EXIT_FAILURE;
        }
        DEBUG_PRINT("Packet received:\n   - Number of received characters: ");
        DEBUG_PRINT_I(r);
        crc = crc32(0L, (const Bytef*)&packet + 4, r - 4);
        // printf("%u %u\n", crc, packet.hash_p.crc);
        if (crc != packet.hash_p.crc) {
            ack.acknowledge = FAULTY_CRC;
            debug++;
            fprintf(stderr, "debug value FAULTY_CRC: %lu\n", debug);
        } else if (first_packet_in_window > packet.hash_p.packet_num) {
            ack.acknowledge = DOUBLE_ACK;
            debug++;
            fprintf(stderr, "debug value DOUBLE_ACK: %lu\n", debug);
        } else if (memcmp(receiver_hash, packet.hash_p.sender_hash, SHA256_DIGEST_LENGTH) != 0) {
            fprintf(stderr, "ERROR: SHA hash mismatch!\n");
            ack.acknowledge = HASH_MISMATCH;
            first_packet_in_window++;
        } else {
            DEBUG_PRINT("   - CRC correct\n");
            DEBUG_PRINT("   - packet num: ");
            fprintf(stderr, "SHA hash match!\n");
            ack.acknowledge = ACK;
            first_packet_in_window++;
        }
        ack.packet_num = packet.hash_p.packet_num;
        ack.crc = crc32(0L, (const Bytef*)&ack + 4, sizeof(ack) - 4);
        sender_addr.sin_port = htons(SENDER_PORT_SEND);
        if (sendto(ack_socket_desc, &ack, sizeof(ack), 0, (struct sockaddr*)&sender_addr, sender_struct_length) < 0) {
            fprintf(stderr, "Unable to send ack/nack packet\n");
            return EXIT_FAILURE;
        }
    }

    shutdown(socket_desc, 2);
    shutdown(ack_socket_desc, 2);
    return ack.acknowledge == ACK ? EXIT_SUCCESS : EXIT_FAILURE;
}
