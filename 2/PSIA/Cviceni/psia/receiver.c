#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <string.h>
#include <zlib.h>   // potreba pridat -lz pri kompilaci
#include "packet.h"
#include "debug_print.h"

#define LOCAL_PORT 5050
#define LOCAL_PORT_SEND 5055
#define SENDER_PORT_SEND 5051

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
    uint64_t r, expected_packet_num = 0, debug = 0;
    uint32_t crc;
    FILE *out;

    while (ack.acknowledge != ACK) {    // prvni cyklus pro ziskani entry paketu
        if ((r = recvfrom(socket_desc, &packet, sizeof(packet), 0, (struct sockaddr*)&sender_addr, &sender_struct_length)) < 0) {
            fprintf(stderr, "Could not receive entry packet!\n");
            return EXIT_FAILURE;
        }
        crc = crc32(0L, (const Bytef*)&packet + 4, r - 4);
        printf("%u %u\n", crc, packet.entry_p.crc);
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
            expected_packet_num++;
        }
        ack.packet_num = packet.entry_p.packet_num;
        ack.crc = crc32(0L, (const Bytef*)&ack + 4, sizeof(ack) - 4);
        sender_addr.sin_port = htons(SENDER_PORT_SEND);
        if (sendto(ack_socket_desc, &ack, sizeof(ack), 0, (struct sockaddr*)&sender_addr, sender_struct_length) < 0) {
            fprintf(stderr, "Unable to send ack/nack packet\n");
            return EXIT_FAILURE;
        }
    }

    SHA256_CTX sha;
    if (SHA256_Init(&sha) != 1) {
        fprintf(stderr, "ERROR initializing SHA context!\n");
        return EXIT_FAILURE;
    }
    uint64_t count = packet.entry_p.num_of_packets;
    
    while (expected_packet_num < count - 1) {    // cyklus pro ziskani data paketu
        if ((r = recvfrom(socket_desc, &packet, sizeof(packet), 0, (struct sockaddr*)&sender_addr, &sender_struct_length)) < 0) {
            fprintf(stderr, "Could not receive data packet!\n");
            return EXIT_FAILURE;
        }
        crc = crc32(0L, (const Bytef*)&packet + 4, r - 4);
        DEBUG_PRINT("Number of recieved characters: ");
        DEBUG_PRINT_I(r);
        printf("%u %u\n", crc, packet.data_p.crc);
        if (expected_packet_num != packet.data_p.packet_num) {
            ack.acknowledge = ACK;
            debug++;
            fprintf(stderr, "debug value DOUBLE_ACK: %lu\n", debug);
        } else if (crc != packet.data_p.crc) {
            ack.acknowledge = FAULTY_CRC;
            debug++;
            fprintf(stderr, "debug value FAULTY_CRC: %lu\n", debug);
        } else {
            ack.acknowledge = ACK;
            if (fwrite(packet.data_p.payload, sizeof(char), r - 3 * sizeof(uint32_t), out) < 0) {
                fprintf(stderr, "ERROR writing into file!\n");
                return EXIT_FAILURE;
            }
            if (SHA256_Update(&sha, packet.data_p.payload, r - 3 * sizeof(uint32_t)) != 1) {
                fprintf(stderr, "ERROR updating SHA context!\n");
                return EXIT_FAILURE;
            }
            expected_packet_num++;
        }
        ack.packet_num = packet.data_p.packet_num;
        ack.crc = crc32(0L, (const Bytef*)&ack + 4, sizeof(ack) - 4);
        sender_addr.sin_port = htons(SENDER_PORT_SEND);
        if (sendto(ack_socket_desc, &ack, sizeof(ack), 0, (struct sockaddr*)&sender_addr, sender_struct_length) < 0) {
            fprintf(stderr, "Unable to send ack/nack packet\n");
            return EXIT_FAILURE;
        }
    }

    unsigned char receiver_hash[SHA256_DIGEST_LENGTH];
    if (SHA256_Final(receiver_hash, &sha) != 1) {
        fprintf(stderr, "ERROR finalizing SHA context!\n");
        return EXIT_FAILURE;
    }

    while (expected_packet_num < count) {    // posledni cyklus pro porovnani hash
        if ((r = recvfrom(socket_desc, &packet, sizeof(packet), 0, (struct sockaddr*)&sender_addr, &sender_struct_length)) < 0) {
            fprintf(stderr, "Could not receive hash packet!\n");
            return EXIT_FAILURE;
        }
        crc = crc32(0L, (const Bytef*)&packet + 4, r - 4);
        printf("%u %u\n", crc, packet.hash_p.crc);
        if (expected_packet_num != packet.hash_p.packet_num) {
            ack.acknowledge = ACK;
            debug++;
            fprintf(stderr, "debug value DOUBLE_ACK: %lu\n", debug);
        } else if (crc != packet.hash_p.crc) {
            ack.acknowledge = FAULTY_CRC;
            debug++;
            fprintf(stderr, "debug value FAULTY_CRC: %lu\n", debug);
        } else if (memcmp(receiver_hash, packet.hash_p.sender_hash, SHA256_DIGEST_LENGTH) != 0) {
            fprintf(stderr, "ERROR: SHA hash mismatch!\n");
            ack.acknowledge = HASH_MISMATCH;
            expected_packet_num++;
        } else {
            fprintf(stderr, "SHA hash match!\n");
            ack.acknowledge = ACK;
            expected_packet_num++;
        }
        ack.packet_num = packet.hash_p.packet_num;
        ack.crc = crc32(0L, (const Bytef*)&ack + 4, sizeof(ack) - 4);
        sender_addr.sin_port = htons(SENDER_PORT_SEND);
        if (sendto(ack_socket_desc, &ack, sizeof(ack), 0, (struct sockaddr*)&sender_addr, sender_struct_length) < 0) {
            fprintf(stderr, "Unable to send ack/nack packet\n");
            return EXIT_FAILURE;
        }
    }

    fclose(out);
    shutdown(socket_desc, 2);
    return ack.acknowledge == ACK ? EXIT_SUCCESS : EXIT_FAILURE;
}
