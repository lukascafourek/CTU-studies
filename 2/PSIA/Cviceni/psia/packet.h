#ifndef __PACKET_H__

#define __PACKET_H__
#define MAX_PACKET_SIZE 1024UL
#define MAX_FILE_NAME 64
#define MAX_PAYLOAD_SIZE MAX_PACKET_SIZE - 3 * sizeof(uint32_t)
#define MAX_PACKET_NUM 5
#define MAX_DELIVERY_ATTEMPTS 20

#include <stdint.h>
#include <openssl/sha.h>    // potreba pridat -lcrypto pri kompilaci

typedef enum {
    ACK,
    NO_RESPONSE,
    DOUBLE_ACK,
    FAULTY_CRC = 100,
    HASH_MISMATCH
} ACK_TYPE;


typedef struct __attribute__((__packed__)) {
    uint32_t crc;
    uint64_t packet_num;
    char payload[MAX_PAYLOAD_SIZE];
} data_packet;

typedef struct __attribute__((__packed__)) {
    uint32_t crc;
    uint64_t packet_num;
    uint64_t num_of_packets;
    char file_name[MAX_FILE_NAME];
} entry_packet;

typedef struct __attribute__((__packed__)) {
    uint32_t crc;
    uint64_t packet_num;
    ACK_TYPE acknowledge;
} acknowledge_packet;

typedef struct __attribute__((__packed__)) {
    uint32_t crc;
    uint64_t packet_num;
    unsigned char sender_hash[SHA256_DIGEST_LENGTH];
} hash_packet;

typedef union __attribute__((__packed__)) {
    entry_packet entry_p;
    data_packet data_p;
    acknowledge_packet ack_p;
    hash_packet hash_p;
} packet_t;

#endif
