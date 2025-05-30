#ifndef __SENDER_H__
#define __SENDER_H__

#include <stdint.h>
#include <stdbool.h>
#include <sys/time.h>
#include "packet.h"

// #define RECIVER_ADDR "147.32.216.79"
#define RECIVER_ADDR "127.0.0.1"

#define NETDERPER

#ifdef NETDERPER
#define RECEIVER_PORT 14000
#define ACK_PORT 5052
#else
#define RECEIVER_PORT 5050
#define ACK_PORT 5051
#endif

typedef enum {
    OK,
    WRONG_INPUT = 100,
    COMMUNICATION_ERROR,
    SOCKET_CREATING_ERROR,
    FILE_NAME_TOO_LONG,
    FILE_ERROR,
    SHA_ERROR,
    BINDING_ERROR,
    QUEUE_ERROR
} errors;

typedef struct {
    packet_t packet;
    uint64_t size;
    struct timeval time_sent;    
    ACK_TYPE ack;
    uint32_t number_of_attempts;
} sending_packet;


#endif