#ifndef __DEBUG_PRINT_H__
#define __DEBUG_PRINT_H__

#ifdef NDEBUG

#define DEBUG_PRINT(x) 
#define DEBUG_PRINT_I(x) 
#define DEBUG_PRINT_P(x) 

#else

#define DEBUG_PRINT(x) fprintf(stderr, x)
#define DEBUG_PRINT_I(x) fprintf(stderr, "%u\n", x)
#define DEBUG_PRINT_P(x) fprintf(stderr, "%p\n", x)

#endif

#endif