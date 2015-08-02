#ifndef __COMMON__
#define __COMMON__

#define TRUE 1
#define FALSE 0

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))
#define abs(a) ((a)<0?-(a):(a))

#define unless(x) if(!(x))
#define until(x) while(!(x))
#define for_each(var, container) for( auto var=container.begin(); var != container.end(); var++ )

/* Returns true if x is a power of 2. */
#define POWEROF2(x) (((x)>0)&&(((x)&((x)-1))==0))

/* Copys string src into sting dst. */
#define COPY_STR(src,dst) while(*dst++=*src++);

/* Bit Utilities */
#define BITSIZE(x) (sizeof(x)*8)
#define BYTESIZE(x) sizeof(x)

/* Define int typedefs */
#include <stdint.h>

#endif
