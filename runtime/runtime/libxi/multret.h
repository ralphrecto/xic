#ifndef __MULTRET_H
#define __MULTRET_H

/*
 *   Support for returning a 2nd result in a register, and extracting that result.
 */

/*
   Set the 2nd result of this function to the value of 64-bit variable x
 */
#define SET2NDRESULT(x) asm("movq %0, %%rdx" :: "r" (x));

/*
   Put the 2nd result obtained from a function call into 64-bit variable x.
 */
#define GET2NDRESULT(x) asm("movq %%rdx, %0" : "+r" (x));

#endif
