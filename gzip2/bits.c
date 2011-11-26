/* bits.c -- output variable-length bit strings
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */


/*
 *  PURPOSE
 *
 *      Output variable-length bit strings. Compression can be done
 *      to a file or to memory. (The latter is not supported in this version.)
 *
 *  DISCUSSION
 *
 *      The PKZIP "deflate" file format interprets compressed file data
 *      as a sequence of bits.  Multi-bit strings in the file may cross
 *      byte boundaries without restriction.
 *
 *      The first bit of each byte is the low-order bit.
 *
 *      The routines in this file allow a variable-length bit value to
 *      be output right-to-left (useful for literal values). For
 *      left-to-right output (useful for code strings from the tree routines),
 *      the bits must have been reversed first with bi_reverse().
 *
 *      For in-memory compression, the compressed bit stream goes directly
 *      into the requested output buffer. The input data is read in blocks
 *      by the mem_read() function. The buffer is limited to 64K on 16 bit
 *      machines.
 *
 *  INTERFACE
 *
 *      void bi_init (FILE *zipfile)
 *          Initialize the bit string routines.
 *
 *      void send_bits (int value, int length)
 *          Write out a bit string, taking the source bits right to
 *          left.
 *
 *      int bi_reverse (int value, int length)
 *          Reverse the bits of a bit string, taking the source bits left to
 *          right and emitting them right to left.
 *
 *      void bi_windup (void)
 *          Write out any remaining bits in an incomplete byte.
 *
 *      void copy_block(char *buf, unsigned len, int header)
 *          Copy a stored block to the zip file, storing first the length and
 *          its one's complement if requested.
 *
 */

#include <stdio.h>
#include "contexts.h"
//#include "gzip.h"


int (*read_buf) (char *buf, unsigned size);

// Initialize the bit string routines.
void bi_init(thread_context* tc)
{
    tc->bi_buf = 0;
    tc->bi_valid = 0;
    //tc->bits_sent = 0L;
}

/* ===========================================================================
 * Send a value on a given number of bits.
 * IN assertion: length <= 16 and value fits in length bits.
 * value - value to send, length - number of bits
 */
#define Buf_size (8 * 2*sizeof(char))
/* Number of bits used within bi_buf. (bi_buf might be implemented on
 * more than 16 bits on some systems.)
 */
void send_bits(int value, int length, thread_context* tc)
{
    /* If not enough room in bi_buf, use (valid) bits from bi_buf and
     * (16 - bi_valid) bits from value, leaving (width - (16-bi_valid))
     * unused bits in value.
     */
    if (tc->bi_valid > (int)Buf_size - length) {
        tc->bi_buf |= (value << tc->bi_valid);
        put_short(tc->bi_buf);
        tc->bi_buf = (ush)value >> (Buf_size - tc->bi_valid);
        tc->bi_valid += length - Buf_size;
    } else {
        tc->bi_buf |= value << tc->bi_valid;
        tc->bi_valid += length;
    }
}

/* ===========================================================================
 * Reverse the first len bits of a code, using straightforward code (a faster
 * method would use a table)
 * IN assertion: 1 <= len <= 15
 * code - the value to invert, len - its bit length
 */
unsigned bi_reverse(unsigned code, int len)
{
    register unsigned res = 0;
    do {
        res |= code & 1;
        code >>= 1, res <<= 1;
    } while (--len > 0);
    return res >> 1;
}

/* ===========================================================================
 * Write out any remaining bits in an incomplete byte.
 */
void bi_windup(thread_context* tc)
{
    if (tc->bi_valid > 8) { put_short(tc->bi_buf); }
    else if (tc->bi_valid > 0) { put_byte(tc->bi_buf); }
    tc->bi_buf = 0; tc->bi_valid = 0;
}

/* ===========================================================================
 * Copy a stored block to the zip file, storing first the length and its
 * one's complement if requested.
 */
void copy_block(char* buf, unsigned len, int header, thread_context* tc)
{
    bi_windup(tc);              /* align on byte boundary */

    if (header)
    {
        put_short((ush)len);  
        put_short((ush)~len);
    }

    while (len--) { put_byte(*buf++); }
}
