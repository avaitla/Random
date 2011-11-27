#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "contexts.h"

#define HASH_BITS  14
#define HASH_SIZE (unsigned)(1<<HASH_BITS)
#define HASH_MASK (HASH_SIZE-1)
#define WMASK     (WSIZE-1)
/* HASH_SIZE and WSIZE must be powers of two */

#define NIL 0
/* Tail of hash chains */

#define FAST 4
#define SLOW 2
/* speed options for the general purpose bit flag */

#define TOO_FAR 4096
/* Matches of length 3 are discarded if their distance exceeds TOO_FAR */

#define WSIZE 0x8000     /* window size--must be a power of two, and */

#define EQUAL 0
/* result of memcmp for equal strings */

/* ===========================================================================
 * Local data used by the "longest match" routines.
 */


/* Use a faster search when the previous match is longer than this */


/* Values for max_lazy_match, good_match and max_chain_length, depending on
 * the desired pack level (0..9). The values given below have been tuned to
 * exclude worst case performance for pathological files. Better values may be
 * found for specific files.
 */

typedef struct config {
   ush good_length; /* reduce lazy search above this match length */
   ush max_lazy;    /* do not perform lazy search above this match length */
   ush nice_length; /* quit search above this match length */
   ush max_chain;
} config;

static config configuration_table[10] = {
/*      good lazy nice chain */
/* 0 */ {0,    0,  0,    0},  /* store only */
/* 1 */ {4,    4,  8,    4},  /* maximum speed, no lazy matches */
/* 2 */ {4,    5, 16,    8},
/* 3 */ {4,    6, 32,   32},

/* 4 */ {4,    4, 16,   16},  /* lazy matches */
/* 5 */ {8,   16, 32,   32},
/* 6 */ {8,   16, 128, 128},
/* 7 */ {8,   32, 128, 256},
/* 8 */ {32, 128, 258, 1024},
/* 9 */ {32, 258, 258, 4096}}; /* maximum compression */

/* Note: the deflate() code requires max_lazy >= MIN_MATCH and max_chain >= 4
 * For deflate_fast() (levels <= 3) good is ignored and lazy has a different
 * meaning.
 */

/* ===========================================================================
 * Update a hash value with the given input byte
 * IN  assertion: all calls to to UPDATE_HASH are made with consecutive
 *    input characters, so that a running hash key can be computed from the
 *    previous key instead of complete recalculation each time.
 */
#define UPDATE_HASH(h,c) (h = (((h)<<H_SHIFT) ^ (c)) & HASH_MASK)

/* ===========================================================================
 * Insert string s in the dictionary and set match_head to the previous head
 * of the hash chain (the most recent string with same hash key). Return
 * the previous length of the hash chain.
 * IN  assertion: all calls to to INSERT_STRING are made with consecutive
 *    input characters and the first MIN_MATCH bytes of s are valid
 *    (except for the last MIN_MATCH-1 bytes of the input file).
 */
#define INSERT_STRING(s, match_head) \
   (UPDATE_HASH(tc->ins_h, tc->window[(s) + MIN_MATCH-1]), \
    tc->prev[(s) & WMASK] = match_head = (tc->prev + WSIZE)[tc->ins_h], \
    (tc->prev + WSIZE)[tc->ins_h] = (s))

/* ===========================================================================
 * Initialize the "longest match" routines for a new file
 */
void lm_init (int pack_level, ush* flags, thread_context* tc)
{
    unsigned int j;

    if (pack_level < 1 || pack_level > 9) error("bad pack level");
    tc->compr_level = pack_level;


    /* Initialize the hash table. */
    for (j = 0;  j < HASH_SIZE; j++) (tc->prev + WSIZE)[j] = NIL;

    /* Set the default configuration parameters:
     */
    tc->max_lazy_match   = configuration_table[pack_level].max_lazy;
    tc->good_match       = configuration_table[pack_level].good_length;
    tc->nice_match       = configuration_table[pack_level].nice_length;
    tc->max_chain_length = configuration_table[pack_level].max_chain;
    if (pack_level == 1) {
       *flags |= FAST;
    } else if (pack_level == 9) {
       *flags |= SLOW;
    }
    /* ??? reduce max_chain_length for binary files */

    tc->strstart = 0;
    tc->block_start = 0L;

    tc->lookahead = thread_read_buf((char*)(tc->window), sizeof(int) <= 2 ? (unsigned)WSIZE : 2*WSIZE, tc);

    if(tc->lookahead == 0 || tc->lookahead == (unsigned)EOF) {
       tc->eofile = 1, tc->lookahead = 0;
       return;
    }

    tc->eofile = 0;
    /* Make sure that we always have enough lookahead. This is important
     * if input comes from a device such as a tty.
     */
    while (tc->lookahead < MIN_LOOKAHEAD && !(tc->eofile)) fill_window(tc);

    tc->ins_h = 0;
    for (j=0; j<MIN_MATCH-1; j++) UPDATE_HASH(tc->ins_h, tc->window[j]);
    /* If lookahead < MIN_MATCH, ins_h is garbage, but this is
     * not important since only literal bytes will be emitted.
     */
}


/* ===========================================================================
 * Fill the window when the lookahead becomes insufficient.
 * Updates strstart and lookahead, and sets eofile if end of input file.
 * IN assertion: lookahead < MIN_LOOKAHEAD && strstart + lookahead > 0
 * OUT assertions: at least one byte has been read, or eofile is set;
 *    file reads are performed for at least two bytes (required for the
 *    translate_eol option).
 */
void fill_window(thread_context* tc)
{
    register unsigned n, m;
    unsigned more = (unsigned)((tc->window_size) - (ulg)(tc->lookahead) - (ulg)(tc->strstart));
    /* Amount of free space at the end of the window. */

    /* If the window is almost full and there is insufficient lookahead,
     * move the upper half to the lower one to make room in the upper half.
     */
    if (more == (unsigned)EOF) {
        /* Very unlikely, but possible on 16 bit machine if strstart == 0
         * and lookahead == 1 (input done one byte at time)
         */
        more--;
    } else if (tc->strstart >= WSIZE+MAX_DIST) {
        /* By the IN assertion, the window is not empty so we can't confuse
         * more == 0 with more == 64K on a 16 bit machine.
         */
        Assert(tc->window_size == (ulg)2*WSIZE, "no sliding with BIG_MEM");

        memcpy((char*)(tc->window), (char*)(tc->window+WSIZE), (unsigned)WSIZE);
        tc->match_start -= WSIZE;
        tc->strstart    -= WSIZE; /* we now have strstart >= MAX_DIST: */

        tc->block_start -= (long) WSIZE;

        for (n = 0; n < HASH_SIZE; n++) {
            m = (tc->prev + WSIZE)[n];
            (tc->prev + WSIZE)[n] = (Pos)(m >= WSIZE ? m-WSIZE : NIL);
        }
        for (n = 0; n < WSIZE; n++) {
            m = tc->prev[n];
            tc->prev[n] = (Pos)(m >= WSIZE ? m-WSIZE : NIL);
            /* If n is not on any hash chain, prev[n] is garbage but
             * its value will never be used.
             */
        }
        more += WSIZE;
    }
    /* At this point, more >= 2 */
    if (!tc->eofile) {
        n = thread_read_buf((char*)(tc->window+tc->strstart+tc->lookahead), more, tc);
        if (n == 0 || n == (unsigned)EOF) {
            tc->eofile = 1;
        } else {
            tc->lookahead += n;
        }
    }
}


int longest_match(IPos cur_match, thread_context* tc)
{
    unsigned chain_length = tc->max_chain_length;       /* max hash chain length */
    register uch *scan = tc->window + tc->strstart;     /* current string */
    register uch *match;                                /* matched string */
    register int len;                                   /* length of current match */
    int best_len = tc->prev_length;                     /* best match length so far */
    IPos limit = tc->strstart > (IPos)MAX_DIST ? tc->strstart - (IPos)MAX_DIST : NIL;

    /* Stop when cur_match becomes <= limit. To simplify the code,
     * we prevent matches with the string of window index 0.
     */

/* The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
 * It is easy to get rid of this optimization if necessary.
 */
#if HASH_BITS < 8 || MAX_MATCH != 258
   error: Code too clever
#endif

    register uch *strend = tc->window + tc->strstart + MAX_MATCH - 1;
    register ush scan_start = *(ush*)scan;
    register ush scan_end   = *(ush*)(scan+best_len-1);

    /* Do not waste too much time if we already have a good match: */
    if (tc->prev_length >= tc->good_match) { chain_length >>= 2; }
    Assert(tc->strstart <= tc->window_size-MIN_LOOKAHEAD, "insufficient lookahead");

    do {
        Assert(cur_match < tc->strstart, "no future");
        match = tc->window + cur_match;

        /* Skip to next match if the match length cannot increase
         * or if the match length is less than 2:
         */

#if (MAX_MATCH == 258)
        /* This code assumes sizeof(unsigned short) == 2. Do not use
         * UNALIGNED_OK if your compiler uses a different size.
         */
        if (*(ush*)(match+best_len-1) != scan_end ||
            *(ush*)match != scan_start) continue;

        /* It is not necessary to compare scan[2] and match[2] since they are
         * always equal when the other bytes match, given that the hash keys
         * are equal and that HASH_BITS >= 8. Compare 2 bytes at a time at
         * strstart+3, +5, ... up to strstart+257. We check for insufficient
         * lookahead only every 4th comparison; the 128th check will be made
         * at strstart+257. If MAX_MATCH-2 is not a multiple of 8, it is
         * necessary to put more guard bytes at the end of the window, or
         * to check more often for insufficient lookahead.
         */
        scan++, match++;
        do {
        } while (*(ush*)(scan+=2) == *(ush*)(match+=2) &&
                 *(ush*)(scan+=2) == *(ush*)(match+=2) &&
                 *(ush*)(scan+=2) == *(ush*)(match+=2) &&
                 *(ush*)(scan+=2) == *(ush*)(match+=2) &&
                 scan < strend);
        /* The funny "do {}" generates better code on most compilers */

        /* Here, scan <= window+strstart+257 */
        Assert(scan <= tc->window+(unsigned)(tc->window_size-1), "wild scan");
        if (*scan == *match) scan++;

        len = (MAX_MATCH - 1) - (int)(strend-scan);
        scan = strend - (MAX_MATCH-1);

#else /* UNALIGNED_OK */

        if (match[best_len]   != scan_end  ||
            match[best_len-1] != scan_end1 ||
            *match            != *scan     ||
            *++match          != scan[1])      continue;

        /* The check at best_len-1 can be removed because it will be made
         * again later. (This heuristic is not always a win.)
         * It is not necessary to compare scan[2] and match[2] since they
         * are always equal when the other bytes match, given that
         * the hash keys are equal and that HASH_BITS >= 8.
         */
        scan += 2, match++;

        /* We check for insufficient lookahead only every 8th comparison;
         * the 256th check will be made at strstart+258.
         */
        do {
        } while (*++scan == *++match && *++scan == *++match &&
                 *++scan == *++match && *++scan == *++match &&
                 *++scan == *++match && *++scan == *++match &&
                 *++scan == *++match && *++scan == *++match &&
                 scan < strend);

        len = MAX_MATCH - (int)(strend - scan);
        scan = strend - MAX_MATCH;

#endif /* UNALIGNED_OK */

        if (len > best_len) {
            tc->match_start = cur_match;
            best_len = len;
            if (len >= tc->nice_match) break;
            scan_end = *(ush*)(scan+best_len-1);

        }
    } while ((cur_match = tc->prev[cur_match & WMASK]) > limit
	     && --chain_length != 0);

    return best_len;
}












#ifdef DEBUG
/* ===========================================================================
 * Check that the match at match_start is indeed a match.
 */
local void check_match(IPos start, IPos match, int length, thread_context* tc)
{
    /* check that the match is indeed a match */
    if (memcmp((char*)(tc->window + match),
                (char*)(tc->window + start), length) != EQUAL) {
        fprintf(stderr,
            " start %d, match %d, length %d\n",
            start, match, length);
        error("invalid match");
    }
    if (verbose > 1) {
        //fprintf(stderr,"\\[%d,%d]", start-match, length);
        //do { putc(window[start++], stderr); } while (--length != 0);
    }
}
#else
#  define check_match(start, match, length, tc)
#endif






#  define tab_prefix prev    /* hash link (see deflate.c) */
//#  define head (prev+WSIZE)  /* hash head (see deflate.c) */

/* ===========================================================================
 * Flush the current block, with given end-of-file flag.
 * IN assertion: strstart is set to the end of the current match.
 */
#define FLUSH_BLOCK(eof) \
   flush_block(tc->block_start >= 0L ? (char*)&(tc->window[(unsigned)(tc->block_start)]) : (char*)NULL, (long)(tc->strstart - tc->block_start), (eof), tc)


/* ===========================================================================
 * Same as above, but achieves better compression. We use a lazy
 * evaluation for matches: a match is finally adopted only if there is
 * no better match at the next window position.
 */
void* deflate_work(void* arg)
{
    thread_context* tc = (thread_context*)arg;
    IPos hash_head;          /* head of hash chain */
    IPos prev_match;         /* previous match */
    int flush;               /* set if current block must be flushed */
    int match_available = 0; /* set if previous match exists */
    register unsigned match_length = MIN_MATCH-1; /* length of best match */
#ifdef DEBUG
//    extern long isize;        /* byte length of input file, for debug only */
#endif

    //if (compr_level <= 3) return deflate_fast(); /* optimized for speed */

    /* Process the input block. */
    while (tc->lookahead != 0) {
        /* Insert the string window[strstart .. strstart+2] in the
         * dictionary, and set hash_head to the head of the hash chain:
         */
        INSERT_STRING(tc->strstart, hash_head);

        /* Find the longest match, discarding those <= prev_length.
         */
        tc->prev_length = match_length, prev_match = tc->match_start;
        match_length = MIN_MATCH-1;

        if (hash_head != NIL && tc->prev_length < tc->max_lazy_match &&
            tc->strstart - hash_head <= MAX_DIST) {
            /* To simplify the code, we prevent matches with the string
             * of window index 0 (in particular we have to avoid a match
             * of the string with itself at the start of the input file).
             */
            match_length = longest_match(hash_head, tc);
            /* longest_match() sets match_start */
            if (match_length > tc->lookahead) match_length = tc->lookahead;

            /* Ignore a length 3 match if it is too distant: */
            if (match_length == MIN_MATCH && tc->strstart-tc->match_start > TOO_FAR){
                /* If prev_match is also MIN_MATCH, match_start is garbage
                 * but we will ignore the current match anyway.
                 */
                match_length--;
            }
        }
        /* If there was a match at the previous step and the current
         * match is not better, output the previous match:
         */
        if (tc->prev_length >= MIN_MATCH && match_length <= tc->prev_length) {

            check_match(tc->strstart-1, prev_match, tc->prev_length, tc);

            flush = ct_tally(tc->strstart-1-prev_match, tc->prev_length - MIN_MATCH, tc);

            /* Insert in hash table all strings up to the end of the match.
             * strstart-1 and strstart are already inserted.
             */
            tc->lookahead -= tc->prev_length-1;
            tc->prev_length -= 2;
            do {
                tc->strstart++;
                INSERT_STRING(tc->strstart, hash_head);
                /* strstart never exceeds WSIZE-MAX_MATCH, so there are
                 * always MIN_MATCH bytes ahead. If lookahead < MIN_MATCH
                 * these bytes are garbage, but it does not matter since the
                 * next lookahead bytes will always be emitted as literals.
                 */
            } while (--tc->prev_length != 0);
            match_available = 0;
            match_length = MIN_MATCH-1;
            tc->strstart++;
            if (flush) { FLUSH_BLOCK(0), tc->block_start = tc->strstart; }

        } else if (match_available) {
            /* If there was no match at the previous position, output a
             * single literal. If there was a match but the current match
             * is longer, truncate the previous match to a single literal.
             */
            //Tracevv((stderr,"%c", tc->window[tc->strstart-1]));
            if (ct_tally (0, tc->window[tc->strstart-1], tc)) {
                FLUSH_BLOCK(0), tc->block_start = tc->strstart;
            }

            tc->strstart++;
            tc->lookahead--;
        } else {
            /* There is no previous match to compare with, wait for
             * the next step to decide.
             */
            match_available = 1;
            tc->strstart++;
            tc->lookahead--;
        }
        //Assert (tc->strstart <= isize && lookahead <= isize, "a bit too far");

        /* Make sure that we always have enough lookahead, except
         * at the end of the input file. We need MAX_MATCH bytes
         * for the next match, plus MIN_MATCH bytes to insert the
         * string following the next match.
         */
        while (tc->lookahead < MIN_LOOKAHEAD && !tc->eofile) fill_window(tc);
    }
    if(match_available) ct_tally (0, tc->window[tc->strstart-1]);

    if(tc->last_block) FLUSH_BLOCK(1); /* eof */
    else FLUSH_BLOCK(0);
    return NULL;
}







void thread_context_init(global_context* gc, thread_context* tc)
{
    bi_init(tc);
    ct_init(&(tc->attr), &(tc->method));
    lm_init(gc->level, &(tc->deflate_flags), tc);
}


thread_context* grab_another_block(global_context* gc, thread_context* tc)
{
    if(tc == NULL)
    {
        thread_context* tc = NULL;
        tc = (thread_context*) malloc(sizeof(thread_context));
        tc->full_input_buffer = (char*) malloc(gc->block_chunk_size);
        tc->full_output_buffer = (char*) malloc(gc->block_chunk_size);
    }

    thread_context_init(gc, tc);
    if(gc->bytes_to_read == 0)
        gc->last_block_number = gc->block_number == 0 ? 0 : gc->block_number - 1;

    tc->block_number = gc->block_number; 
    gc->block_number += 1;
    gc->blocks_read += 1;
    

    if(gc->bytes_to_read > gc->block_chunk_size)
    {
        tc->full_input_buffer_size = gc->block_chunk_size;
        tc->last_block = 0;
        file_read(tc->full_input_buffer, gc->block_chunk_size, gc);
        gc->bytes_in += gc->block_chunk_size;
        gc->bytes_to_read -= gc->block_chunk_size;
    }

    else
    {
        tc->full_input_buffer_size = gc->bytes_to_read;        
        tc->last_block = 1;
        file_read(tc->full_input_buffer, gc->bytes_to_read, gc);
        gc->bytes_in += gc->bytes_to_read;
        gc->last_block_number = gc->block_number - 1;
        gc->bytes_to_read = 0; 
    }

    return tc;
}


void* io_out_function(void* arg)
{
    global_context* gc = (global_context*) arg;
    queue* q = initialize_queue();

    while(1)
    {
        pthread_mutex_lock(&(gc->output_fd_lock));
        while(queue_empty(gc->output_queue))
            pthread_cond_wait(&(gc->more_io_output), &(gc->output_fd_lock));

        while(!queue_empty(gc->output_queue))
            enqueue(q, dequeue(gc->output_queue));

        pthread_mutex_unlock(&(gc->output_fd_lock));
        
        while(!queue_empty(q))
        {
            quick_data* data = (quick_data*) dequeue(q);

            unsigned int written_bytes = 0;
            unsigned int total_bytes = data->length; 
            unsigned int remaining_bytes = total_bytes;

            while(remaining_bytes != 0)
            {
                unsigned int n = write(gc->ofd, data->buffer + written_bytes, remaining_bytes);
	            if (n == (unsigned)-1) write_error();
                remaining_bytes -= n; written_bytes += n;
	        }

            free(data->buffer); free(data);
        }
    }
}


/* ===========================================================================
 * Same as above, but achieves better compression. We use a lazy
 * evaluation for matches: a match is finally adopted only if there is
 * no better match at the next window position.
 */
/* This is the main IO thread that will join back all items;*/
ulg deflate(global_context* gc)
{
    pthread_t io_out_thread;
    pthread_create(&io_out_thread, NULL, io_out_function, (void*)gc);

    int i; int first_pass = 0; int quit_flag = 0;
    for(i = 0; i != gc->number_of_threads; i++)
    {
        thread_context* tc = grab_another_block(gc, NULL);
        if(tc == NULL) { break; }
        dispatch(gc->pool, deflate_work, (void*)tc);
    }

    queue* temp = initialize_queue();
    while(1)
    {
        pthread_mutex_lock(&(gc->output_block_lock));                  
        while(!queue_empty(gc->thread_return_queue))
            enqueue(temp,dequeue(gc->thread_return_queue));
        pthread_mutex_unlock(&(gc->output_block_lock));

        if(!queue_empty(temp))
        {
            pthread_mutex_lock(&(gc->output_fd_lock));            
            while(!queue_empty(temp))
            {
                thread_context* tc = (thread_context*) dequeue(temp);
                quick_data* q = (quick_data*) malloc(sizeof(quick_data));
                q->buffer = (char*)malloc(tc->full_output_block_length);
                memcpy(q->buffer, tc->full_output_buffer, tc->full_output_block_length);
                q->length = tc->full_output_block_length;
                if(gc->bytes_to_read != 0)
                {                    
                    tc = grab_another_block(gc, tc);
                    dispatch(gc->pool, deflate_work, (void*)tc);
                }                    
                insert_into_sorted_linked_list(gc->processed_blocks, tc->block_number, (void*)q);
            }
            pthread_mutex_unlock(&(gc->output_fd_lock));
        }
            
        first_pass = 0;
        if(gc->processed_blocks->head != NULL)
        {
            while(gc->processed_blocks->head->index == gc->next_block_to_output)
            {
                if(gc->last_block_number == gc->processed_blocks->head->index) { quit_flag = 1; }
                if(first_pass == 0)
                {   pthread_mutex_lock(&(gc->output_block_lock));
                    first_pass = 1; }

                gc->next_block_to_output += 1;
                gc->blocks_read -= 1;
                void* block = (void*) pop_top(gc->processed_blocks);
                enqueue(gc->output_queue, block);
                if(gc->processed_blocks->head == NULL) { break; }
            }
        }
        
        if(first_pass == 1) { pthread_mutex_unlock(&(gc->output_block_lock)); pthread_cond_signal(&gc->more_io_output); }
        if(quit_flag == 1) break;
    }

    destroy_threadpool(gc->pool);
}
