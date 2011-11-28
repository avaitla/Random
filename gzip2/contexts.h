#ifndef CONTEXTS
#define CONTEXTS
#include <sys/stat.h>
#include <pthread.h>
#include <stdlib.h>
#include "threads.h"

// bits.c

#define Buf_size (8 * 2*sizeof(char))
/* Number of bits used within bi_buf. (bi_buf might be implemented on
 * more than 16 bits on some systems.)
 */
 
 
 
 
// util.c

#define INBUFSIZ  0x8000  /* input buffer size */
#define OK      0
#define ERROR   1
#define WARNING 2

extern unsigned long crc_32_tab[];   /* crc table, defined below */





// Variables that Can be Made Global
typedef unsigned char  uch;
typedef unsigned short ush;
typedef unsigned long  ulg;

// Required Defines

#define EXTRA_FIELD 0x04
#define CONTINUATION 0x02
#define ORIG_NAME 0x08
#define COMMENT 0x10
#define RW_USER (S_IRUSR | S_IWUSR)
#define O_BINARY        0
#define	GZIP_MAGIC     "\037\213" /* Magic header for gzip files, 1F 8B */
#define DEFLATED    8
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define OK      0
#define OS_CODE 0


/* ===========================================================================
 * Constants
 */

#define MAX_BITS 15
/* All codes must not exceed MAX_BITS bits */

#define MAX_BL_BITS 7
/* Bit length codes must not exceed MAX_BL_BITS bits */

#define LENGTH_CODES 29
/* number of length codes, not counting the special END_BLOCK code */

#define LITERALS  256
/* number of literal bytes 0..255 */

#define END_BLOCK 256
/* end of block literal code */

#define L_CODES (LITERALS+1+LENGTH_CODES)
/* number of Literal or Length codes, including the END_BLOCK code */

#define D_CODES   30
/* number of distance codes */

#define BL_CODES  19
/* number of codes used to transfer the bit lengths */

#define STORED_BLOCK 0
#define STATIC_TREES 1
#define DYN_TREES    2
/* The three kinds of block type */

#define LIT_BUFSIZE  0x8000

/* Sizes of match buffers for literals/lengths and distances.  There are
 * 4 reasons for limiting LIT_BUFSIZE to 64K:
 *   - frequencies can be kept in 16 bit counters
 *   - if compression is not successful for the first block, all input data is
 *     still in the window so we can still emit a stored block even when input
 *     comes from standard input.  (This can also be done for all blocks if
 *     LIT_BUFSIZE is not greater than 32K.)
 *   - if compression is not successful for a file smaller than 64K, we can
 *     even emit a stored file instead of a stored block (saving 5 bytes).
 *   - creating new Huffman trees less frequently may not provide fast
 *     adaptation to changes in the input data statistics. (Take for
 *     example a binary file with poorly compressible code followed by
 *     a highly compressible string table.) Smaller buffer sizes give
 *     fast adaptation but have of course the overhead of transmitting trees
 *     more frequently.
 *   - I can't count above 4
 * The current code is general and allows DIST_BUFSIZE < LIT_BUFSIZE (to save
 * memory at the expense of compression). Some optimizations would be possible
 * if we rely on DIST_BUFSIZE == LIT_BUFSIZE.
 */

#define REP_3_6      16
/* repeat previous bit length 3-6 times (2 bits of repeat count) */

#define REPZ_3_10    17
/* repeat a zero length 3-10 times  (3 bits of repeat count) */

#define REPZ_11_138  18
/* repeat a zero length 11-138 times  (7 bits of repeat count) */

/* ===========================================================================
 * Local data
 */


#define Freq fc.freq
#define Code fc.code
#define Dad  dl.dad
#define Len  dl.len

#define HEAP_SIZE (2*L_CODES+1)
/* maximum heap size */


#define l_buf inbuf


/* Data structure describing a single value and its code string. */
typedef struct ct_data {
    union {
        ush  freq;       /* frequency count */
        ush  code;       /* bit string */
    } fc;
    union {
        ush  dad;        /* father node in Huffman tree */
        ush  len;        /* length of bit string */
    } dl;
} ct_data;

typedef struct tree_desc {
    ct_data *dyn_tree;      /* the dynamic tree */
    ct_data *static_tree;   /* corresponding static tree or NULL */
    int     *extra_bits;    /* extra bits for each code or NULL */
    int     extra_base;          /* base index for extra_bits */
    int     elems;               /* max number of elements in the tree */
    int     max_length;          /* max bit length for the codes */
    int     max_code;            /* largest code with non zero frequency */
} tree_desc;





typedef struct global_context
{

    int level;

    int  ifd;                               /* input file descriptor */
    int  ofd;                               /* output file descriptor */

    char* in_filepath;                      /* full path of input file */
    char* out_filepath;                     /* full path of output file */

    struct stat istat;                      /* status for input file */
    unsigned int decompress;                /* 1 if we are decompressing, 0 if compressing */
    unsigned long long int ifile_size;      /* Complete input file size in bytes */

    unsigned long long int bytes_to_read;   /* total number of bytes available to be read (effectively (ifile_size - bytes_in)) */
    unsigned long long int bytes_in;        /* total number of bytes read in from the input file */
    unsigned long long int bytes_out;       /* total number of bytes sent out to the output file */
    unsigned long header_bytes;             /* number of bytes in gzip header */

    unsigned int last_block_number;         /* the last block number to flush out, 0 if not computed yet */
    unsigned int block_number;              /* the next block read in will be this index */
    unsigned long block_chunk_size;         /* the size to chunk the input file by for each thread */ 
    unsigned int number_of_threads;         /* number of threads we want to use */
    unsigned int next_block_to_output;      /* next chunk that needs to be sent to the ofd */
    _threadpool* pool;                      /* pool of worker threads */

    unsigned int blocks_read;               /* number of input block reads currently with access to a thread
                                               or that hasn't been flushed out yet, if this number is greater
                                               than 2*number_of_threads, we stop reading in more information */

    pthread_mutex_t output_block_lock;      /* lock for threads to add their output buffers back to processed_blocks */
    queue* thread_return_queue;    
    pthread_cond_t take_io_action;          /* This condition variable will be signaled whenever a thread adds to the processed_blocks list */


    sorted_linked_list* processed_blocks;   /* blocks returned by threads (they should be accessed with the lock) */

    pthread_mutex_t output_fd_lock;
    queue* output_queue;
    pthread_cond_t more_io_output;


    int compr_level;                        /* compression level we are using */
    int (*work) (struct global_context*);   /* function to do the zipping or unzipping */

    unsigned long crc;                      /* current crc checksum for the bytes read in */

} global_context;

typedef struct quick_data
{
    unsigned int length;
    char* buffer;
} quick_data;






#define MIN_MATCH 3
#define MAX_MATCH 258

#define STORED 0
#define COMPRESSED 1


#define WSIZE 0x8000     /* window size--must be a power of two, and */
#define MIN_LOOKAHEAD (MAX_MATCH+MIN_MATCH+1)
#define MAX_DIST  (WSIZE-MIN_LOOKAHEAD)

#define UNKNOWN 0xffff
#define BINARY 0
#define ASCII 1

#define DIST_BUFSIZE 0x8000 /* buffer for distances, see trees.c */
#define H_SHIFT  ((HASH_BITS+MIN_MATCH-1)/MIN_MATCH)

# define tab_prefix prev /* hash link (see deflate.c) */
//# define head (prev+WSIZE) /* hash head (see deflate.c) */

typedef ush Pos;
typedef unsigned IPos;

typedef struct thread_context
{
    
    unsigned long bits_sent;

int attr;
int method;
ush deflate_flags;

char* full_output_buffer;
unsigned int full_output_block_length;
unsigned int block_number;
unsigned int full_input_buffer_size;

int last_block;

ush bl_count[MAX_BITS+1];


/* length code for each normalized match length (0 == MIN_MATCH) */


Pos prev[WSIZE];


    tree_desc l_desc;// = {dyn_ltree, static_ltree, extra_lbits, LITERALS+1, L_CODES, MAX_BITS, 0};

    tree_desc d_desc;// = {dyn_dtree, static_dtree, extra_dbits, 0, D_CODES, MAX_BITS, 0};

    tree_desc bl_desc;// = {bl_tree, (ct_data near *)0, extra_blbits, 0, BL_CODES, MAX_BL_BITS, 0};

    int compr_level;
    unsigned insize; /* valid bytes in inbuf */
    unsigned inptr;  /* index of next byte to be processed in inbuf */
    unsigned outcnt; /* bytes in output buffer */
    unsigned short bi_buf;
    unsigned int bi_valid;
    long block_start;
    unsigned ins_h;

    char* full_input_buffer;
    unsigned int full_input_buffer_bytes_read;
    unsigned int full_input_buffer_remaining_bytes;

    vector* output_vector;
    unsigned int bytes_in;
    unsigned int bytes_out; 

    uch* inbuf;     /* input buffer */
    uch* outbuf;    /* output buffer */
    ush* d_buf;     /* buffer for distances, see trees.c */
    uch* window;    /* Sliding window and suffix table (unlzw) */

    unsigned int prev_length;
    unsigned strstart;          /* window offset of current string */
    unsigned match_start;       /* window offset of current string */
    int           eofile;        /* flag set at end of input file */
    unsigned      lookahead;     /* number of valid bytes ahead in window */

    unsigned max_chain_length;
    unsigned int max_lazy_match;
    unsigned good_match;
    int nice_match;

    int* file_type;
    int* file_method;

    int compressed_len;
    int input_len;

    ct_data dyn_ltree[HEAP_SIZE];   /* literal and length tree */
    ct_data dyn_dtree[2*D_CODES+1]; /* distance tree */

    ct_data static_ltree[L_CODES+2];
    /* The static literal tree. Since the bit lengths are imposed, there is no
     * need for the L_CODES extra codes used during heap construction. However
     * The codes 286 and 287 are needed to build a canonical tree (see ct_init
     * below).
     */

    ct_data static_dtree[D_CODES];
    /* The static distance tree. (Actually a trivial tree since all codes use
     * 5 bits.)
     */

    ct_data bl_tree[2*BL_CODES+1];
    /* Huffman tree for the bit lengths */

//    tree_desc l_desc = {dyn_ltree, static_ltree, extra_lbits, LITERALS+1, L_CODES, MAX_BITS, 0};
//    tree_desc d_desc = {dyn_dtree, static_dtree, extra_dbits, 0, D_CODES, MAX_BITS, 0};
//    tree_desc bl_desc = {bl_tree, (ct_data near *)0, extra_blbits, 0, BL_CODES, MAX_BL_BITS, 0};

    /* number of codes at each bit length for an optimal tree */

//    uch bl_order[BL_CODES]    = {16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15};
    /* The lengths of the bit length codes are sent in order of decreasing
     * probability, to avoid transmitting the lengths for unused bit length codes.
     */

uch length_code[MAX_MATCH-MIN_MATCH+1];

    int heap[2*L_CODES+1]; /* heap used to build the Huffman trees */
    int heap_len;               /* number of elements in the heap */
    int heap_max;               /* element of largest frequency */
    /* The sons of heap[n] are heap[2*n] and heap[2*n+1]. heap[0] is not used.
     * The same heap array is used to build all trees.
     */

    uch depth[2*L_CODES+1];
    /* Depth of each subtree used as tie breaker for trees of equal frequency */

//    uch length_code[MAX_MATCH-MIN_MATCH+1];
    /* length code for each normalized match length (0 == MIN_MATCH) */

ulg window_size;//= (ulg)2*WSIZE;
/* window size, 2*WSIZE except for MMAP or BIG_MEM, where it is the
 * input file length plus MIN_LOOKAHEAD.
 */

    uch dist_code[512];
    /* distance codes. The first 256 values correspond to the distances
     * 3 .. 258, the last 256 values correspond to the top 8 bits of
     * the 15 bit distances.
     */

    int base_length[LENGTH_CODES];
    /* First normalized length for each code (0 = MIN_MATCH) */

    int base_dist[D_CODES];
    /* First normalized distance for each code (0 = distance of 1) */

    uch flag_buf[(LIT_BUFSIZE/8)];
    /* flag_buf is a bit array distinguishing literals from lengths in
     * l_buf, thus indicating the presence or absence of a distance.
     */

    unsigned last_lit;    /* running index in l_buf */
    unsigned last_dist;   /* running index in d_buf */
    unsigned last_flags;  /* running index in flag_buf */
    uch flags;            /* current flags not yet saved in flag_buf */
    uch flag_bit;         /* current bit used in flags */
    /* bits are filled in flags starting at bit 0 (least significant).
     * Note: these flags are overkill in the current code since we don't
     * take advantage of DIST_BUFSIZE == LIT_BUFSIZE.
     */

    ulg opt_len;        /* bit length of current block with optimal trees */
    ulg static_len;     /* bit length of current block with static trees */

} thread_context;

// Extern Declarations
// gzip.c

extern int create_outfile(global_context* gc);
extern void treatfile(global_context* gc);
extern int abort_gzip();

// zip.c

extern int zip(global_context* gc);
extern int file_read(char *buf, unsigned long size, global_context* gc);
extern int thread_read_buf(char *buf, unsigned long size, thread_context* tc);

// deflate.c

extern void lm_init (int pack_level, ush* flags, thread_context* tc);
extern int longest_match(unsigned cur_match, thread_context* tc);
extern void check_match(unsigned start, IPos match, int length, thread_context* tc);
extern void fill_window(thread_context* tc);
extern unsigned long deflate(global_context* gc);
extern void* deflate_work(void* tc);

// trees.c

extern void init_block     (thread_context* tc);
extern void pqdownheap     (ct_data *tree, int k, thread_context* tc);
extern void gen_bitlen     (tree_desc *desc, thread_context* tc);
extern void gen_codes      (ct_data *tree, int max_code, thread_context* tc);
extern void build_tree     (tree_desc *desc, thread_context* tc);
extern void scan_tree      (ct_data *tree, int max_code, thread_context* tc);
extern void send_tree      (ct_data *tree, int max_code, thread_context* tc);
extern int  build_bl_tree  (thread_context* tc);
extern void send_all_trees (int lcodes, int dcodes, int blcodes, thread_context* tc);
extern void compress_block (ct_data *ltree, ct_data *dtree, thread_context* tc);
extern void set_file_type  (thread_context* tc);

// bits.c

#define put_byte(c) {tc->outbuf[tc->outcnt++]=(uch)(c); if (tc->outcnt==OUTBUFSIZ)\
   flush_outbuf(tc);}
#define put_ubyte(c) {tc->window[tc->outcnt++]=(uch)(tc->c); if (tc->outcnt==WSIZE)\
   flush_window(tc);}

/* Output a 16 bit value, lsb first */
#define put_short(w) \
{ if (tc->outcnt < OUTBUFSIZ-2) { \
    tc->outbuf[tc->outcnt++] = (uch) ((w) & 0xff); \
    tc->outbuf[tc->outcnt++] = (uch) ((ush)(w) >> 8); \
  } else { \
    put_byte((uch)((w) & 0xff)); \
    put_byte((uch)((ush)(w) >> 8)); \
  } \
}

/* Output a 32 bit value to the bit stream, lsb first */
#define put_long(n) { \
    put_short((n) & 0xffff); \
    put_short(((ulg)(n)) >> 16); \
}

#define get_char() get_byte()
#define put_char(c) put_byte(c)

#define NO_FILE  (-1)   /* in memory compression */
#define OUTBUFSIZ  16384  /* output buffer size */

#define verbose 0
#define seekable() 0
#define Trace(x) {if(verbose) fprintf x ;}  
#define Tracev(x) {if (verbose) fprintf x ;}
#define Tracevv(x) {if (verbose) fprintf x; }
#define Assert(cond,msg) {if(!(cond)) error(msg);}

void bi_init(thread_context* tc);
void send_bits(int value, int length, thread_context* tc);
unsigned bi_reverse(unsigned code, int len);
void bi_windup(thread_context* tc);
void copy_block(char* buf, unsigned len, int header, thread_context* tc);


// util.c


void error(char *m);
void warn(char *a, char *b);
void read_error();
void write_error();
void write_buf(void* buf, unsigned cnt, thread_context* tc);
ulg updcrc(uch *s, unsigned n);
void clear_bufs(thread_context* tc);
int fill_inbuf(int eof_ok, thread_context* tc);
void flush_outbuf(thread_context* tc);
void flush_window(thread_context* tc);
char *strlwr(char* s);
void* xmalloc(unsigned size);






#endif
