#include <ctype.h>
#include <sys/types.h>
#include <unistd.h>
#include <libgen.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "contexts.h"

int zip(global_context* gc)
{
    char *p = (char*) basename(gc->in_filepath);
    long length = strlen(p);
    long header_length = 4 + 4 + 1 + 1 + length + 1 + 4 + 4;
    char* headerbuffer = (char*)malloc(header_length);
    /* Write the header to the gzip file. See algorithm.doc for the format */
    headerbuffer[0] = GZIP_MAGIC[0];
    headerbuffer[1] = GZIP_MAGIC[1];
    headerbuffer[2] = DEFLATED;
    headerbuffer[3] = ORIG_NAME;

    unsigned int time_stamp = gc->istat.st_mtime;
    headerbuffer[4] = (char)(time_stamp);
    headerbuffer[5] = (char)(time_stamp >> 8);
    headerbuffer[6] = (char)(time_stamp >> 16);
    headerbuffer[7] = (char)(time_stamp >> 24);

    /* Write deflated file to zip file */
    gc->crc = updcrc(0, 0);
    gc->pool = create_threadpool(gc->number_of_threads);
    gc->thread_return_queue = initialize_queue();
    gc->output_queue = initialize_queue();
    gc->processed_blocks = init_sorted_linked_list();
    pthread_mutex_init(&(gc->output_block_lock), NULL);
    pthread_mutex_init(&(gc->output_fd_lock), NULL);
    pthread_cond_init(&(gc->take_io_action), NULL);
    pthread_cond_init(&(gc->more_io_output), NULL);






    headerbuffer[8] = 0;
    headerbuffer[9] = OS_CODE;

    int i = 10;
	do { headerbuffer[i] = *p; i++; } while (*p++);
    headerbuffer[i] = 0; i += 1;

    gc->header_bytes = (long)header_length;
    gc->bytes_out = header_length;
    write(gc->ofd, headerbuffer, header_length - 8);

    (void)deflate(gc);

    /* Write the crc and uncompressed size */
    headerbuffer[i] = (char)(gc->crc);
    headerbuffer[i + 1] = (char)(gc->crc >> 8);
    headerbuffer[i + 2] = (char)(gc->crc >> 16);
    headerbuffer[i + 3] = (char)(gc->crc >> 24);
    
    headerbuffer[i + 4] = (char)(gc->ifile_size);
    headerbuffer[i + 5] = (char)(gc->ifile_size >> 8);
    headerbuffer[i + 6] = (char)(gc->ifile_size >> 16);
    headerbuffer[i + 7] = (char)(gc->ifile_size >> 24);
    write(gc->ofd, headerbuffer + i, 8);
    return OK;
}

/* ===========================================================================
 * Read a new buffer from the current input file, perform end-of-line
 * translation, and update the crc and input file size.
 * IN assertion: size >= 2 (for end-of-line translation)
 */

int thread_read_buf(char *buf, unsigned long size, thread_context* tc)
{
    if(size < tc->full_input_buffer_remaining_bytes)
    { size = tc->full_input_buffer_remaining_bytes; }

    tc->full_input_buffer_remaining_bytes -= size;
    memcpy(buf, tc->full_input_buffer + tc->full_input_buffer_bytes_read, size);
    tc->full_input_buffer_bytes_read += (unsigned long long int)size;
    return (int)size;
}

int file_read(char *buf, unsigned long size, global_context* gc)
{
    unsigned len;
    unsigned long total_bytes_read = 0;
    unsigned long total_bytes_left = size;

    while(total_bytes_left != 0)
    {
        len = read(gc->ifd, buf + total_bytes_read, total_bytes_left);
        if (len == (unsigned)(-1)) error("read error");
        total_bytes_left -= len;
        total_bytes_read += len;
    }
        
    gc->crc = updcrc((uch*)buf, len);
    gc->bytes_in += (unsigned long long int)size;
    return (int)size;
}
