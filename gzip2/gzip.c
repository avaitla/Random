#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "contexts.h"

int create_outfile(global_context* gc)
{
    struct stat	ostat;
    int flags = O_WRONLY | O_CREAT | O_EXCL | O_BINARY;

    gc->ofd = open(gc->out_filepath, flags, RW_USER);
	if (gc->ofd == -1) { return -1; }

	if(stat(gc->out_filepath, &ostat) != 0)
    { close(gc->ofd); return -1; }

    return 0;
}

void treatfile(global_context* gc)
{
    if (stat(gc->in_filepath, &(gc->istat)) != 0)
    { printf("Could Not Stat File: %s\n", gc->in_filepath); return; }

    gc->ifile_size = (gc->istat).st_size;
    gc->bytes_to_read = gc->ifile_size;
    gc->ifd = open(gc->in_filepath, !gc->decompress ? O_RDONLY : O_RDONLY | O_BINARY, RW_USER);
    
    if (gc->ifd == -1) { printf("Could Not Open File: %s\n", gc->in_filepath); return; }
    //if (gc->decompress) { if (get_method(gc) < 0) { close(gc->ifd); return; } }
	if(create_outfile(gc) != 0) { close(gc->ifd); return; }

    printf("Starting Work\n");
    (*(gc->work))(gc);
    printf("Completeing Work\n");
    close(gc->ifd);
    close(gc->ofd);
}



global_context* init_global_context(unsigned long block_chunk_size, unsigned int number_of_threads)
{
    global_context* gc = (global_context*) malloc(sizeof(global_context));
    gc->ifd = 0; gc->ofd = 0; gc->in_filepath = NULL; gc->out_filepath = NULL;
    gc->decompress = 0; gc->ifile_size = 0LL; gc->bytes_in = 0LL; gc->bytes_out = 0LL;
    gc->header_bytes = 0; gc->block_chunk_size = block_chunk_size; 
    gc->number_of_threads = number_of_threads; gc->pool = NULL;
    gc->blocks_read = 0; gc->processed_blocks = NULL;
    gc->compr_level = 6; gc->crc = 0; gc->work = NULL;
    pthread_mutex_init(&(gc->output_block_lock), NULL);
    pthread_cond_init(&(gc->take_io_action), NULL);
    gc->block_number = 1; return gc;
}

int main(int argc, char **argv)
{
    if(argc < 2) { printf("First Argument must be the Filename"); return -1; }
 
    global_context* gc = init_global_context(210000000, 6);
    if(argc == 3)
    { if(strcmp(argv[2], "decompress") == 0) { gc->decompress = 1; }
      else { printf("Illegal Second Argument, Must be decompress\n"); free(gc); return -1; }
      printf("Decompression Not Supported! Use standard GZIP.\n"); return 0;    
    }

    char cwd[1024];
    if((char*)getcwd(cwd, sizeof(cwd)) == NULL)
    { printf("Illegal CWD Result\n"); free(gc); return -1; }

    char* in_filepath = (char*)malloc(strlen(cwd) + 1 + strlen(argv[1]) + 1);
    memcpy(in_filepath, cwd, (int)strlen(cwd)); in_filepath[strlen(cwd)] = '/';
    strcpy(in_filepath + strlen(cwd) + 1, argv[1]);
    gc->in_filepath = in_filepath;

    gc->work = zip;
    char* out_filepath = (char*)malloc(strlen(in_filepath) + 3);
    memcpy(out_filepath, in_filepath, strlen(in_filepath));
    memcpy(out_filepath + strlen(in_filepath), ".gz\0", 3);
    gc->out_filepath = out_filepath;
   
    printf("Input Path: %s\n", gc->in_filepath);
    printf("Output Path: %s\n", gc->out_filepath);
    
    /* Now we Have the Input Path and Output Path */
    treatfile(gc);
    return 0;
}

int abort_gzip() { exit(0); } /* Poor Man's Soln */
