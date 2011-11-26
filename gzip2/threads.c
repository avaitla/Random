#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include "threads.h"

/***********************************************************************/
/*      Generic Extensible Array                                       */
/***********************************************************************/

vector* init_vector(unsigned int num_elements, unsigned int element_size)
{
    vector* v = (vector*) malloc(sizeof(vector));
    v->elements = (void*) malloc(element_size * num_elements);
    v->total_elements = num_elements; v->occupied_elements = 0;
    v->element_size = element_size;
    return v;
}

void memcpy_safe(vector* vec, void* memory, int number_of_elements)
{
    unsigned int free_elements = vec->total_elements - vec->occupied_elements;
    if(number_of_elements > free_elements)
    {
        if(number_of_elements + vec->occupied_elements > 2 * vec->total_elements)
        {
            vec->elements = realloc(vec->elements, 2 * vec->element_size * (vec->occupied_elements + number_of_elements));
            vec->total_elements = 2 * (vec->occupied_elements + number_of_elements);
        }
        else
        {
            vec->elements = realloc(vec->elements, 2 * vec->element_size * vec->total_elements);
            vec->total_elements = 2 * vec->total_elements;
        }
    }

    memcpy(vec->elements + (vec->occupied_elements * vec->element_size), memory, vec->element_size * number_of_elements);
    vec->occupied_elements += number_of_elements;
}

int write_vector_to_fd(vector* vec, int fd)
{
    unsigned int written_bytes = 0;
    unsigned int total_bytes = vec->element_size * vec->total_elements; 
    unsigned int remaining_bytes = total_bytes;

    while(remaining_bytes != 0)
    {
        unsigned int n = write(fd, (char*) (vec->occupied_elements + written_bytes), remaining_bytes);
	    if (n == (unsigned)-1) exit(-1);
        remaining_bytes -= n; written_bytes += n;
	}
    
    return written_bytes;
}

void destroy_vector(vector* vec)
{
    free(vec->elements);
    free(vec); vec = NULL;
}





/***********************************************************************/
/*      Generic Queue Interface                                        */
/***********************************************************************/

// Queue Data Structure - http://en.wikipedia.org/wiki/Queue_(data_structure)

void enqueue(queue* q, void* object)
{
	queue_node* node = (queue_node*) malloc(sizeof(queue_node));
	node->object = object;
	if(q->first == NULL && q->last == NULL)
	{
		q->first = node;
		q->last = node;
	}

	else
	{
		q->last->next = node;
		q->last = node;
	}
	
	node->next = NULL;
}

// Recall that this will return a void pointer
// you must catch it, and free it manually
// the actual queue_node however is freed in this function
void* dequeue(queue* q)
{	
	void* pointer = NULL;
	if(q->first == NULL) pointer = (void*) NULL;
	pointer = q->first->object;
	queue_node* temp = q->first;
	if(q->first == q->last)
	{ q->first = NULL; q->last = NULL; }
	else{ q->first = (q->first)->next; }
	free(temp); temp = NULL;
	return pointer;	
}

queue* initialize_queue()
{
    queue* q = (queue *) malloc(sizeof(queue));
	q->first = NULL;
	q->last = NULL;
    return q;
}

int queue_empty(const queue* q)
{
	return (q->first == NULL);
}


queue* queue_join(queue* p1, queue* p2)
{
	queue* join = (queue*)malloc(sizeof(queue));
	join->first = p1->first;
	join->last = p2->last;
	p1->last->next = p2->first;
	return join;
}



/***********************************************************************/
/*      Sorted Linked List Interfaces                                  */
/***********************************************************************/

sorted_linked_list* init_sorted_linked_list()
{
    sorted_linked_list* lst = (sorted_linked_list*) malloc(sizeof(sorted_linked_list));
    if (lst == NULL) { fprintf(stderr, "Out of memory creating a new Sorted Linked List!\n"); return NULL; }
    lst->length = 0; lst->head = NULL; return lst;
}


void destroy_sorted_linked_list(sorted_linked_list* lst)
{
    sorted_linked_list_node* temp = NULL;
    sorted_linked_list_node* node = lst->head;
    if(node != NULL)
    {
        temp = node; node = node->next;
        free(temp->data); free(temp);
    }

    free(lst); lst = NULL;
}


void print_sorted_linked_list(sorted_linked_list* lst)
{
    printf("List Contains %d Elements\n-----------------------\n", lst->length);
    sorted_linked_list_node* cur = lst->head;
    if(lst->length != 0)
    {
        while(cur != NULL) { printf("%d -> ", cur->index); cur = cur->next;}
        printf("NULL\n");        
    }
}


// Insertion with equal indices always places the new item after the one already in list
void insert_into_sorted_linked_list(sorted_linked_list* list_to_insert_into, int index, void* data)
{
    sorted_linked_list_node* new_node = (sorted_linked_list_node*) malloc(sizeof(sorted_linked_list_node));
    new_node->index = index; new_node->data = data; new_node->next = NULL;

    list_to_insert_into->length += 1;
    if(list_to_insert_into->head == NULL)
    { list_to_insert_into->head = new_node; return; }

    sorted_linked_list_node* cur = list_to_insert_into->head;
    sorted_linked_list_node* prev = NULL;
    while(cur != NULL)
    {
        if(index < cur->index)
        {
            new_node->next = cur;
            if(prev != NULL) { prev->next = new_node; }
            else { list_to_insert_into->head = new_node; }
            return;
        }
        prev = cur; cur = cur->next;
    }

    prev->next = new_node;
}


void* remove_from_sorted_linked_list(sorted_linked_list* list, int index)
{
    sorted_linked_list_node* cur = list->head;
    sorted_linked_list_node* prev = NULL; void* data = NULL;
    while(cur != NULL)
    {
        if(cur->index == index)
        {
            list->length -= 1;
            data = cur->data;

            if(prev == NULL) { list->head = cur->next; }
            else { prev->next = cur->next; }
            free(cur); return data;
        }
        prev = cur;
        cur = cur->next;
    }
    return data;
}


void* pop_top(sorted_linked_list* list)
{ 
    if(list->head == NULL) return NULL;
    return remove_from_sorted_linked_list(list, (list->head)->index);
}


int peek_top(sorted_linked_list* list)
{
    if(list->head == NULL) return 0;
    else return list->head->index;
}

/***********************************************************************/
/*      Thread Pool Interfaces                                         */
/***********************************************************************/


// Slightly Modified to avoid indefinite locking from Jeanna Matthews Code:
// http://people.clarkson.edu/~jmatthew/cs644.archive/cs644.fa2001/proj/locksmith/code/ExampleTest/threadpool.c

_threadpool* create_threadpool(unsigned int num_threads_in_pool)
{
  _threadpool *pool;
  int i;

  if (num_threads_in_pool <= 0) return NULL;

  pool = (_threadpool *) malloc(sizeof(_threadpool));
  if (pool == NULL) { fprintf(stderr, "Out of memory creating a new threadpool!\n"); return NULL; }

  pool->threads = (pthread_t*) malloc (sizeof(pthread_t) * num_threads_in_pool);
  if(!pool->threads) { fprintf(stderr, "Out of memory creating a new threadpool!\n"); return NULL; }

  pool->num_threads = num_threads_in_pool;
  pool->qsize = 0;
  pool->qhead = NULL;
  pool->qtail = NULL;
  pool->shutdown = 0;
  pool->dont_accept = 0;

  //initialize mutex and condition variables.  
  if(pthread_mutex_init(&pool->qlock,NULL)) { fprintf(stderr, "Mutex initiation error!\n"); return NULL; }
  if(pthread_cond_init(&(pool->q_empty),NULL)) { fprintf(stderr, "CV initiation error!\n"); return NULL; }
  if(pthread_cond_init(&(pool->q_not_empty),NULL)) { fprintf(stderr, "CV initiation error!\n");	return NULL; }

  //make threads
  for (i = 0;i < num_threads_in_pool; i++)
  {   if(pthread_create(&(pool->threads[i]),NULL,do_work,pool))
      { fprintf(stderr, "Thread initiation error!\n"); return NULL; }
      printf("Thread %d Created.\n", i);
  }
  return pool;
}


void dispatch(_threadpool* from_me, void* (*dispatch_to_here) (void*), void *arg)
{
    _threadpool *pool = (_threadpool *) from_me;
    work_t * cur;
    int k; k = pool->qsize;

    //make a work queue element.
    cur = (work_t*) malloc(sizeof(work_t));
    if(cur == NULL) { fprintf(stderr, "Out of memory creating a work struct!\n"); return; }

    cur->routine = (void*) dispatch_to_here;
    cur->arg = arg; cur->next = NULL;

    pthread_mutex_lock(&(pool->qlock));

    // In case someone wants to queue more jobs
    if(pool->dont_accept)
    { free(cur); return; }

    if(pool->qsize == 0)
    {  
        pool->qhead = cur;  //set to only one
        pool->qtail = cur;
        pthread_cond_signal(&(pool->q_not_empty));  //I am not empty.
    }
    else 
    {
        pool->qtail->next = cur;	//add to end;
        pool->qtail = cur;			
	}
    pool->qsize++;
    pthread_mutex_unlock(&(pool->qlock));  //unlock the queue.
}


void destroy_threadpool(_threadpool* pool)
{
    void* nothing;
    int i = 0;

    pthread_mutex_lock(&(pool->qlock));
    pool->dont_accept = 1;
    while(pool->qsize != 0) { pthread_cond_wait(&(pool->q_empty),&(pool->qlock)); }

    pool->shutdown = 1;
    pthread_cond_broadcast(&(pool->q_not_empty));
    pthread_mutex_unlock(&(pool->qlock));

    for(;i < pool->num_threads;i++)
    {   
        pthread_cond_broadcast(&(pool->q_not_empty));
        pthread_join(pool->threads[i],&nothing);
    }

	free(pool->threads);
	pthread_mutex_destroy(&(pool->qlock));
	pthread_cond_destroy(&(pool->q_empty));
	pthread_cond_destroy(&(pool->q_not_empty));
	return;
}


/* This function is the work function of the thread */
void* do_work(void* p)
{
    _threadpool* pool = (_threadpool*) p;
    work_t* cur;	//The q element

    while(1)
    {
		pthread_mutex_lock(&(pool->qlock));  //get the q lock.
        while(pool->qsize == 0)  // If size is 0 Simply Wait
        {
		    if(pool->shutdown) { pthread_mutex_unlock(&(pool->qlock)); pthread_exit(NULL); }

            //wait until the condition says its not emtpy and give up the lock. 
            pthread_mutex_unlock(&(pool->qlock));  //get the qlock.

            pthread_cond_wait(&(pool->q_not_empty),&(pool->qlock));
            //check to see if in shutdown mode.
            if(pool->shutdown) { pthread_mutex_unlock(&(pool->qlock)); pthread_exit(NULL); }
        }

        cur = pool->qhead;	//set the cur variable.
        pool->qsize--;		//decriment the size.

        if(pool->qsize == 0) { pool->qhead = NULL; pool->qtail = NULL; }
        else { pool->qhead = cur->next; }

        //the q is empty again, now signal that its empty
		if(pool->qsize == 0 && ! pool->shutdown) { pthread_cond_signal(&(pool->q_empty)); }

        pthread_mutex_unlock(&(pool->qlock));

        // Custom Modification Not Included in Original Pthread Code
        if(pool->qsize > 0) { pthread_cond_signal(&(pool->q_not_empty)); }
        
        (cur->routine) (cur->arg);  //actually do work.
		free(cur);                  //free the work storage.  	
	}

    return NULL;
}


/***********************************************************************/
/*      IO Compression Thread Information (Gzip Specific)              */
/***********************************************************************/


/*typedef struct thread_compression_block
{
    unsigned char inbuf[INBUFSIZ + INBUF_EXTRA];
    unsigned char outbuf[OUTBUFSIZ + OUTBUF_EXTRA];
    unsigned short d_buf[DIST_BUFSIZE];
    unsigned char window [2L*WSIZE];
    unsigned short tab_prefix[1L << BITS];
    unsigned int chunk;
} thread_compression_block;*/


/*IOCompressThreadInfo* init_IOCompressThreadInfo(int input_fd, int output_fd)
{
    IOCompressThreadInfo* info = (IOCompressThreadInfo*) malloc(sizeof(IOCompressThreadInfo));
    if(info == NULL) { fprintf(stderr, "Out of memory creating a new Info Struct for Compression!\n"); return NULL; }

    info->processed_blocks = init_sorted_linked_list();
    if(info->processed_blocks == NULL) { free(info); return NULL; }

    info->next_block_to_write = 1;
    info->blocks_read = 0;

    //initialize mutex and condition variables.  
    if(pthread_mutex_init(&(info->output_block_lock),NULL)) { fprintf(stderr, "Mutex initiation error!\n"); return NULL; }
    if(pthread_mutex_init(&(info->input_block_lock), NULL)) { fprintf(stderr, "Mutex initiation error!\n"); return NULL; }
    if(pthread_cond_init(&(info->take_io_action),NULL)) { fprintf(stderr, "CV initiation error!\n"); return NULL; }

    info->input_fd = input_fd; info->output_fd = output_fd;
    return info;
}*/


/***********************************************************************/
/*      Main Sample Code Usage Samples                                 */
/***********************************************************************/


/*// Sample Code Demonstrating Sorted Linked List Usage
int main()
{
    sorted_linked_list* lst = init_sorted_linked_list();
    sorted_linked_list_node* node = pop_top(lst);
    remove_from_sorted_linked_list(lst, 423);    
    print_sorted_linked_list(lst);    
    insert_into_sorted_linked_list(lst, 43, NULL);
    print_sorted_linked_list(lst);
    insert_into_sorted_linked_list(lst, 25, NULL);
    insert_into_sorted_linked_list(lst, 3, NULL);
    insert_into_sorted_linked_list(lst, 55, NULL);
    remove_from_sorted_linked_list(lst, 55);
    pop_top(lst);
    print_sorted_linked_list(lst);
    destroy_sorted_linked_list(lst);
}

// Sample Code Demonstrating Pthread Usage
void* hello(void* _notused)
{
    printf("Hello From Thread %u\n", (unsigned int)pthread_self());
    sleep(15); return NULL;
}

int main()
{
    _threadpool* pool = create_threadpool(15); int i;
    for(i = 0; i != 50; i++) { dispatch(pool, hello, NULL); }
    destroy_threadpool(pool); return 0;
}*/
