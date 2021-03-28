#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/sysinfo.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <semaphore.h>


#if !defined(TRUE)
#  define TRUE (1==1)
#endif

#define MAX_SEMAPHORES  1000
static sem_t *sem_array;
static unsigned int sem_used;


/*
 *
 */

int multiprocessor_maxProcessors (void)
{
  return get_nprocs ();
}


int multiprocessor_fork (void)
{
  return fork ();
}


int multiprocessor_rand (void)
{
  int value = rand ();
  printf ("rand = %d\n", value);
  return value;
}


sem_t *multiprocessor_initSem (int value)
{
  if (sem_used == MAX_SEMAPHORES)
    {
      printf ("MAX_SEMAPHORES has been exceeded\n");
      exit (1);
    }
  sem_t *base_sem = &sem_array[sem_used];
  sem_used++;

  /* set up semaphore.  */
  int status = sem_init (base_sem, TRUE, value);
  return base_sem;
}


void multiprocessor_wait (sem_t *base_sem)
{
  sem_wait (base_sem);
}


void multiprocessor_signal (sem_t *base_sem)
{
  sem_post (base_sem);
}


void multiprocessor_killSem (sem_t *base_sem)
{
  shmdt (base_sem);
}


void *multiprocessor_initSharedMemory (unsigned int mem_size)
{
  void *allocated;
  key_t segid = ftok (".", 'R');
  int shmid = shmget (segid, mem_size + MAX_SEMAPHORES * sizeof (sem_t), IPC_CREAT | 0660);
  if (shmid < 0)
    {
      printf ("shmget failed\n");
      exit (1);
    }
  sem_array = (sem_t *) shmat (shmid, (void *)0, 0);
  allocated = &sem_array[MAX_SEMAPHORES];  /* start of the memory after the semaphores.  */
  sem_used = 0;
  return allocated;
}


/*
 *  adjust - while value < 0 wait ; value++
 *           while value > 0 signal ; value--
 */

void multiprocessor_adjust (sem_t *base_sem, int value)
{
  while (value < 0)
    {
      multiprocessor_wait (base_sem);
      value++;
    }
  while (value > 0)
    {
      multiprocessor_signal (base_sem);
      value--;
    }
}


/* constructor for the module.  */

void _M2_multiprocessor_init (void)
{
  srand (time (NULL));
}


/* deconstructor for the module.  */

void _M2_multiprocessor_finish (void)
{
  if (sem_array != NULL)
    shmdt (sem_array);
}
