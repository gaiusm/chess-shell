#include <sys/types.h>
#include <sys/wait.h>
#include <sys/sysinfo.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>


#define HALT_VALUE  (10000 + 2)


/*
 *  waitChild - waits for any child process to terminate.  It returns the
 *              pid of the child which has exited and also assigns result
 *              to the exit status of the child.
 */

int multiprocessor_waitChild (int *result)
{
  siginfo_t info;
  /*
   *   wait for any child to finish and find and return its exit
   *   (result) code.
   */

  if (waitid (P_ALL, 0, &info, WEXITED) == 0)
    {
      if (info.si_code == CLD_EXITED)
	{
	  *result = info.si_status;
	  printf ("retieved process status: %d\n", *result);
	  return info.si_pid;
	}
    }
  return HALT_VALUE;
}


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


/* constructor for the module.  */

void _M2_multiprocessor_init (void)
{
  srand (time (NULL));
}


/* deconstructor for the module.  */

void _M2_multiprocessor_finish (void)
{
}
