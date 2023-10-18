#define mailbox_c

#include "mailbox.h"
#include <assert.h>

#define NO_MAILBOXES 30

static void *shared_memory = NULL;
static mailbox *freelist = NULL;  /* list of free mailboxes.  */


/*
 *  initialise the data structures of mailbox.  Assign prev to the
 *  mailbox prev field.
 */

static mailbox *mailbox_config (mailbox *mbox, mailbox *prev)
{
  mbox->prev = prev;
  mbox->item_available = multiprocessor_initSem (0);
  mbox->space_available = multiprocessor_initSem (1);
  mbox->mutex = multiprocessor_initSem (1);
  return mbox;
}


/*
 *  initSharedMemory - initialise the shared memory region once.
 *                     It also initialises all mailboxes and returns a
 *                     pointer to the start of shared memory which is
 *                     mem_size bytes in size.  Note that the shared
 *                     memory segment will also contain other data structures
 *                     before the pointer address.  The call to init_memory
 *                     will call multiprocessor_initSharedMemory to layout
 *                     all the shared memory contigeously using one shmat.
 */

void *mailbox_initSharedMemory (unsigned int mem_size)
{
  void *allocated;
  mailbox *mbox;
  mailbox *prev = NULL;
  int i;

  assert (shared_memory == NULL);
  _M2_multiprocessor_init ();
  shared_memory = multiprocessor_initSharedMemory
    (NO_MAILBOXES * sizeof (mailbox) + mem_size);
  mbox = shared_memory;
  for (i = 0; i < NO_MAILBOXES; i++)
    prev = mailbox_config (&mbox[i], prev);
  freelist = prev;
  allocated = &mbox[NO_MAILBOXES];
  return allocated;
}


/*
 *  init - create a single mailbox which can contain a single triple.
 */

mailbox *mailbox_init (void)
{
  mailbox *mbox;

  assert (shared_memory != NULL);
  if (freelist == NULL)
    {
      printf ("exhausted mailboxes\n");
      exit (1);
    }
  mbox = freelist;
  freelist = freelist->prev;
  return mbox;
}


/*
 *  kill - return the mailbox to the freelist.  No process must use this
 *         mailbox.
 */

mailbox *mailbox_kill (mailbox *mbox)
{
  mbox->prev = freelist;
  freelist = mbox;
  return NULL;
}


/*
 *  send - send (result, move_no, positions_explored) to the mailbox mbox.
 */

void mailbox_send (mailbox *mbox, int result, int move_no, int positions_explored)
{
  multiprocessor_wait (mbox->space_available);
  multiprocessor_wait (mbox->mutex);
  mbox->data.result = result;
  mbox->data.move_no = move_no;
  mbox->data.positions_explored = positions_explored;
  multiprocessor_signal (mbox->mutex);
  multiprocessor_signal (mbox->item_available);
}


/*
 *  rec - receive (result, move_no, positions_explored) from the
 *        mailbox mbox.
 */

void mailbox_rec (mailbox *mbox,
		  int *result, int *move_no, int *positions_explored)
{
  multiprocessor_wait (mbox->item_available);
  multiprocessor_wait (mbox->mutex);
  *result = mbox->data.result;
  *move_no = mbox->data.move_no;
  *positions_explored = mbox->data.positions_explored;
  multiprocessor_signal (mbox->mutex);
  multiprocessor_signal (mbox->space_available);
}
