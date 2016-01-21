#include "jove.h"
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <process.h>

#ifdef OS2
#define INCL_BASE
#include <os2.h>
unsigned long kbd_ram_sem;	/* define a RAM semaphore to */
 /* control kbd process */

#define SIGKBD SIGUSR1		/* kbd special signal */
struct _iobuf {
    char _FAR_ *_ptr;
    int _cnt;
    char _FAR_ *_base;
    char _flag;
    char _file;
};
typedef struct _iobuf FILE;
/* declare _iob[] array */

#ifndef _STDIO_DEFINED
#ifdef _DLL
extern FILE _FAR_ _cdecl _iob[];
#else
extern FILE _near _cdecl _iob[];
#endif
#endif


/* define file position type */

#ifndef _FPOS_T_DEFINED
typedef long fpos_t;
#define _FPOS_T_DEFINED
#endif

/* standard file pointers */

#define stdin  (&_iob[0])
#define stderr (&_iob[2])
#define stdaux (&_iob[3])
#define stdprn (&_iob[4])

int kbhit (void);
int getpid (void);
int read (int handle, void *buffer, unsigned int count);
int write (int handle, void *buffer, unsigned int count);

#endif				/* OS2 */

/* prototypes */
static SIGRESULT hold_read (int dummy);
static SIGRESULT strt_read (int dummy);
static void _fastcall kbd_reset (void);

#ifdef BSD_SIGS
#define pause()	sigpause(0L)
#endif

#define CR 13
#define LF 10
#define ESC 0x1B
#define CNTL_X 0x18
#define CNTL_U 0x15
#define CHAR_WAIT_TIME 500L
#define SHORT_INT      50L

struct header {
    int pid, nbytes;
    char buf[10];
};

#define HEADER_SIZE	(2 * sizeof (int))

/* JOVE sends SIGKBD whenever it wants the kbd process (this program)
   to stop competing for input from the keyboard.  JOVE does this when
   JOVE realizes that there are no more interactive processes running.
   The reason we go through all this trouble is that JOVE slows down
   a lot when it's getting its keyboard input via a pipe. */

/*************************************/
static SIGRESULT hold_read (int dummy)
/*************************************/
 /* DUMMY passed in when invoked by a signal; of no interest */
{
    kbd_reset ();
    signal (SIGKBD, strt_read);
/* set semaphore to high and then wait until next SIGKBD call */
    DosSemSet (&kbd_ram_sem);
    DosSemRequest (&kbd_ram_sem, -1L);
    SIGRETURN;
}

/**************************************/
static SIGRESULT strt_read (int dummy)
/**************************************/
{
	KBDKEYINFO KeyInfo;
	short		kbd_code = 0;
	unsigned char	buffer[10];
	int		n_read = 0, n_written;
	unsigned char	c, scan_code;
	struct header	header;
	unsigned short	wait_for_char;
	long		time_to_wait;
	HSEM		hSem;
	HTIMER		TimeHandle;


/* reintialize header */
    header.pid = getpid ();
    signal (SIGKBD, hold_read);
    kbd_reset ();
    header.nbytes = 0;
    while (1) {
	while ((kbhit() != 0) && kbd_code == 0) {
		kbd_code = KbdCharIn (&KeyInfo, IO_NOWAIT, 0);
		c = KeyInfo.chChar;
		scan_code = KeyInfo.chScan;
		if (c <= 0xFF && c != 0xE0) {
		      header.buf[n_read++] = c;
		  } else {
		      header.buf[n_read++] = 0xff;
		      header.buf[n_read++] = scan_code;
		}
/* wait extra time if first charater needs second to make sense */
	wait_for_char = ((n_read == 1) &&
			 (c == ESC || c == CNTL_X || c == CNTL_U));
	if (wait_for_char){
		time_to_wait = CHAR_WAIT_TIME;
		while (time_to_wait >= 0) {
			DosSleep (SHORT_INT);
			if (kbhit ()) break;
			time_to_wait -= SHORT_INT;
		}
	}

	}
	if (n_read != 0) {
		header.nbytes = n_read;
		DosWrite (1, (void *) &header, HEADER_SIZE + n_read, &n_written);
		n_read = 0;
	}
	DosSleep (50L);
	kbd_reset ();
	
    }
	SIGRETURN;
}

/*******************************/
int main (int argc, char **argv)
/*******************************/
{
    struct header header;
    int pid;
    int trans_mode;
    int fd;


//translation mode of stdin / out / err
    trans_mode = (setmode(0, O_BINARY) == O_BINARY);
    trans_mode = (setmode(1, O_BINARY) == O_BINARY);
    trans_mode = (setmode(2, O_BINARY) == O_BINARY);

    signal(SIGINT, SIG_IGN);
    pid = getpid();
    header.pid = pid;

    hold_read(0);
    return 0;
}

/*************************************/
static void _fastcall kbd_reset (void)
/*************************************/
/* reset keyboard before starting any kbd input */
{
    KBDINFO kbdInfo;

    kbdInfo.cb = 0x000A;
    KbdGetStatus(&kbdInfo, 0);
    kbdInfo.fsMask &= ~0x0001;	/* not echo on		*/
    kbdInfo.fsMask |= 0x0002;	/* echo off		*/
    kbdInfo.fsMask &= ~0x0008;	/* cooked mode off	*/
    kbdInfo.fsMask |= 0x0004;	/* raw mode		*/
    kbdInfo.fsMask &= ~0x0100;	/* shift report	off	*/
    //kbdInfo.fsMask = (KEYBOARD_ECHO_OFF | KEYBOARD_ASCII_MODE);
    KbdSetStatus(&kbdInfo, 0);
    return;
}
