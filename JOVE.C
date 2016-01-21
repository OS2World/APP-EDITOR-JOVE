/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Contains the main loop initializations, and some system- dependent
   type things, e.g. putting terminal in CBREAK mode, etc. */

#ifdef OS2
# define INCL_BASE
# include <os2.h>
#endif
#include "jove.h"
#include "fp.h"
#include "termcap.h"
#include "ctype.h"
#include "chars.h"
#include "disp.h"
#include "re.h"	/* for find_tag() */
#include "rec.h"
#if (defined IPROCS || defined OS2IPROCS)
#  ifdef OS2
#    include <stdio.h>
#  endif /* OS2 */
# include "iproc.h"
#endif

#ifdef MAC
# include "mac.h"
#else
# ifdef	STDARGS
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
# include <sys/stat.h>
#endif

#include <signal.h>
#include <errno.h>

#ifdef UNIX
# ifndef SYSV
#  include <sgtty.h>
# else
#  include <termio.h>
# endif /* SYSV */
#endif /* UNIX */

#ifdef MSDOS
# include <process.h>
#endif /* MSDOS */

#ifndef MAC
# include <fcntl.h>
#endif

#ifdef MSDOS
static	void	break_off proto((void)),
		break_rst proto((void));
#endif
static void _fastcall kbd_reset(void);
static void
	_fastcall DoKeys proto((int firsttime)),
	_fastcall UNIX_cmdline proto((int argc,char * *argv));

#ifdef MSDOS
extern
#else
static
#endif
void
	_fastcall UnsetTerm proto((char *)),
	do_sgtty proto((void));

/* Various tty state structures.
 * Each is an array, subscripted by one of "OFF" or "ON".
 */

#ifndef MAC
#include "ttystate.h"
#endif

#ifdef UNIX
# ifdef TIOCSLTC
struct ltchars	ls[2];
# endif /* TIOCSLTC */

# if defined(TIOCGETC) && !defined(SYSV)
struct tchars	tc[2];
# endif

# ifdef PASS8			/* use pass8 instead of raw for meta-key */
static int	lmword[2];		/* local mode word */
# endif

# ifdef BRLUNIX
struct sg_brl	sg[2];
# else
#  ifdef SYSV
struct termio	sg[2];
#  else /* SYSV */
struct sgttyb	sg[2];
#  endif /* SYSV */
# endif /* BRLUNIX */

# ifdef BIFF
static struct stat	tt_stat;	/* for biff */
#  ifndef BSD4_2
static char	*tt_name = 0;		/* name of the control tty */
extern char	*ttyname();		/* for systems w/o fchmod ... */
#  endif
static int	dw_biff = NO;		/* whether or not to fotz at all */
# endif /* BIFF */
#endif /* UNIX */

int	errormsg;
char	NullStr[] = "";
jmp_buf	mainjmp;


#ifdef MSDOS
# define SIGHUP	99
#endif /* MSDOS */

/* finish() does not return, so it is funny that it returns a non-void
 * result.  This is because most systems claim that signal(2) deals
 * with functions of type int ().  ANSI changes this: the function
 * type must be void (int).  This bridge must soon be crossed.
 */
/*****************************/
SIGRESULT  finish (int code)
/*****************************/
{
	void _fastcall screen_buffer_flush(void);
	int _fastcall kbd_char(KBD_REQUEST request, int *UpdModLine);
	int	CoreDump = (code != 0 && code != SIGHUP),
		DelTmps = 1;		/* Usually we delete them. */

	if (code == SIGINT) {
		char	c;
#if (defined(IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
		int	started;
#endif
#ifndef MENLO_JCL
		signal (code, finish);
#endif
		f_mess("Abort (Type 'n' if you're not sure)? ");
#if (!defined MSDOS) && (!defined OS2IPROCS)
# if (defined(IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
		started = kbd_stop();
# endif
#ifdef SYSV
		if (read(0, &c, (size_t) 1) != 1)
#endif
			(void) read(0, &c, (size_t) 1);
# if (defined (IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
		if (started)
			(void) kbd_strt();
# endif
#else /* MSDOS */
		c = getrawinchar();
#endif /* MSDOS */
		message(NullStr);
		if ((c & 0377) != 'y') {
			redisplay();
			SIGRETURN;
		}
	}
	DisabledRedisplay = YES;
#ifndef MAC
	UnsetTerm(NullStr);
#endif
#if (defined(IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
	kbd_kill();		/* kill the keyboard process */
#endif
#ifndef MSDOS
	if (code != 0) {
		if (!Crashing) {
			Crashing = YES;
			lsave();
			SyncRec();
			writef("JOVE CRASH!! (code %d)\n", code);
			if (ModBufs(1)) {
				writef("Your buffers have been saved.\n");
				writef("Use \"jove -r\" to have a look at them.\n");
				DelTmps = 0;	/* Don't delete anymore. */
			} else
				writef("You didn't lose any work.\n");
		} else
			writef("\r\nYou may have lost your work!\n");
	}
#endif /* MSDOS */
	flusho();
	screen_buffer_flush();
	if (DelTmps) {
#if defined (IPROCS) && !defined(PIPEPROCS)
		(void) signal(SIGCHLD, SIG_IGN);
#endif
		tmpclose();
#ifndef MSDOS
		recclose();
#endif /* MSDOS */
	}
#ifdef UNIX
	if (CoreDump)
		abort();
#ifdef PROFILING
	exit(0);
#else
	_exit(0);
#endif
#else /* MSDOS or MAC*/
#ifdef MSDOS
	break_rst();	/* restore previous ctrl-c handling */
#endif
	exit(0);
#endif /* UNIX */
	/*NOTREACHED*/
}

static char	smbuf[20],
		*bp = smbuf;
static int	nchars = 0;

static char	peekbuf[10],
		*peekp = peekbuf;

#if defined(SYSV) || defined(M_XENIX)
/***********************************************/
void setblock( register int fd, register int on)	/* turn blocking on or off */
/***********************************************/
{
    static int blockf, nonblockf;
    static int first = 1;
    int flags;

    if (first) {
	first = 0;
	if ((flags = fcntl(fd, F_GETFL, 0)) == -1)
	    finish(SIGHUP);
	blockf = flags & ~O_NDELAY;	/* make sure O_NDELAY is off */
	nonblockf = flags | O_NDELAY;	/* make sure O_NDELAY is on */
    }
    if (fcntl(fd, F_SETFL, on ? blockf : nonblockf) == -1)
	finish(SIGHUP);
}
#endif /* SYSV */

/*******************************/
static int _fastcall Peekc(void)
/*******************************/
{
	int	c;

	if (peekp == peekbuf)
		c = EOF;
	else
		c = *--peekp & 0377;
	return c;
}
/****************************/
void _fastcall Ungetc(int c)
/****************************/
{
	if (peekp == &peekbuf[(sizeof peekbuf) - 1])
		return;		/* Sorry, can't oblige you ... */
	*peekp++ = c;
}

int	InputPending = 0;

char	*Inputp = 0;

#if (defined(IPROCS) && !defined(PIPEPROCS))	/* that is, if ptys */
/*********************************/
int _fastcall jgetchar(void)
/*********************************/
{
	long		reads;
	register  int	tmp,
			nfds;
	int		c;

	if (nchars <= 0) {
		/* Get a character from the keyboard, first checking for
		   any input from a process.  Handle that first, and then
		   deal with the terminal input. */
		do {
			do {
				reads = global_fd;
				nfds = select(32, &reads, (long *) 0, (long *) 0, (struct timeval *) 0);
			} while (nfds < 0 && errno == EINTR);

			if (nfds == -1)
				complain("\rerror %d in select %ld", errno, global_fd);
			else {
				if (reads & 01) {
					nchars = read(0, smbuf, sizeof(smbuf));
					reads &= ~01;
					nfds -= 1;
				}
				while (nfds--) {
					tmp = ffs(reads) - 1;
					read_proc(tmp);
					reads &= ~(1L << tmp);
				}
			}
		} while (nchars <= 0);

		if (nchars <= 0)
			finish(SIGHUP);

		bp = smbuf;
		InputPending = (nchars > 1);
	}

	if (((c = *bp) & 0200) && MetaKey != 0) {
		*bp = (c & CHARMASK);
		return '\033';
	}
	nchars -= 1;
	return *bp++ & 0377;
}

#else
/******************************/
int _fastcall jgetchar (void)
/******************************/
{
	register  int	c;
	int _fastcall kbd_char( KBD_REQUEST request, int *UpdModLine );
	void _fastcall read_proc( int pid, register int nbytes );

	struct header {
		int	pid;
		int	nbytes;
	} header;
	int	n;
	int _fastcall f_readn( File *fp, char *addr, int n );
normal:
	if (nchars <= 0) {
		bp = smbuf;
#if (defined (MSDOS) && !defined (OS2IPROCS))
		*bp = getrawinchar();
		nchars = 1;
#else
# if defined (IPROCS) || defined (OS2IPROCS)
		if (NumProcs == 0) {
# endif
			do
#if !defined (OS2IPROCS)
				nchars = read(0, smbuf, sizeof smbuf);
#  else
			{
			  *bp = getrawinchar();
			   nchars = 1;
			}
#endif /* OS2IPROCS */
# ifdef SYSV
			while (nchars == 0 || (nchars < 0 && errno == EINTR));
			if (nchars < 0)
# else
			while ( nchars < 0 && errno == EINTR );
			if( nchars <= 0 )
# endif /* SYSV */
				finish(SIGHUP);
# if defined (IPROCS) || defined (OS2IPROCS)
		} else for (;;) {
			n = f_readn( ProcInput, (char *) &header,
				    sizeof( header ) );
			if (n == EOF) {
				printf("\rError reading kbd process.\n");
				finish(1);
			}
			/* data is from the keyboard process */
			if (header.pid == kbd_pid) {
				nchars = f_readn(ProcInput, smbuf, header.nbytes);
				if (nchars != header.nbytes) {
					printf("\rError reading kbd process.");
					finish(1);
				} else
					break;
			} else
				read_proc (header.pid, header.nbytes);
			if (NumProcs == 0) {
				kbd_reset ();
				kbd_stop ();
				goto normal;
			}
		}
# endif /* IPROCS */
#endif /* MSDOS */

	}
	if (((c = *bp) & 0200) && MetaKey != 0) {
		*bp = (c & CHARMASK);
		return '\033';
	}
	nchars -= 1;
	return (*bp++ & CHARMASK);
}

#endif /* IPROCS */

/* Returns non-zero if a character waiting */
/****************************/
int _fastcall charp (void)
/****************************/
{
	int	some = 0;
	int _fastcall kbd_char(KBD_REQUEST request, int *UpdModLine);
	if (InJoverc != 0 || nchars > 0 || Inputp != 0)
		return 1;
#ifdef BRLUNIX
	{
		static struct sg_brl gttyBuf;

		gtty(0, (char *) &gttyBuf);
		if (gttyBuf.sg_xflags & INWAIT)
			some += 1;
	}
#endif
#ifdef FIONREAD
	{
		long c;

		if (ioctl(0, FIONREAD, (UnivPtr) &c) == -1)
			c = 0;
		some = (c > 0);
	}
#endif /* FIONREAD */
#if defined(SYSV) || defined(M_XENIX)
	setblock(0, 0);		/* turn blocking off */
	nchars = read(0, smbuf, sizeof smbuf);	/* Is anything there? */
	setblock(0, 1);		/* turn blocking on */
	if (nchars > 0)		/* something was there */
	    bp = smbuf;		/* make sure bp points to it */
	some = (nchars > 0);	/* just say we found something */
#endif /* SYSV */
#ifdef c70
	some = !empty(0);
#endif
#ifdef MSDOS
	some = rawkey_ready();
#endif
#ifdef MAC
	some = rawchkc();
#endif
	return some;
}

#ifdef BIFF
static void	biff_init proto((void));
#endif

#ifdef TERMCAP

/****************************/
static void ResetTerm(void)
/****************************/
{
	do_sgtty();		/* this is so if you change baudrate or stuff
				   like that, JOVE will notice. */
	ttyset(ON);
	putpad(TI, 1);
	putpad(VS, 1);
	putpad(KS, 1);
#ifdef UNIX
	(void) chkmail(YES);	/* force it to check to we can be accurate */
#endif
#ifdef BIFF
	if (BiffChk != dw_biff)
		biff_init();
	/* just in case we changed our minds about whether to deal with
	   biff */
#endif
}
/*******************************************/
static void _fastcall UnsetTerm(char *mesg)
/*******************************************/
{
	ttyset(OFF);
#ifdef ID_CHAR
	INSmode(0);
#endif
	putpad(KE, 1);
	putpad(VE, 1);

	/* Place the cursor at the bottom of the screen.  If TE then we
	   might have an alternate page, in which case we return to that
	   alternate page and write the message. */
	Placur(ILI, 0);
	putpad(CE, 1);
	if (TE != 0)
		putpad(TE, 1);
	if (mesg[0] != '\0')
		writef("%s\n", mesg);
	flusho();
	screen_buffer_flush();
}
#endif /* TERMCAP */

#ifdef JOB_CONTROL
/********************/
void PauseJove(void)
/********************/
{
	UnsetTerm(ModBufs(0) ? "[There are modified buffers]" : NullStr);
	(void) kill(0, SIGTSTP);
	ResetTerm();
	ClAndRedraw();
}
#endif

#ifndef MAC
/***************/
void Push(void)
/***************/
{
#if !definded (MSDOS) && !defined (OS2IPROCS)
	int	pid;
	SIGRESULT	(*old_quit) proto((int)) = signal(SIGQUIT, SIG_IGN);
#endif /* MSDOS */
	SIGRESULT	(*old_int) proto((int)) = signal(SIGINT, SIG_IGN);
# if (defined(IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
	int	started;
# endif

#ifndef MSDOS
#ifdef IPROCS
	SigHold(SIGCHLD);
#endif
#if defined(TIOCGWINSZ) && defined(SIGWINCH) && defined(SigRelse)
	SigHold(SIGWINCH);
#endif
	alarm(0);
# if (defined(IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
	started = kbd_stop();
# endif
	switch (pid = fork()) {
	case -1:
# if (defined(IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
		if (started)
			(void) kbd_strt();
# endif
		complain("[Fork failed]");
		/*NOTREACHED*/

	case 0:
		UnsetTerm(ModBufs(0) ? "[There are modified buffers]" : NullStr);
#if defined(TIOCGWINSZ) && defined(SIGWINCH) && defined(SigRelse)
		SigRelse(SIGWINCH);
#endif
#ifdef IPROCS
		SigRelse(SIGCHLD);
#endif
		(void) signal(SIGTERM, SIG_DFL);
#else /* MSDOS */
	UnsetTerm(ModBufs(0) ? "[There are modified buffers]" : NullStr);
#endif /* MSDOS */
		signal(SIGINT, SIG_DFL);
#ifdef UNIX
		(void) signal(SIGQUIT, SIG_DFL);
		/* note that curbuf->bfname may be NULL */
		execl(Shell, basename(Shell), "-is", pr_name(curbuf->b_fname, NO),
			(char *)NULL);
		message("[Execl failed]");
		_exit(1);
	}
#ifdef IPROCS
	SigRelse(SIGCHLD);
#endif
	dowait(pid, (int *) 0);
#endif /* UNIX */
#ifdef MSDOS
	break_rst();
	if (spawnl(0, Shell, basename(Shell), (char *)0) == -1)
		message("[Spawn failed]");
#ifdef OS2
	  os2_kbd_open();
#endif /* OS2 */
#endif /* MSDOS */
#ifndef MAC
	ResetTerm();
#endif
#if defined(TIOCGWINSZ) && defined(SIGWINCH) && defined(SigRelse)
	SigRelse(SIGWINCH);
#endif
	ClAndRedraw();
#ifndef MSDOS
	(void) signal(SIGQUIT, old_quit);
#else /* MSDOS */
	break_off();
	getCWD();
#endif /* MSDOS */
#ifndef OS2
	(void) signal(SIGINT, old_int);
	if (UpdFreq != 0)
	    (void) alarm((unsigned) (UpdFreq - (time((time_t *) 0) % UpdFreq)));
#endif
# if (defined(IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
	if (started)
		(void) kbd_strt();
# endif
}
#endif /* MAC */

int	OKXonXoff = 0,		/* ^S and ^Q initially DON'T work */
	IntChar = CTL(']');

/**************************/
static void ttsize(void)
/**************************/
{
#ifdef UNIX
#   ifdef TIOCGWINSZ
	struct winsize win;

	if (ioctl (0, TIOCGWINSZ, (UnivPtr) &win) == 0) {
		if (win.ws_col)
			CO = win.ws_col;
		if (win.ws_row)
			LI = win.ws_row;
	}
#   else /* TIOCGWINSZ */
#	ifdef BTL_BLIT
#include <sys/jioctl.h>
	struct jwinsize jwin;

	if (ioctl(0, JWINSIZE, &jwin) == 0) {
		if (jwin.bytesx)
			CO = jwin.bytesx;
		if (jwin.bytesy)
			LI = jwin.bytesy;
	}
#	endif /* BTL_BLIT */
#   endif /* TIOCGWINSZ */
#endif /* UNIX */
#ifdef MAC
	CO = getCO();	/* see mac.c */
	LI = getLI();
	Windchange = 1;
	clr_page();
#endif
	ILI = LI - 1;
}

#ifdef BIFF
/******************************/
static void biff_init (void)
/******************************/
{
	dw_biff = ((BiffChk) &&
#   ifndef BSD4_2
		   ((tt_name != 0) || (tt_name = ttyname(0))) &&
		   (stat(tt_name, &tt_stat) != -1) &&
#   else
		   (fstat(0, &tt_stat) != -1) &&
#   endif
		   (tt_stat.st_mode & S_IEXEC));	/* he's using biff */

}

/*************************/
static void biff(int on)
/*************************/
{
	if (dw_biff == NO)
		return;
#   ifndef BSD4_2
	(void) chmod(tt_name, on ? tt_stat.st_mode :
				   (tt_stat.st_mode & ~S_IEXEC));
#   else
	(void) fchmod(0, on ? tt_stat.st_mode :
			      (tt_stat.st_mode & ~S_IEXEC));
#   endif
}

#endif /* BIFF */
/*************************/
static void ttinit(void) 
/*************************/
{
#ifdef BIFF
	biff_init();
#endif
#ifdef TIOCSLTC
	(void) ioctl(0, TIOCGLTC, (UnivPtr) &ls[OFF]);
	ls[ON] = ls[OFF];
	ls[ON].t_suspc = (char) -1;
	ls[ON].t_dsuspc = (char) -1;
	ls[ON].t_flushc = (char) -1;
	ls[ON].t_lnextc = (char) -1;
#endif

#if defined(TIOCGETC) && !defined(SYSV)
	/* Change interupt and quit. */
	(void) ioctl(0, TIOCGETC, (UnivPtr) &tc[OFF]);
	tc[ON] = tc[OFF];
	tc[ON].t_intrc = IntChar;
	tc[ON].t_quitc = (char) -1;
	if (OKXonXoff) {
		tc[ON].t_stopc = (char) -1;
		tc[ON].t_startc = (char) -1;
	}
#endif /* TIOCGETC */
	do_sgtty();
}

static int	done_ttinit = 0;

#ifndef	MSDOS
static
#endif
/****************************/
void do_sgtty(void)
/****************************/
{
#ifdef UNIX
# ifdef SYSV
	(void) ioctl(0, TCGETA, (char *) &sg[OFF]);
# else
	(void) gtty(0, &sg[OFF]);
# endif /* SYSV */
	sg[ON] = sg[OFF];

# ifdef LPASS8
	(void) ioctl(0, TIOCLGET, (UnivPtr) &lmword[OFF]);
	lmword[ON] = lmword[OFF];
	if (MetaKey == YES)
		lmword[ON] |= LPASS8;
	if (HZ)
		lmword[ON] &= ~LTILDE;
# endif

# ifdef SYSV
	TABS = !((sg[OFF].c_oflag & TAB3) == TAB3);
	ospeed = sg[OFF].c_cflag & CBAUD;

	if (OKXonXoff)
		sg[ON].c_iflag &= ~(IXON | IXOFF);
	sg[ON].c_iflag &= ~(INLCR|ICRNL|IGNCR);
	sg[ON].c_lflag &= ~(ICANON|ECHO);
	sg[ON].c_oflag &= ~(OCRNL|ONLCR);
	sg[ON].c_cc[VINTR] = IntChar;
	sg[ON].c_cc[VQUIT] = (char) -1;
	sg[ON].c_cc[VMIN] = sizeof smbuf;
	sg[ON].c_cc[VTIME] = 1;
# else
	TABS = !(sg[OFF].sg_flags & XTABS);
	sg[ON].sg_flags &= ~XTABS;
	ospeed = sg[OFF].sg_ospeed;
#  ifdef BRLUNIX
	sg[ON].sg_flags &= ~(ECHO | CRMOD);
	sg[ON].sg_flags |= CBREAK;

	/* VT100 Kludge: leave STALL on for flow control if DC3DC1 (Yuck.) */
	sg[ON].sg_xflags &= ~((sg[ON].sg_xflags&DC3DC1 ? 0 : STALL) | PAGE);
#  else
	sg[ON].sg_flags &= ~(ECHO | CRMOD);
#  endif /* BRLUNIX */

#  ifdef LPASS8
	sg[ON].sg_flags |= CBREAK;
#  else
	sg[ON].sg_flags |= (MetaKey ? RAW : CBREAK);
#  endif
# endif /* SYSV */
#endif /* UNIX */

#ifdef MSDOS
# ifndef IBMPC
	setmode(1, 0x8000);
# endif /* IBMPC */
	TABS = 0;
#endif /* MSDOS */
}
/********************/
void tty_reset(void)
/********************/
{
	if (!done_ttinit)
		return;
	ttyset(OFF);	/* go back to original modes */
	ttinit();
	ttyset(ON);
}

/* If n is OFF reset to original modes */

/********************/
void ttyset(int n)
/********************/
{
	if (!done_ttinit && n == 0)	/* Try to reset before we've set! */
		return;
#ifdef UNIX
# ifdef SYSV
	(void) ioctl(0, TCSETAW, (UnivPtr) &sg[n]);
# else
#  ifdef BRLUNIX
	(void) stty(0, &sg[n]);
#  else
	(void) ioctl(0, TIOCSETN, (UnivPtr) &sg[n]);
#  endif /* BRLUNIX */
# endif /* SYSV */

# if defined(TIOCSETC) && !defined(SYSV)
	(void) ioctl(0, TIOCSETC, (UnivPtr) &tc[n]);
# endif /* TIOCSETC */
# ifdef TIOCSLTC
	(void) ioctl(0, TIOCSLTC, (UnivPtr) &ls[n]);
# endif /* TIOCSLTC */
# ifdef LPASS8
	(void) ioctl(0, TIOCLSET, (UnivPtr) &lmword[n]);
# endif
#endif /* UNIX */

#ifdef MSDOS
# ifndef IBMPC
	setmode(1, n == 0 ? 0x4000 : 0x8000);
# endif
#endif /* MSDOS */
	done_ttinit = 1;
#ifdef BIFF
	biff(!n);
#endif
}

int	this_cmd,
	last_cmd,
	LastKeyStruck,
	MetaKey = 0;

/***************************/
int _fastcall get_ch (void)
/***************************/
{
	register  int	c,
			peekc;
	int _fastcall jgetchar( void );
	int _fastcall get_ch( void );

	if (Inputp) {
		if ((c = *Inputp++) != '\0')
			return LastKeyStruck = c;
		Inputp = NULL;
	}

	if (InJoverc)
		return EOF;	/* somethings wrong if Inputp runs out while
				   we're reading a .joverc file. */

#ifndef MSDOS
	if (ModCount >= SyncFreq) {
		ModCount = 0;
		SyncRec();
	}
#endif /* MSDOS */

	/* If we're not interactive and we're not executing a macro,
	   AND there are no ungetc'd characters, we read from the
	   terminal (i.e., get_ch()).  And characters only get put
	   in macros from inside this if. */
	if (((peekc = c = Peekc()) == EOF) &&
	    (Interactive || ((c = mac_getc()) == EOF))) {
		/* So messages that aren't error messages don't
		   hang around forever. */
		if (!UpdMesg && !Asking && mesgbuf[0] != '\0' && !errormsg)
			message(NullStr);
		redisplay();
#ifdef UNIX
		inIOread = 1;
#endif
		if ((c = jgetchar()) == EOF)
			finish(SIGHUP);
#ifdef UNIX
		inIOread = 0;
#endif

		if (!Interactive && InMacDefine)
			mac_putc(c);
	}
	if (peekc == EOF)	/* don't add_stroke peekc's */
		add_stroke(c);
	return LastKeyStruck = c;
}

#ifdef UNIX

/****************************/
static void dorecover(void)
/****************************/
{
	/* Since recover is a normal cooked mode program, reset the terminal */
	UnsetTerm(NullStr);
#if (defined(IPROCS) || defined (OS2IPROCS)) && defined(PIPEPROCS)
	kbd_kill();		/* kill the keyboard process */
#endif
	execl(Recover, "recover", "-d", TmpFilePath, (char *) NULL);
	writef("%s: execl failed!\n", Recover);
	flusho();
	_exit(-1);
	/* NOTREACHED */
}
#endif /* UNIX */

/**********************/
void ShowVersion(void)
/**********************/
{
	s_mess("Jonathan's Own Version of Emacs (%s)", version);
}

/************************************************************/
static void _fastcall UNIX_cmdline (int argc, char *argv[])
/************************************************************/
{
	int	lineno = 0,
		nwinds = 1;
	Buffer	*b;

	ShowVersion();
	while (argc > 1) {
		if (argv[1][0] != '-' && argv[1][0] != '+') {
			int	force = (nwinds > 0 || lineno != 0);

#ifdef MSDOS
			strlwr(argv[1]);
#endif
			minib_add(argv[1], force);
			b = do_find(nwinds > 0 ? curwind : (Window *) NULL,
				    argv[1], force);
			if (force) {
				SetABuf(curbuf);
				SetBuf(b);
				if (lineno >= 0)
					SetLine(next_line(curbuf->b_first, lineno));
				else
					SetLine(curbuf->b_last);
				if (nwinds > 1)
					NextWindow();
				if (nwinds)
					nwinds -= 1;
			}
			lineno = 0;
		} else	switch (argv[1][1]) {
			case 'd':
				argv += 1;
				argc -= 1;
				break;

			case 'j':	/* Ignore .joverc in HOME */
				break;
#ifndef MAC
			case 'p':
				argv += 1;
				argc -= 1;
				if (argv[1] != NULL) {
					SetBuf(do_find(curwind, argv[1], NO));
					ErrParse();
					nwinds = 0;
				}
				break;
#endif
			case 't':
				/* check if syntax is -tTag or -t Tag */
				if (argv[1][2] != '\0') {
					find_tag(&(argv[1][2]), YES);
				} else {
					argv += 1;
					argc -= 1;
					if (argv[1] != NULL)
						find_tag(argv[1], YES);
				}
				break;

			case 'w':
				if (argv[1][2] == '\0')
					nwinds += 1;
				else {
					int	n;

					(void) chr_to_int(&argv[1][2], 10, NO, &n);
					nwinds += -1 + n;
				}
				(void) div_wind(curwind, nwinds - 1);
				break;

			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				(void) chr_to_int(&argv[1][1], 10, NO, &lineno);
				lineno -= 1;
				break;
			case  0:
				lineno = -1;	/* goto end of file ... */
				break;		/* just like some people's */
		}				/* favourite editor */
		argv += 1;
		argc -= 1;
	}
}

#ifdef	STDARGS
	void
error(char *fmt, ...)
#else
	/*VARARGS1*/ void
error(fmt, va_alist)
	char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	if (fmt) {
		va_init(ap, fmt);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		UpdMesg = YES;
	}
	rbell();
	longjmp(mainjmp, ERROR);
}

#ifdef	STDARGS
	void
complain(char *fmt, ...)
#else
	/*VARARGS1*/ void
complain(fmt, va_alist)
	char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	if (fmt) {
		va_init(ap, fmt);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		UpdMesg = YES;
	}
	rbell();
	longjmp(mainjmp, COMPLAIN);
}

#ifdef	STDARGS
	void
confirm(char *fmt, ...)
#else
	/*VARARGS1*/ void
confirm(fmt, va_alist)
	char	*fmt;
	va_dcl
#endif
{
	char	*yorn;
	va_list	ap;

	va_init(ap, fmt);
	format(mesgbuf, sizeof mesgbuf, fmt, ap);
	va_end(ap);
	yorn = ask((char *) 0, mesgbuf);
	if (*yorn != 'Y' && *yorn != 'y')
		longjmp(mainjmp, COMPLAIN);
}

int	RecDepth = 0;

/*******************/
void Recur(void)
/*******************/
{
	char	bname[128];
	Mark	*m;

	swritef(bname, "%s", curbuf->b_name);
	m = MakeMark(curline, curchar, M_FLOATER);

	RecDepth += 1;
	UpdModLine = YES;
	DoKeys(NO);	/* NO means not first time */
	UpdModLine = YES;
	RecDepth -= 1;
	SetBuf(do_select(curwind, bname));
	if (!is_an_arg())
		ToMark(m);
	DelMark(m);
}

#ifdef MAC
jmp_buf auxjmp;
#endif

static int	iniargc;	/* main sets these for DoKeys() */
static char	**iniargv;


/******************************************/
static void _fastcall DoKeys(int firsttime)
/******************************************/
{
	int	c;
	jmp_buf	savejmp;
	int _fastcall get_ch(void);

	push_env(savejmp);

	switch (setjmp(mainjmp)) {
	case 0:
		if (firsttime)
			UNIX_cmdline(iniargc, iniargv);
		break;

	case QUIT:
		if (RecDepth == 0) {
			if (ModMacs()) {
				rbell();
				if (CharUpcase(*ask("No",
"Some MACROS haven't been saved; leave anyway? ")) != 'Y')
					break;
			}
			if (ModBufs(0)) {
				rbell();
				if (CharUpcase(*ask("No",
"Some buffers haven't been saved; leave anyway? ")) != 'Y')
					break;
			}
#if defined (IPROCS) || defined (OS2IPROCS)
			KillProcs();
#endif
		}
		pop_env(savejmp);
		return;

	case ERROR:
		getDOT();	/* God knows what state linebuf was in */
		/*FALLTHROUGH*/
	case COMPLAIN:
	    {
		gc_openfiles();		/* close any files we left open */
		errormsg = YES;
		unwind_macro_stack();
		Asking = 0;
		curwind->w_bufp = curbuf;
		DisabledRedisplay = NO;
		redisplay();
		break;
	    }
	}

	this_cmd = last_cmd = 0;

	for (;;) {
#ifdef MAC
		setjmp(auxjmp);
#endif
		if (this_cmd != ARG_CMD) {
			clr_arg_value();
			last_cmd = this_cmd;
			init_strokes();
		}
#ifdef MAC
		HiliteMenu(0);
		EventCmd = 0;
		menus_on();
#endif
		c = get_ch();
		if (c == EOF)
			continue;
		dispatch(c);
	}
}

int	Crashing = 0;


/**************************************************/
static char ** scanvec(char **args, char *str)
/**************************************************/
{
	while (*args) {
		if (strcmp(*args, str) == 0)
			return args;
		args += 1;
	}
	return 0;
}

#ifdef UNIX
int	UpdFreq = 30,
	inIOread = 0;

/*************************************/
static SIGRESULT updmode ( 
      int junk /* passed in on signal; of no interest */)
/*************************************/
{
	UpdModLine = YES;
	if (inIOread)
		redisplay();
#ifndef JOB_CONTROL
	(void) signal(SIGALRM, updmode);
#endif
	if (UpdFreq != 0)
		(void) alarm((unsigned) (UpdFreq - (time((time_t *) 0) % UpdFreq)));
	SIGRETURN;
}
#endif /* UNIX */

#ifdef MSDOS
# ifndef IBMPC
char	ttbuf[JBUFSIZ];
# endif	/* IBMPC */
#endif /* MSDOS */

#if defined(MAC) || (defined(TIOCGWINSZ) && defined(SIGWINCH))
#ifndef	MAC
static
#endif

/***********************/
SIGRESULT win_reshape(
     int junk	/* passed in when invoked by a signal; of no interest */)
/***********************/
{
	/* register */ int oldLI;
	int oldCO;

	/*
	 * Save old number of lines and columns.
	 */
	oldLI = LI;
	oldCO = CO;

	/*
	 * Get new line/col info (in LI/CO).
	 */
	ttsize();

	if (LI != oldLI || CO != oldCO) {
		if (LI != oldLI) {
			/*
 			 * Go through the window list, changing each
			 * window size in proportion to the resize. If a
			 * window becomes too small, delete it. We keep
			 * track of all the excess lines (caused by
			 * roundoff!), and give them to the current
			 * window, as a sop - can't be more than one or
			 * two lines anyway. This seems fairer than just
			 * resizing the current window.
			 */
			/* register */ int newsize, total;
			/* register */ Window *wp;

			wp = fwind;
			total = 0;
			do {
				newsize = LI * wp->w_height / oldLI;

				if (newsize < 2) {
					total += wp->w_height;
					wp = wp->w_next;
					del_wind(wp->w_prev);
				}
				else {
					wp->w_height = newsize;
					total += newsize;
					wp = wp->w_next;
				}
			} while (wp != fwind);

			curwind->w_height += LI - total - 1;
		} /* LI != oldLI */

		/* Make a new screen structure */
		make_scr();
		/* Do a 'hard' update on the screen - clear and redraw */
		cl_scr(YES);
		flusho();
		screen_buffer_flush();
		redisplay();
	} /* LI != oldLI || CO != oldCO */

	SIGRETURN;
}
#endif

void

#ifdef MAC	/* will get args from user, if option key held during launch */
main(void)
{
	int argc;
	char **argv;
#else
/****************************/
main (int argc, char *argv[])
/****************************/
{
#endif /* MAC */
	char	*cp;
	char	ttbuf[MAXTTYBUF];
	int	fd_std; // for checking file descriptor	at differnt points
#ifndef MSDOS
# ifndef VMUNIX
	char	s_iobuff[LBSIZE],
		s_genbuf[LBSIZE],
		s_linebuf[LBSIZE];
	/* The way I look at it, there ain't no way I is gonna run
	   out of stack space UNLESS I have some kind of infinite
	   recursive bug.  So why use up some valuable memory, when
	   there is plenty of space on the stack?  (This only matters
	   on wimpy pdp11's, of course.) */

	iobuff = s_iobuff;
	genbuf = s_genbuf;
	linebuf = s_linebuf;
# endif

#else /* MSDOS */
	char	*getenv();
#endif /* MSDOS */

#ifdef MAC
	MacInit();		/* initializes all */
	if(make_cache() == 0) exit(-1);
	argc = getArgs(&argv);
#endif /* MAC */

	iniargc = argc;
	iniargv = argv;

	if (setjmp(mainjmp)) {
		writef("\rAck! I can't deal with error \"%s\" now.\n\r", mesgbuf);
		finish(6);
	}

#ifdef MSDOS
	/* import the temporary file path from the environment and
	   fix the string, so that we can append a slash safely	*/

	if (((cp = getenv("TMP")) || (cp = getenv("TMPDIR"))) &&
	    (*cp != '\0')) {
		strcpy(TmpFilePath, cp);
		cp = &TmpFilePath[strlen(TmpFilePath)-1];
		if ((*cp == '/') || (*cp == '\\'))
			*cp = 0;
	}
	ShFlags[0] = switchar();
#endif /* MSDOS */

#ifdef UNIX
	/*
	 * Get the temporary and library file paths from the environment.
	 */
	{
		/* register */ char ***lfp, *dir, *buf;
		static char **libfiles[] = {
			&Recover,
			&CmdDb,
			&Joverc,
			0
		};

		if ((dir = getenv("JOVETMP")) &&
			strlen(dir) <
				(sizeof TmpFilePath - sizeof d_tempfile - 1))
			strcpy(TmpFilePath, dir);

		if (dir = getenv("JOVELIB"))
			for (lfp = libfiles; *lfp; lfp++) {
				if (!(cp = strrchr(**lfp, '/')))
					continue;
				cp++;
				buf = emalloc(strlen(cp) +
					strlen(dir) + 2);
				sprintf(buf, "%s/%s", dir, cp);
				**lfp = buf;
			}
	}
#endif /* UNIX */

	getTERM();	/* Get terminal. */
#ifndef METAKEY
	if (getenv("METAKEY"))
#endif /* METAKEY */
		MetaKey = 1;
	ttsize();
#ifdef MAC
	InitEvents();
#else
	InitCM();
#endif

	d_cache_init();		/* initialize the disk buffer cache */
#ifdef MSDOS
	if ((cp = getenv("COMSPEC")) && (*cp != '\0')) {
		strcpy(Shell, cp);
	}
	if ((cp = getenv("DESCRIBE")) && (*cp != '\0'))
	   strcpy(CmdDb, cp);
#else /* !MSDOS */
#ifndef MAC
	if ((cp = getenv("SHELL"))!=NULL && (*cp != '\0')) {
		strcpy(Shell, cp);
	}
#endif
#endif /* !MSDOS */

	make_scr();
	mac_init();	/* Initialize Macros */
	winit();	/* Initialize Window */
#if defined (IPROCS) || defined (OS2IPROCS)
	fd_std = fileno (stdout);
	pinit();	/* Pipes/process initialization */
#endif
	buf_init();

	{
		char	**argp;

		if ((argp = scanvec(argv, "-d"))!=NULL
#ifdef UNIX
		    && chkCWD(argp[1])
#endif
		    )
			setCWD(argp[1]);
		else
			getCWD();	/* After we setup curbuf in case we have to getwd() */
	}

	HomeDir = getenv("HOME");
	if (HomeDir == NULL)
		HomeDir = "/";
	HomeLen = strlen(HomeDir);

#ifdef UNIX
# ifdef SYSV
	swritef(Mailbox, "/usr/mail/%s", getenv("LOGNAME"));
# else
	swritef(Mailbox, "/usr/spool/mail/%s", getenv("USER"));
# endif
#endif

	InitKeymaps();
#ifdef OS2
	os2_kbd_open();
#endif
	ttinit();	/* initialize terminal (before ~/.joverc) */
	settout(ttbuf);	/* not until we know baudrate */
#ifndef MAC
	ResetTerm();
#endif

	(void) joverc(Joverc);			/* system wide .joverc */
	cp = 0;
#if defined(MSDOS) || defined(UNIX)
	/* If a JOVERC environment variable is set, then use that instead */
	if ((cp = getenv("JOVERC"))!=NULL && (*cp != '\0'))
	   (void) joverc(cp);
#endif /* MSDOS || UNIX */
	if (!scanvec(argv, "-j") && (!cp || *cp == '\0')) {
		char	tmpbuf[100];

		swritef(tmpbuf, "%s/.joverc", HomeDir);
		(void) joverc(tmpbuf);		/* .joverc in home directory */
	}

#ifndef MSDOS
	if (scanvec(argv, "-r"))
		dorecover();
	if (scanvec(argv, "-rc"))
		FullRecover();
#endif	/* MSDOS */

#ifdef MSDOS
	(void) signal(SIGINT, SIG_IGN);
	break_off();	/* disable ctrl-c checking */
#endif /* MSDOS */
#ifdef UNIX
	(void) signal(SIGHUP, finish);
	(void) signal(SIGINT, finish);
	(void) signal(SIGBUS, finish);
	(void) signal(SIGSEGV, finish);
	(void) signal(SIGPIPE, finish);
	(void) signal(SIGTERM, SIG_IGN);
# if defined(TIOCGWINSZ) && defined(SIGWINCH)
	(void) signal(SIGWINCH, win_reshape);
# endif
	/* set things up to update the modeline every UpdFreq seconds */
	(void) signal(SIGALRM, updmode);
	if (UpdFreq != 0)
		(void) alarm((unsigned) (UpdFreq - (time((time_t *) 0) % UpdFreq)));
#endif /* UNIX */
	cl_scr(1);
	flusho();
	screen_buffer_flush();
	RedrawDisplay();	/* start the redisplay process. */
	DoKeys(YES);
	finish(0);
}

#ifdef MSDOS

#include <dos.h>

static	char break_state;

/* set the break state to off */

/*****************************/
static void  break_off(void)
/*****************************/
{
#ifndef OS2
	union REGS regs;

	regs.h.ah = 0x33;		/* break status */
	regs.h.al = 0x00;		/* request current state */
	intdos(&regs, &regs);
	break_state = regs.h.dl;
	bdos(0x33, 0, 1);	/* turn off break */
#endif
}

/* reset the break state */
/******************************/
static void break_rst (void)
/******************************/
{
#ifndef OS2
	bdos(0x33, break_state, 1);
# else
	KBDINFO   kbdInfo;

	kbdInfo.cb = 0x000A;
	KbdGetStatus(&kbdInfo, 0);
	kbdInfo.fsMask &= ~0x0001;		/* not echo on		*/
	kbdInfo.fsMask |= 0x0002;		/* echo off		*/
	kbdInfo.fsMask &= ~0x0008;		/* cooked mode off	*/
	kbdInfo.fsMask |= 0x0004;		/* raw mode		*/
	kbdInfo.fsMask &= ~0x0100;		/* shift report	off	*/
	KbdSetStatus(&kbdInfo, 0);
#endif /* OS2 */
}
#endif

/*************************************/
static void _fastcall kbd_reset(void)
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
