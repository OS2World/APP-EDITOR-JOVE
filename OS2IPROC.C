/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#ifdef OS2
# define INCL_DOS
# include <os2.h>
# include <process.h>
# include <d:\c600\include\io.h>
# include <fcntl.h>
# include <string.h>
// prevent inheritance of files
#  define no_inherit(fd) DosSetFHandState (fd, OPEN_FLAGS_NOINHERIT);
struct _iobuf {
	char _FAR_ *_ptr;
	int   _cnt;
	char _FAR_ *_base;
	char  _flag;
	char  _file;
	};
typedef struct _iobuf FILE;
int fputs( char *string, FILE *stream );
/* declare _iob[] array */

# ifndef _STDIO_DEFINED
# ifdef _DLL
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

#endif

#include "re.h"
#include "ctype.h"
#include "disp.h"
#if ( defined( IPROCS ) || defined( OS2IPROCS ) )
# include "fp.h"
# include "iproc.h"
#endif

#ifdef	STDARGS
# include <stdarg.h>
#else
# include <varargs.h>
#endif

#if ( defined( IPROCS ) || defined( OS2IPROCS ) )

private void
	_fastcall proc_rec( register Process *p, char *buf ),
	proc_close( Process *p ),
	proc_kill( Process *p, int sig ),
	_fastcall SendData( int newlinep );
static int kill( int pid, int sig );

private SIGRESULT
	proc_child proto((int));

/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* NOTE WELL:
 * This file is "included" into iproc.c -- it is not compiled separately!
 */

#include <signal.h>
//#include <stdlib.h>
#include "wait.h"

#define DEAD	1	/* Dead but haven't informed user yet */
#define STOPPED	2	/* Job stopped */
#define RUNNING	3	/* Just running */
#define NEW	4	/* This process is brand new */

/* If process is dead, flags says how. */
#define EXITED	1
#define KILLED	2

#define isdead(p)	((p) == NULL || proc_state((p)) == DEAD || (p)->p_toproc == -1)
#define makedead(p)	{ proc_state((p)) = DEAD; }

#define proc_buf(p)	((p)->p_buffer->b_name)
#define proc_cmd(p)	((p)->p_name)
#define proc_state(p)	((p)->p_state)

static Process	*procs = (Process *)0;

File	*ProcInput ;
int	ProcOutput,
	kbd_pid = 0,
	NumProcs = 0;
/************************************/
static Process *proc_pid( int pid)
/************************************/
{
	register Process	*p;

	for (p = procs; p != (Process *)0; p = p->p_next)
		if (p->p_portpid == pid)
			break;

	return (p);
}

/******************************************************/
void _fastcall read_proc (int pid, register int nbytes)
/******************************************************/
{
	int _fastcall f_readn( File *fp, char *addr, int n );
	register Process	*p;
	int	n;
	char	ibuf[512];

	p = proc_pid ( pid );
	if (p == (Process *)0) {
		writef( "\riproc: unknown pid (%d)", pid );
		return;
	}

	if (proc_state( p ) == NEW) {
		int	rpid;
		/* pid of real child, not of portsrv */

		f_readn (ProcInput, (char *) &rpid, sizeof (int));
		p->p_pid = rpid;
		p->p_state = RUNNING;
		return;
	}

	if (nbytes == EOF) {		/* okay to clean up this process */
		int	 pid;
		union wait  status;

		f_readn( ProcInput, (int *)& status, sizeof ( int ) );
		do {
			pid = wait((int *) 0);
			if (pid < 0)
				break;
			kill_off( pid, status.w_status );
		} while( pid != p->p_portpid );
		proc_close( p );
		makedead( p );
		return;
	}

	while (nbytes > 0) {
		n = min( (sizeof ibuf) - 1, nbytes );
		f_readn( ProcInput, ibuf, n );
		ibuf[n] = 0;	/* Null terminate for convenience */
		nbytes -= n;
		proc_rec( p, ibuf );
	}
}

/********************/
void ProcKill (void)
/********************/
{
	proc_kill (curbuf->b_process, SIGKILL);
}

/******************/
void ProcInt(void)
/******************/
{
	proc_kill(curbuf->b_process, SIGINT);
}

/********************/
void ProcQuit (void)
/********************/
{
	proc_kill(curbuf->b_process, SIGQUIT);
}
/***********************************/
static void proc_close( Process *p )
/***********************************/
{
	if ( p->p_toproc >= 0 ) {
		DosClose( p->p_toproc );
		p->p_toproc = -1;	/* writes will fail */
		NumProcs -= 1;
	}
}
/********************************************************/
size_t _fastcall fix_new_line (char *buf, size_t nbytes)
/********************************************************/
/* replace LF with CR-LF at the end of line. Return the new */
/* number of bytes */
#define CR 0x0D
#define LF 0x0A
{
	if (buf[nbytes - 1] == LF && buf[nbytes - 2] != CR) {
		buf[nbytes - 1] = CR;
		buf[nbytes] = LF;
	}
	return (nbytes+1);
}

/********************************************************/
void proc_write ( Process *p, char *buf, size_t nbytes )
/********************************************************/
{
	size_t	_fastcall fix_new_line (char *buf, size_t nbytes);
	int	n_written;
	size_t  fixed_nbytes;

	fixed_nbytes = fix_new_line (buf, nbytes);
	n_written = write (p->p_toproc, buf, fixed_nbytes);
	// n_written = write( p->p_toproc, buf, nbytes );
}

/*****************************************************************/
void make_cmd (char *argv[], char cmd[])
/*****************************************************************/
/* prepare command line from ARGV. stop in a NULL element */
{
	int arg = 0;
	char *ptcom;


	ptcom = cmd;
	while ( argv[arg] != NULL )  {
	    strcpy ( ptcom, argv[arg] );
	    ptcom += strlen ( argv[arg] );
	    if ( arg == 0 )
		ptcom ++;	// leave a null byte before arguments
	     else {
		*ptcom = ' ';	// leave a space between arguments
		ptcom++;
	    };
	    arg++;
	}
	*ptcom = *(ptcom+1) = '\0';   // two null bytes at the end of arguments
}

#ifdef	STDARGS
	static void
proc_strt(char *bufname, int clobber, ...)
#else
	static /*VARARGS3*/ void
proc_strt(bufname, clobber, va_alist)
/*************************************/
	char	*bufname;
	int	clobber;
	va_dcl
#endif
{
	Window	*owind = curwind;
	int	toproc[2],
		pid,
		      spawn_code;
	HFILE	      hRead, hWrite;		     // pipe handles
	char	      portsrv_cmd[80], objbuf[80], cmd_name[80];
	RESULTCODES   result_codes;

	Process	*newp;
	Buffer	*newbuf;
	char	*argv[32],
		*cp,
		foo[10],
		cmdbuf[128];
	int	i, fd;
	va_list	ap;
	FILE	*toproc_fp;

	isprocbuf (bufname);	/* make sure BUFNAME is either nonexistant
				   or is of type B_PROCESS */
	_pipe (toproc, 512, O_BINARY);
	no_inherit (toproc[1]);
/* launching child process */
launch:
  {
	int fd_stdin, fd_stdout, fd_stderr, fd_std;
	int err_code;

	fd_stdin  = dup (fileno (stdin));
	fd_stdout = dup (fileno (stdout));
	fd_stderr = dup (fileno (stderr));

	argv[0] = "portsrv.exe";
	va_init(ap, clobber);
	make_argv (&argv[1], ap);
	va_end(ap);

	err_code = dup2 (toproc[0],  0);
	err_code = dup2 (ProcOutput, 1);
	err_code = dup2 (ProcOutput, 2);

	make_cmd (argv, portsrv_cmd);	// make command line
	for (fd = 3; fd <= 20; fd++) no_inherit (fd);
	err_code = DosExecPgm (objbuf, 80, 2, portsrv_cmd,
			      NULL, &result_codes, portsrv_cmd);
	//spawn_code = spawnv (P_NOWAIT, "portsrv.exe", argv);
	spawn_code = (err_code == 0) ? result_codes.codeTerminate : -1;
	switch (spawn_code) {
	case (-1):
		writef ("spawn failed\n");
		_exit (1);
		break;
	default:
		pid = spawn_code;
		break;
	}
/* return standard fd to their original state */
	err_code = dup2 (fd_stdin,  fileno (stdin));
	err_code = dup2 (fd_stdout, fileno (stdout));
	err_code = dup2 (fd_stderr, fileno (stderr));

	DosClose (fd_stdin);
	DosClose (fd_stdout);
	DosClose (fd_stderr);
  }
	newp = (Process *) malloc (sizeof *newp);
	newp->p_next = procs;
	newp->p_state = NEW;

	cmdbuf[0] = '\0';
	va_init (ap, clobber);
	while (cp = va_arg (ap, char *))
		swritef (&cmdbuf[strlen( cmdbuf )], "%s ", cp);
	va_end (ap);
	va_init (ap, clobber);
	newp->p_name = copystr (cmdbuf);
	procs = newp;
	newp->p_portpid = pid;
	newp->p_pid = -1;

	newbuf = do_select ((Window *) 0, bufname);
	newbuf->b_type = B_PROCESS;
	newp->p_buffer = newbuf;
	newbuf->b_process = newp;	/* sorta circular, eh? */
	pop_wind (bufname, clobber, B_PROCESS);
	ToLast();
	if (!bolp())
		LineInsert(1);
	/* Pop_wind() after everything is set up; important!
	   Bindings won't work right unless newbuf->b_process is already
	   set up BEFORE NEWBUF is first SetBuf()'d. */
	newp->p_mark = MakeMark( curline, curchar, M_FLOATER );
	newp->p_dbx_mode = NO;

	newp->p_toproc = toproc[1];
	newp->p_reason = 0;
	NumProcs += 1;
	if (NumProcs == 1)
		  kbd_strt();
	DosClose(toproc[0]);
	SetWind(owind );
}

/****************/
void pinit(void)
/****************/
{
	int		p[2];
	int		fd;
	HFILE		new_handle;
	HFILE		hRead, hWrite;	     // pipe handles
	int		err_code;
	RESULTCODES	result_codes;
	char		kbd_cmd[80], objbuf[80];

	_pipe( p, 512, O_BINARY );
	ProcInput = fd_open( "process-input", F_READ | F_LOCKED, p[0],
			    (char *) 0, 512 );

	ProcOutput = p[1];

/* launching the kbd process */
launch:
  {
	int spawn_code;
	int fd_stdin, fd_stdout, fd_stderr, fd_std;
/* store original handles of stdin,out,err */

	fd_stdin  = dup ( fileno (stdin)  );
	fd_stdout = dup ( fileno (stdout) );
	fd_stderr = dup ( fileno (stderr) );

	signal(SIGINT, SIG_IGN);

	err_code = dup2( ProcOutput, 1 );
	err_code = dup2( ProcOutput, 2 );
/* no iheritance */

	for (fd = 3; fd <=20; fd++) no_inherit( fd );
	strcpy ( kbd_cmd,"d:\\tmp\\jove\\kbd.exe" );
	err_code = DosExecPgm ( objbuf, 80, 2, kbd_cmd, NULL,
			       &result_codes, kbd_cmd );
	spawn_code = ( err_code == 0 ) ? result_codes.codeTerminate : -1;
	//spawn_code = spawnl ( P_NOWAIT, "kbd.exe", "kbd", NULL );
	switch ( spawn_code ) {
	case (-1):
		write( 2, "kdb spawn failed\n", 16 );
		exit(-1);
		break;
	default:
		kbd_pid = spawn_code;
		break;
	}
/* restore original handles of std/in/out/err */
	dup2 ( fd_stdin,  fileno ( stdin) );
	dup2 ( fd_stdout, fileno ( stdout) );
	dup2 ( fd_stderr, fileno ( stderr) );

	DosClose ( fd_stdin );
	DosClose ( fd_stdout );
	DosClose ( fd_stderr );
  }
}

static int	kbd_state = OFF;

/* kbd_strt() and kbd_stop() return true if they changed the state
   of the keyboard process.  E.g., kbd_strt() returns TRUE if the
   kbd process was previously stopped.  This is so kbd starting and
   stopping in pairs works - see finish() in jove.c. */
/*****************/
int kbd_strt(void)
/*****************/
{
	if ( kbd_state == OFF ) {
		kbd_state = ON;
		kill( kbd_pid, SIGQUIT );
		return TRUE;
	}
	return FALSE;
}
/********************/
int kbd_stop(void)
/********************/
{
	if (kbd_state == ON) {
		kbd_state = OFF;
		kill( kbd_pid, SIGQUIT );
		return TRUE;
	}
	return FALSE;
}
/********************/
int kbd_kill( void )
/********************/
{
	if (kbd_pid != 0) {
		kill( kbd_pid, SIGKILL );
		kbd_pid = 0;
	}
}

char	proc_prompt[128] = "]";

/************************/
char *pstate(Process *p)
/************************/
{
	switch (proc_state(p)) {
	case NEW:
		return "New";

	case STOPPED:
		return "Stopped";

	case RUNNING:
		return "Running";

	case DEAD:
		if (p->p_howdied == EXITED) {
			if (p->p_reason == 0)
				return "Done";
			return sprint("Exit %d", p->p_reason);
		}
		return sprint( "Killed %d", p->p_reason );

	default:
		return "Unknown state";
	}
}

/**********************/
void KillProcs( void )
/**********************/
{
	register  Process	*p;
	register  int	killem = -1;		/* -1 means undetermined */
	register  char	*yorn;

	for (p = procs; p != 0; p = p->p_next)
		if (!isdead(p)) {
			if (killem == -1) {
				yorn = ask("y", "Should I kill your i-processes? ");
				killem = (CharUpcase(*yorn) == 'Y');
			}
			if (killem)
				proc_kill (p, SIGKILL);
		}
}

/*********************************/
void pbuftiedp(register Buffer *b)
/*********************************/
{
	register Process *p = b->b_process;

	if (!isdead(p))
		complain("Process %s, attached to %b, is %s.",
			 proc_cmd(p), b, pstate(p));
}

char	dbx_parse_fmt[128] = "line \\([0-9]*\\) in \\{file,\\} *\"\\([^\"]*\\)\"";

/**********************/
void DBXpoutput(void)
/**********************/
{
	if (curbuf->b_process == 0)
		complain("[Must be in a process buffer to enable dbx mode]");
	curbuf->b_process->p_dbx_mode = !curbuf->b_process->p_dbx_mode;
	UpdModLine = YES;
}
/********************************/
static void watch_input(Mark *m)
/********************************/
{
	Bufpos	save;
	char	fname[FILESIZE],
		lineno[FILESIZE];
	int	lnum;
	Window	*savew = curwind;
	Buffer	*buf;

	DOTsave(&save);
	ToMark(m);
	if (dosearch(dbx_parse_fmt, FORWARD, YES) != NULL) {
		get_FL_info (fname, lineno);
		buf = do_find ((Window *) 0, fname, YES);
		pop_wind (buf->b_name, NO, -1);
		lnum = atoi(lineno);
		SetLine(next_line(buf->b_first, lnum - 1));
		SetWind(savew);
	}
	SetDot(&save);
}

/* Process receive: receives the characters in buf, and appends them to
   the buffer associated with p. */
/*************************************************************/
static	void _fastcall proc_rec (register Process *p, char *buf)
/*************************************************************/
{
	Buffer	*saveb = curbuf;
	register  Window	*w;
	register  Mark	*savepoint;
	int	sameplace = NO,
		do_disp = NO;

	if (curwind->w_bufp == p->p_buffer)
		w = curwind;
	else
		w = windbp(p->p_buffer);	/* Is this window visible? */
	if (w != 0)
		do_disp = (in_window(w, p->p_mark->m_line) != -1);
	SetBuf(p->p_buffer);
	savepoint = MakeMark(curline, curchar, M_FLOATER);
	ToMark(p->p_mark);		/* where output last stopped */
	if (savepoint->m_line == curline && savepoint->m_char == curchar)
		sameplace = YES;
	ins_str(buf, YES);
	if (do_disp == YES && p->p_dbx_mode == YES)
		watch_input(p->p_mark);
	MarkSet(p->p_mark, curline, curchar);
	if (!sameplace)
		ToMark(savepoint);	/* back to where we were */
	DelMark(savepoint);
	/* redisplay now, instead of right after the ins_str, so that
	   we don't get a bouncing effect if point is not the same as
	   the process output position */
	if (do_disp) {
		w->w_line = curline;
		w->w_char = curchar;
		redisplay();
	}
	SetBuf(saveb);
}
/***************************************************/
static void proc_kill(Process *p, int sig)
/***************************************************/
{
	if (isdead(p))
		return;
	if (killpg (p->p_pid, sig) == -1) {
	//if (killpg (p->p_portpid, sig) == -1) {
		s_mess("Cannot kill %s!", proc_buf(p));
		return;
	}
}
/*************************************/
static int kill (int pid, int sig)
/*************************************/
/* imitate the unix kill command for OS2 */
{
    int err_code;
    switch (sig) {
	case (SIGKILL) : err_code = DosKillProcess (DKP_PROCESS, pid);
	    break;
//	case (SIGQUIT) : err_code = DosFlagProcess (pid, DCWA_PROCESSTREE,
//						      PFLG_A, 0);
	case (SIGQUIT) : err_code = DosFlagProcess (pid, FLGP_PID,
						      PFLG_A, 0);
	    break;
	case (SIGINT)  : err_code = DosSendSignal (pid, SIGINT);
    }
    if (err_code != 0) return (-1);
	else
	    return (0);


}
/* Free process CHILD.  Do all the necessary cleaning up (closing fd's,
   etc.). */
/**************************************/
static void free_proc (Process *child)
/**************************************/
{
	register  Process	*p,
				*prev = 0;

	if (!isdead(child))
		return;
	for (p = procs; p != child; prev = p, p = p->p_next)
		;
	if (prev == 0)
		procs = child->p_next;
	else
		prev->p_next = child->p_next;
	proc_close(child);		/* if not already closed */

	/* It's possible that the buffer has been given another process
	   between the time CHILD dies and CHILD's death is noticed (via
	   list-processes).  So we only set it the buffer's process to
	   0 if CHILD is still the controlling process. */
	if (child->p_buffer->b_process == child) {
		child->p_buffer->b_process = 0;
	}
	{
		Buffer	*old = curbuf;

		SetBuf(child->p_buffer);
		DelMark(child->p_mark);
		SetBuf(old);
	}
	free((char *) child->p_name);
	free((char *) child);
}

/*********************/
void ProcList(void)
/*********************/
{
	register  Process	*p,
				*next;
	char	*fmt = "%-15s  %-15s  %-8s %s",
		pidstr[16];
	void _fastcall screen_buffer_flush (void);

	if (procs == 0) {
		message("[No subprocesses]");
		return;
	}
	TOstart("Process list", TRUE);

	Typeout(fmt, "Buffer", "Status", "Pid ", "Command");
	Typeout(fmt, "------", "------", "--- ", "-------");
	for (p = procs; p != 0; p = next) {
		next = p->p_next;
		swritef(pidstr, "%d", p->p_pid);
		Typeout(fmt, proc_buf(p), pstate(p), pidstr, p->p_name);
		if (isdead(p)) {
			free_proc(p);
			UpdModLine = YES;
		}
	}
	TOstop();
}
/*************************************/
static void do_rtp (register Mark *mp)
/*************************************/
{
	register  Process *p = curbuf->b_process;
	Line	*line1 = curline,
		*line2 = mp->m_line;
	int	char1 = curchar,
		char2 = mp->m_char;
	char	*gp;
	size_t	nbytes;

	if (isdead(p) || p->p_buffer != curbuf)
		return;

	(void) fixorder(&line1, &char1, &line2, &char2);
	while (line1 != line2->l_next) {
		gp = ltobuf(line1, genbuf) + char1;
		if (line1 == line2)
			gp[char2] = '\0';
		else
			strcat(gp, "\n");
		if ((nbytes = strlen(gp)) != 0)
			proc_write(p, gp, nbytes);
		line1 = line1->l_next;
		char1 = 0;
	}
}

/***********************/
void ProcNewline (void)
/***********************/
{
#ifdef ABBREV
	MaybeAbbrevExpand();
#endif
	SendData(YES);
}

/***********************/
void ProcSendData (void)
/***********************/
{
#ifdef ABBREV
	MaybeAbbrevExpand();
#endif
	SendData(NO);
}
/**********************************************/
static void _fastcall SendData (int newlinep )
/**********************************************/
{
	register Process	*p = curbuf->b_process;
	register char	*lp,
			*gp;	/* JF fix for better prompt handling */

	if (isdead(p))
		return;
	/* If the process mark was involved in a big deletion, because
	   the user hit ^W or something, then let's do some magic with
	   the process mark.  Problem is that if the user yanks back the
	   text he deleted, the mark stays at the beginning of the region,
	   and so the next time SendData() is called the entire region
	   will be sent.  That's not good.  So, to deal with that we reset
	   the mark to the last line, after skipping over the prompt, etc. */
	if (p->p_mark->m_flags & M_BIG_DELETE) {
		Bufpos	bp;

		p->p_mark->m_flags &= ~M_BIG_DELETE;

		DOTsave(&bp);
		ToLast();
		Bol();
		/* While we're looking at a prompt, and while we're
		   moving forward.  This is for people who accidently
		   set their process-prompt to ">*" which will always
		   match! */
		while ((LookingAt(proc_prompt, linebuf, curchar)) &&
		       (REeom > curchar))
			curchar = REeom;
		MarkSet(p->p_mark, curline, curchar);
		SetDot(&bp);
	}

	if (lastp(curline)) {
		Eol();
		if (newlinep)
			LineInsert(1);
		do_rtp(p->p_mark);
		MarkSet(p->p_mark, curline, curchar);
	} else {
		/* Either we're looking at a prompt, or we're not, in
		   which case we want to strip off the beginning of the
		   line anything that looks like what the prompt at the
		   end of the file is.  In other words, if "(dbx) stop in
		   ProcessNewline" is the line we're on, and the last
		   line in the buffer is "(dbx) ", then we strip off the
		   leading "(dbx) " from this line, because we know it's
		   part of the prompt.  But this only happens if "(dbx) "
		   isn't one of the process prompts ... follow what I'm
		   saying? */
		Bol();
		if (LookingAt(proc_prompt, linebuf, curchar)) {
			do
				curchar = REeom;
			while ((LookingAt(proc_prompt, linebuf, curchar)) &&
			       (REeom > curchar));
			strcpy(genbuf, linebuf + curchar);
			Eof();
			ins_str(genbuf, NO);
		} else {
			strcpy(genbuf, linebuf + curchar);
			Eof();
			gp = genbuf;
			lp = linebuf;
			while (*lp == *gp && *lp != '\0') {
				lp += 1;
				gp += 1;
			}
			ins_str(gp, NO);
		}
	}
}

/********************/
void ShellProc(void)
/********************/
{
	char	*shbuf = "*shell*";
	register  Buffer *b;

	b = buf_exists(shbuf);
	if (b == 0 || isdead(b->b_process))
		//proc_strt(shbuf, NO, Shell, "-i", (char *) 0);
		proc_strt(shbuf, NO, Shell, "/K", (char *) 0);
	pop_wind(shbuf, NO, -1);
}

/*******************/
void Iprocess(void)
/*******************/
{
	register  char	*command;
	char	scratch[64],
		*bnm;
	int	cnt = 1;
	Buffer	*bp;

	command = ask(ShcomBuf, ProcFmt);
	null_ncpy(ShcomBuf, command, (sizeof ShcomBuf) - 1);
	bnm = MakeName(command);
	strcpy(scratch, bnm);
	while ((bp = buf_exists(scratch)) != NIL && !isdead(bp->b_process))
		swritef(scratch, "%s.%d", bnm, cnt++);
	proc_strt(scratch, YES, Shell, ShFlags, command, (char *) 0);
}

static SIGRESULT proc_child(int junk)
  	/* JUNK needed for signal handler; not used */
{
	union wait	w;
	register  int	pid;

	for (;;) {
#ifndef WAIT3
		pid = wait2(&w.w_status, (WNOHANG | WUNTRACED));
#else
		pid = wait3(&w, (WNOHANG | WUNTRACED), (struct rusage *) 0);
#endif
		if (pid <= 0)
			break;
		kill_off(pid, w.w_status);
	}
	SIGRETURN;
}

/***********************************************/
void _fastcall kill_off( int pid, union wait w )
/***********************************************/
{
	register  Process  *child;

	if ((child = proc_pid( pid )) == 0)
		return;

	UpdModLine = YES;		/* we're changing state ... */
	if (WIFSTOPPED( w ))
		child->p_state = STOPPED;
	else {
		child->p_state = DEAD;
		if (WIFEXITED(w))
			child->p_howdied = EXITED;
		else if (WIFSIGNALED(w)) {
			child->p_reason = w_termsignum(w);
			child->p_howdied = KILLED;
		}
		{
			Buffer	*save = curbuf;
			char	mesg[128];

			/* insert status message now */
			swritef(mesg, "[Process %s: %s]\n",
				proc_cmd(child),
				pstate(child));
			SetBuf(child->p_buffer);
			ins_str(mesg, NO);
			SetBuf(save);
			redisplay();
		}
	}
}

#endif /* IPROCS */
