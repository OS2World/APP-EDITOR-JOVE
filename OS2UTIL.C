/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/
#ifdef OS2
#define INCL_BASE
#include <os2.h>
#endif


#define UTIL
#include "jove.h"
#include "ctype.h"
#include "termcap.h"
#include "disp.h"
#include "fp.h"
#include <signal.h>

#ifdef MAC
#include "mac.h"
#else
#ifdef	STDARGS
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#endif

#ifdef MSDOS
#include <time.h>
#endif

const struct cmd *
 FindCmd(proc)
 register  void (*proc) proto((void));
{
    register const struct cmd *cp;

    for (cp = commands; cp->Name; cp++)
	if (cp->c_proc == proc)
	    return cp;
    return 0;
}

int Interactive;		/* True when we invoke with the command handler? */
data_obj *LastCmd;
char *ProcFmt = ": %f ";

/********************************************/
void _fastcall ExecCmd (struct data_obj *cp)
/********************************************/
{
    LastCmd = cp;
    if (cp->Type & MAJOR_MODE) {
	SetMajor((cp->Type >> 8));
    } else if (cp->Type & MINOR_MODE) {
	TogMinor((cp->Type >> 8));
    } else
	switch (cp->Type & TYPEMASK) {
	case MACRO:
	    do_macro((struct macro *) cp);
	    break;

	case FUNCTION:
	    {
		register  struct cmd *cmd = (struct cmd *) cp;

		if (cmd->c_proc) {
		    if ((cmd->Type & MODIFIER) &&
			(BufMinorMode(curbuf, ReadOnly))) {
			rbell();
			message("[Buffer is read-only]");
		    } else
			(*cmd->c_proc) ();
		}
	    }
	}
}

/**********************************/
Line *lastline( register Line *lp )
/**********************************/
{
    register  Line *next;

    while ((next = lp->l_next) != NULL)
	lp = next;
    return lp;
}

char key_strokes[100], *keys_p = key_strokes;

void pp_key_strokes(buffer, size)
char *buffer;
size_t size;
{
    char *buf_end = buffer + size - 5,	/* leave some extra space */
    *kp = key_strokes, c;

    *buffer = '\0';
    while ((c = *kp++) != '\0') {
	swritef(buffer, "%p ", c);
	buffer += strlen(buffer);
	if (buffer > buf_end)
	    break;
    }
}

private int *slowp = 0;		/* for waitchar() */

private SIGRESULT
 slowpoke(junk)
int junk;
{
    char buffer[100];

    if (slowp)
	*slowp = YES;
    pp_key_strokes(buffer, sizeof(buffer));
    f_mess (buffer);
    SIGRETURN;
}

#define N_SEC	1
int in_macro();

/*********************************/
int _fastcall waitchar (int *slow)
/*********************************/
{
    int c;
    long     sleep_time = 1000L;
    long     sleep_interval = 100L;
    int      _fastcall get_ch(void);

    slowp = slow;

    if (in_macro())		/* make macros faster ... */
	return get_ch();

    /* If slow is a valid pointer and it's value is yes, then
       we know we have already been slow during this sequence,
       so we just wait for the character and then echo it. */
    if (slow != 0 && *slow == YES) {
	c = get_ch();
	slowpoke(0);
	return c;
    }
    while (sleep_time >= 0) {
	if (charp() || in_macro())
	    return (get_ch());
	DosSleep(sleep_interval);
	sleep_time -= sleep_interval;
    }
    slowpoke ();
    c = get_ch ();
    slowpoke ();

    return c;
}

/* dir > 0 means forward; else means backward. */

char *
 StrIndex(dir, buf, charpos, what)
int dir;
register  char *buf;
int charpos;
register  int what;
{
    /* register */ char *cp = &buf[charpos];
    /* register */ int c;

    if (dir > 0) {
	while ((c = *cp++) != '\0')
	    if ((c == what) != '\0')
		return (cp - 1);
    } else {
	while (cp >= buf && (c = *cp--) != '\0')
	    if (c == what)
		return (cp + 1);
    }
    return 0;
}

int blnkp(buf)
register  char *buf;
{
    /* register */ char c;

    while ((c = *buf++) != '\0' && (c == ' ' || c == '\t'));
    return c == 0;		/* It's zero if we got to the end of the Line */
}
/***********************/
int within_indent (void)
/***********************/
{
    /* register */ char c;
    /* register */ int i;

    i = curchar;
    while (--i >= 0 && ((c = linebuf[i]) == ' ' || c == '\t'));
    return (i < 0);		/* it's < 0 if we got to the beginning */
}

/***********************************************/
Line  * next_line(Line *line, int num)
/***********************************************/
{
    if (num < 0)
	return prev_line(line, -num);
    if (line)
	while (--num >= 0 && line->l_next != 0)
	    line = line->l_next;
    return line;
}

/************************************************/
Line *  prev_line (Line *line, int num)
/************************************************/
{
    if (num < 0)
	return next_line(line, -num);
    if (line)
	while (--num >= 0 && line->l_prev != 0)
	    line = line->l_prev;
    return line;
}
/*****************************************/
void _fastcall DotTo(Line *line, int col)
/*****************************************/
{
    Bufpos bp;

    bp.p_line = line;
    bp.p_char = col;
    SetDot(&bp);
}

/* If bp->p_line is != current line, then save current line.  Then set dot
   to bp->p_line, and if they weren't equal get that line into linebuf.  */
/***********************************/
void _fastcall SetDot (Bufpos *bp)
/***********************************/
{
    /* register */ int notequal;

    if (bp == 0)
	return;

    notequal = bp->p_line != curline;
    if (notequal)
	lsave();
    if (bp->p_line)
	curline = bp->p_line;
    if (notequal)
	getDOT();
    curchar = bp->p_char;
    if (curchar > length(curline))
	curchar = length(curline);
}
/***************************/
void _fastcall ToLast(void)
/***************************/
{
    SetLine(curbuf->b_last);
    Eol();
}

int MarkThresh = 22;		/* average screen size ... */
static int line_diff;

int LineDist(nextp, endp)
/* register */ Line *nextp, *endp;
{
    (void) inorder(nextp, 0, endp, 0);
    return line_diff;
}

int inorder(nextp, char1, endp, char2)
/* register */ Line *nextp, *endp;
int char1, char2;
{
    int count = 0;
    /* register */ Line *prevp = nextp;

    line_diff = 0;
    if (nextp == endp)
	return char1 < char2;

    while (nextp || prevp) {
	if (nextp == endp || prevp == endp)
	    break;
	if (nextp)
	    nextp = nextp->l_next;
	if (prevp)
	    prevp = prevp->l_prev;
	count += 1;
    }
    if (nextp == 0 && prevp == 0)
	return -1;
    line_diff = count;

    return nextp == endp;
}

void PushPntp(line)
/* register */ Line *line;
{
    if (LineDist(curline, line) >= MarkThresh)
	set_mark();
}
/****************************/
void _fastcall ToFirst(void)
/****************************/
{
    SetLine(curbuf->b_first);
}
/********************************/
int _fastcall length(Line *line)
/********************************/
{
    return strlen(lcontents(line));
}
/***************************/
void _fastcall to_word(dir)
/***************************/
{
    register  char c;

    if (dir == FORWARD) {
	while ((c = linebuf[curchar]) != 0 && !isword(c))
	    curchar += 1;
	if (eolp()) {
	    if (curline->l_next == 0)
		return;
	    SetLine(curline->l_next);
	    to_word(dir);
	    return;
	}
    } else {
	while (!bolp() && (c = linebuf[curchar - 1], !isword(c)))
	    curchar -= 1;
	if (bolp()) {
	    if (curline->l_prev == 0)
		return;
	    SetLine(curline->l_prev);
	    Eol();
	    to_word(dir);
	}
    }
}

/* Are there any modified buffers?  Allp means include B_PROCESS
   buffers in the check. */
/*******************************/
int _fastcall ModBufs(int allp)
/*******************************/
{
    /* register */ Buffer *b;

    for (b = world; b != 0; b = b->b_next) {
	if (b->b_type == B_SCRATCH)
	    continue;
	if ((b->b_type == B_FILE || allp) && IsModified(b))
	    return 1;
    }
    return 0;
}

/***********************************/
char *_fastcall filename(Buffer *b)
/***********************************/
{
    return b->b_fname ? pr_name(b->b_fname, YES) : "[No file]";
}

char *
 itoa(num)
/* register */ int num;
{
    static char line[15];

    swritef(line, "%d", num);
    return line;
}

int min(a, b)
/* register */ int a, b;
{
    return (a < b) ? a : b;
}

int max(a, b)
/* register */ int a, b;
{
    return (a > b) ? a : b;
}

void tiewind(w, bp)
/* register */ Window *w;
/* register */ Buffer *bp;
{
    int not_tied = (w->w_bufp != bp);

    UpdModLine = YES;		/* kludge ... but speeds things up considerably */
    w->w_line = bp->b_dot;
    w->w_char = bp->b_char;
    w->w_bufp = bp;
    if (not_tied)
	CalcWind(w);		/* ah, this has been missing since the
				   beginning of time! */
}

char *
 lcontents(line)
/* register */ Line *line;
{
    if (line == curline)
	return linebuf;
    else
	return lbptr(line);
}

char *
 ltobuf(line, buf)
Line *line;
char *buf;
{
    if (line == curline) {
	if (buf != linebuf)
	    strcpy(buf, linebuf);
	Jr_Len = strlen(linebuf);
    } else
	getline(line->l_dline, buf);
    return buf;
}

void DOTsave(buf)
Bufpos *buf;
{
    buf->p_line = curline;
    buf->p_char = curchar;
}

/* Return none-zero if we had to rearrange the order. */

int fixorder(line1, char1, line2, char2)
/* register */ Line **line1, **line2;
/* register */ int *char1, *char2;
{
    Line *tline;
    int tchar;

    if (inorder(*line1, *char1, *line2, *char2))
	return 0;

    tline = *line1;
    tchar = *char1;
    *line1 = *line2;
    *char1 = *char2;
    *line2 = tline;
    *char2 = tchar;

    return 1;
}

int inlist(first, what)
/* register */ Line *first, *what;
{
    while (first) {
	if (first == what)
	    return 1;
	first = first->l_next;
    }
    return 0;
}

/* Make `buf' (un)modified and tell the redisplay code to update the modeline
   if it will need to be changed. */

int ModCount = 0;

void modify()
{
    if (!curbuf->b_modified) {
	UpdModLine = YES;
	curbuf->b_modified = YES;
    }
    DOLsave = YES;
    if (!Asking)
	ModCount += 1;
}

void unmodify()
{
    if (curbuf->b_modified) {
	UpdModLine = YES;
	curbuf->b_modified = NO;
    }
}

int numcomp(s1, s2)
/* register */ char *s1, *s2;
{
    /* register */ int count = 0;

    while (*s1 != 0 && *s1++ == *s2++)
	count += 1;
    return count;
}

char *
 copystr(str)
char *str;
{
    char *val;

    if (str == 0)
	return 0;
    val = emalloc((size_t) (strlen(str) + 1));

    strcpy(val, str);
    return val;
}

#ifndef byte_copy
void byte_copy(from, to, count)
/* register */ char *from, *to;
/* register */ size_t count;
{
    while (count-- > 0)
	*to++ = *from++;
}

#endif

void len_error(flag)
int flag;
{
    char *mesg = "[line too long]";

    if (flag == COMPLAIN)
	complain(mesg);
    else
	error(mesg);
}

/* Insert num number of c's at offset atchar in a linebuf of LBSIZE */

void ins_c(c, buf, atchar, num, max)
int c;
char *buf;
int atchar, num, max;
{
    /* register */ char *pp, *pp1;
    /* register */ int len;
    int numchars;		/* number of characters to copy forward */

    if (num <= 0)
	return;
    len = atchar + strlen(&buf[atchar]);
    if (len + num >= max)
	len_error(COMPLAIN);
    pp = &buf[len + 1];		/* + 1 so we can --pp (not pp--) */
    pp1 = &buf[len + num + 1];
    numchars = len - atchar;
    while (numchars-- >= 0)
	*--pp1 = *--pp;
    pp = &buf[atchar];
    while (--num >= 0)
	*pp++ = c;
}

int TwoBlank()
{
    /* register */ Line *next = curline->l_next;

    return ((next != 0) &&
	    (*(lcontents(next)) == '\0') &&
	    (next->l_next != 0) &&
	    (*(lcontents(next->l_next)) == '\0'));
}

void linecopy(onto, atchar, from)
/* register */ char *onto, *from;
int atchar;
{
    /* register */ char *endp = &onto[LBSIZE - 2];

    onto += atchar;

    while ((*onto = *from++) != '\0')
	if (onto++ >= endp)
	    len_error(ERROR);
}

char *
 IOerr(err, file)
char *err, *file;
{
    return sprint("Couldn't %s \"%s\".", err, file);
}

#ifdef UNIX
void dopipe(p)
int *p;
{
    if (pipe(p) == -1)
	complain("[Pipe failed]");
}

void pclose(p)
int *p;
{
    (void) close(p[0]);
    (void) close(p[1]);
}

#endif				/* UNIX */

/* NOSTRICT */

char *
 emalloc(size)
size_t size;
{
    /* register */ char *ptr;

    if ((ptr = malloc(size)) != NULL)
	return ptr;
    /* Try garbage collecting lines */
    GCchunks();
    if ((ptr = malloc(size)) != NULL)
	return ptr;
    /* Uh ... Oh screw it! */
    error("[Out of memory] ");
    /* NOTREACHED */
}

/* Return the basename of file F. */

char *
 basename(f)
/* register */ char *f;
{
    /* register */ char *cp;

    if ((cp = strrchr(f, '/')) != NULL)
	return cp + 1;
    else
#ifdef MSDOS
    if (cp = strrchr(f, '\\'))
	return cp + 1;
    else if (cp = strrchr(f, ':'))
	return cp + 1;
#endif				/* MSDOS */
    return f;
}

void push_env(savejmp)
jmp_buf savejmp;
{
    byte_copy((char *) mainjmp, (char *) savejmp, sizeof(jmp_buf));
}

void pop_env(savejmp)
jmp_buf savejmp;
{
    byte_copy((char *) savejmp, (char *) mainjmp, sizeof(jmp_buf));
}

#ifdef LOAD_AV
#if defined(BSD4_2) && !defined(BSD2_10)
#if defined(PURDUE_EE) && (defined(vax) || defined(gould))

void get_la(dp)
double *dp;
{
    *dp = (double) loadav(0) / 100.0;
}

#else				/* !PURDUE_EE || (!vax && !gould) */

#ifdef sun
#include <sys/param.h>
#endif
#include <nlist.h>

static struct nlist nl[] =
{
    {"_avenrun", 0, 0, 0, 0},
#define	X_AVENRUN	0
    {"", 0, 0, 0, 0}
};

void get_la(dp)
double *dp;
{
#ifdef sun
    long avenrun[3];
#else
    double avenrun[3];
#endif
    static int kmem = 0;
    extern long lseek proto((int, long, int));

    if (kmem == -1) {
	*dp = 4.0;		/* So shell commands will say "Chugging" */
	return;
    } else if (kmem == 0) {
	if ((kmem = open("/dev/kmem", 0)) == -1) {
	    f_mess("Can't open kmem for load average.");
	    *dp = 4.0;
	    return;
	}
	nlist("/vmunix", nl);
    }
    lseek(kmem, (long) nl[X_AVENRUN].n_value, 0);
    read(kmem, (char *) avenrun, sizeof(avenrun));
#ifdef sun
    *dp = (double) avenrun[0] / FSCALE;
#else
    *dp = avenrun[0];
#endif
}

#endif
#else				/* !BSD4_2 || BSD2_10 */

void get_la(dp)
double *dp;
{
    short avg[3];

    gldav(avg);
    *dp = (double) avg[0] / 256;
}

#endif
#endif				/* LOAD_AV */

/* get the time buf, designated by *timep, from FROM to TO. */
char *
 get_time(timep, buf, from, to)
time_t *timep;
char *buf;
int from, to;
{
    time_t now;
    char *cp;
    extern char *ctime();

    if (timep != 0)
	now = *timep;
    else
	(void) time(&now);
    cp = ctime(&now) + from;
#ifndef MSDOS
    if (to == -1)
#else				/* MSDOS */
    if ((to == -1) && (cp[strlen(cp) - 1] == '\n'))
#endif				/* MSDOS */
	cp[strlen(cp) - 1] = '\0';	/* Get rid of \n */
    else
	cp[to - from] = '\0';
    if (buf) {
	strcpy(buf, cp);
	return buf;
    } else
	return cp;
}

int casecmp(s1, s2)
/* register */ char *s1, *s2;
{
    if (!s1 || !s2)
	return 1;		/* which is not zero ... */
    while (CharUpcase(*s1) == CharUpcase(*s2++))
	if (*s1++ == '\0')
	    return 0;
    return (*s1 - *--s2);
}

int casencmp(s1, s2, n)
/* register */ char *s1, *s2;
/* register */ size_t n;
{
    if (!s1 || !s2)
	return 1;		/* which is not zero ... */
    for (;;) {
	if (n == 0)
	    return 0;
	n--;
	if (CharUpcase(*s1) != CharUpcase(*s2++))
	    return *s1 - *--s2;
	if (*s1++ == '\0')
	    return 0;
    }
}

void null_ncpy(to, from, n)
char *to, *from;
size_t n;
{
    (void) strncpy(to, from, n);
    to[n] = '\0';
}

/* Tries to pause for delay/10 seconds OR until a character is typed
   at the keyboard.  This works well on BSD4_2 and not so well on the
   rest.  Returns 1 if it returned because of keyboard input, or 0
   otherwise. */

/***********************/
void SitFor (int delay)
/***********************/
{
#include <dos.h>

    long start, end;
    DATETIME DateTime;
    unsigned long sleep_interval = 100L;
    unsigned long alarm_mls = delay * 100;	/* delay in milliseconds */
    unsigned long sleep_time = 0L;

    redisplay();
    do {
	if (InputPending = charp())
	    break;
	sleep_time += sleep_interval;
	DosSleep(sleep_interval);
    }
    while (sleep_time < alarm_mls);
}

int sindex(pattern, string)
/* register */ char *pattern, *string;
{
    /* register */ size_t len = strlen(pattern);

    while (*string != '\0') {
	if (*pattern == *string && strncmp(pattern, string, len) == 0)
	    return TRUE;
	string += 1;
    }
    return FALSE;
}

void make_argv(argv, ap)
/* register */ char *argv[];
va_list ap;
{
    register  char *cp;
    register  int i = 0;

    argv[i++] = va_arg(ap, char *);
    argv[i++] = basename(argv[0]);
    while ((cp = va_arg(ap, char *)) != NULL)
	argv[i++] = cp;
    argv[i] = 0;
}

/******************/
int pnt_line(void)
/******************/
{
    register Line *lp = curbuf->b_first;
    register int i;

    for (i = 0; lp != 0; i++, lp = lp->l_next)
	if (lp == curline)
	    break;
    return i + 1;
}

char *
 ralloc(obj, size)
/* register */ char *obj;
size_t size;
{
    /* register */ char *new;

    if (obj)
	new = realloc(obj, size);
    if (new == 0 || !obj)
	new = emalloc(size);
    return new;
}
