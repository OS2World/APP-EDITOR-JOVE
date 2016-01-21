/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/
#define INCL_BASE
#include <os2.h>
#include "jove.h"
#include "fp.h"
#include "ctype.h"
#include "termcap.h"
#include "disp.h"


int AbortCnt, CanScroll = 0, tabstop = 8;

struct scrimage
*DesiredScreen = 0, *PhysScreen = 0;

struct screenline *Screen = 0,	/* the screen (a bunch of screenline) */
*Curline = 0;			/* current line */

static struct screenline *Savelines = 0;	/* another bunch (LI of them) */


static char *cursor;		/* offset into current Line */

char *cursend;

int CapCol, CapLine, i_line, i_col;
extern unsigned char char_per_line;
extern void _near _fastcall normfun(),
 _near _fastcall scr_win(),
 _near _fastcall clr_page(),
 _near _fastcall clr_eoln();

/****************/
void make_scr()
/****************/
{
    /* register */ int i;
    /* register */ struct screenline *ns;
    /* register */ char *nsp;
    DesiredScreen = (struct scrimage *) malloc((unsigned) LI * sizeof(struct scrimage));
    PhysScreen = (struct scrimage *) malloc((unsigned) LI * sizeof(struct scrimage));

    Savelines = (struct screenline *)
	malloc((unsigned) LI * sizeof(struct screenline));
    ns = Screen = (struct screenline *)
	malloc((unsigned) LI * sizeof(struct screenline));

    nsp = (char *) malloc((unsigned) CO * LI);
    if (nsp == 0) {
	writef("\n\rCannot malloc screen!\n");
	finish(1);
    }
    for (i = 0; i < LI; i++) {
	ns->s_line = nsp;
	nsp += CO;
	ns->s_length = nsp - 1;	/* End of Line */
	ns += 1;
    }
    cl_scr(0);
}

/****************************************************/
void clrline(/* register */ char *cp1, /* register */ char *cp2)
/****************************************************/
{
    while (cp1 <= cp2)
	*cp1++ = ' ';
}
int force = 0;

/******************/
void cl_eol(void)
/******************/
{
    void _fastcall Placur(int line, int col);

    if (cursor > cursend)
	return;

    if (cursor < Curline->s_length) {

	Placur(i_line, i_col);
	clr_eoln();

	clrline(cursor, Curline->s_length);

	Curline->s_length = cursor;
    }
}

/*******************/
void cl_scr(int doit)
/*******************/
{
    /* register */ int i;
    /* register */ struct screenline *sp = Screen;

    for (i = 0; i < LI; i++, sp++) {
	clrline(sp->s_line, sp->s_length);
	sp->s_length = sp->s_line;
	PhysScreen[i].s_id = 0;
    }
    if (doit) {
	clr_page();

	CapCol = CapLine = 0;
	UpdMesg = 1;
    }
}

/* Output one character (if necessary) at the current position */

/******************************************************/
int /* only for lints sake */ dosputc(/* register */ int c)
/******************************************************/
{
    void _fastcall Placur(int line, int col);

    if ((force) || (*cursor != c)) {
	if (i_line != CapLine || i_col != CapCol)
	    Placur(i_line, i_col);
	*cursor++ = c;
	normfun((char) c);

	AbortCnt -= 1;
	CapCol += 1;
	i_col += 1;
    } else {
	cursor += 1;
	i_col += 1;
    }
    return (0);			/* useless result */
}

/*
 * Write `line' at the current position of `cursor'.  Stop when we reach the
 * end of the screen.  Aborts if there is a character waiting.
 */
int swrite(line, inversep, abortable)
/* register */ char *line;
int inversep;
/* register */ int abortable;
{
    /* register */ int c;
    int col = i_col, aborted = 0;
    /* register */ int n = cursend - cursor;
    int thebyte;
    force = inversep ? 1 : 0;	/* to force a redraw of the modeline */

    if (n <= 0)
	return 1;
    while ((c = *line++) != '\0') {
	if (abortable && AbortCnt < 0) {
	    AbortCnt = BufSize;
	    if ((InputPending = charp()) != '\0') {
		aborted = 1;
		break;
	    }
	}
	if (c == '\t') {
	    int nchars;

	    nchars = (tabstop - (col % tabstop));
	    col += nchars;
	    while (nchars--) {
		if (--n <= 0)
		    break;
		else
		    dosputc(((' ')));
	    };

	    if (n <= 0)
		break;
	} else if (((CharTable[0][c & (0400 - 1)]) & 020)) {
	    {
		if (--n <= 0)
		    break;
		else
		    dosputc((('^')));
	    };

	    c = ((c == '\177') ? '?' : c + '@');
	    {
		if (--n <= 0)
		    break;
		else
		    dosputc(((c)));
	    };

	    col += 2;
	} else {
	    if (c == 255)
		c = 1;
	    if (c == ' ' && inversep)
		c = 255;
	    {
		if (--n <= 0)
		    break;
		else
		    dosputc(((c)));
	    };

	    col += 1;
	}
    }
    if (n <= 0) {
	if ((*line == '\0') && (c != '\t') && !((CharTable[0][c & (0400 - 1)]) & 020))
	    dosputc((c));
	else
	    dosputc(('!'));
    }
    if (cursor > Curline->s_length)
	Curline->s_length = cursor;

    force = 0;
    return !aborted;
}


/*
 * This is for writing a buffer line to the screen.  This is to minimize the
 * amount of copying from one buffer to another buffer. This gets the info
 * directly from the disk buffers.
 */

int BufSwrite(linenum)
int linenum;
{
    void screen_buffer_flush (void);
    /* register */ int n = cursend - cursor, col = 0, c = -1;
    /* register */ char *bp;
    int StartCol = DesiredScreen[linenum].s_offset, visspace = DesiredScreen[linenum].s_window->w_flags & 04,
     aborted = 0;

    bp = lcontents(DesiredScreen[linenum].s_lp);
    if (*bp)
	for (;;) {
	    if (col >= StartCol) {
		DesiredScreen[linenum].s_offset = col;
		break;
	    }
	    c = *bp++ & (0400 - 1);
	    if (c == '\0')
		break;
	    if (c == '\t')
		col += (tabstop - (col % tabstop));
	    else if (((CharTable[0][c & (0400 - 1)]) & 020))
		col += 2;
	    else
		col += 1;
	}
    if (c != '\0')
	while ((c = *bp++) != '\0') {
	    if (AbortCnt < 0) {
		AbortCnt = BufSize;
		if ((InputPending = charp()) != '\0') {
		    aborted = 1;
		    break;
		}
	    }
	    if (c == '\t') {
		int nchars = (tabstop - (col % tabstop));

		col += nchars;
		if (visspace) {
		    {
			if (--n <= 0)
			    break;
			else
			    dosputc((('>')));
		    };
		    nchars -= 1;
		}
		while (--nchars >= 0) {
		    if (--n <= 0)
			break;
		    else
			dosputc(((' ')));
		};
		if (n <= 0)
		    break;
	    } else if (((CharTable[0][c & (0400 - 1)]) & 020)) {
		{
		    if (--n <= 0)
			break;
		    else
			dosputc((('^')));
		};
		{
		    if (--n <= 0)
			break;
		    else
			dosputc((((c == '\177') ? '?' : c + '@')));
		};
		col += 2;
	    } else {
		if (c == ' ' && visspace)
		    c = '_';

		if (c == 255)
		    c = 1;
		{
		    if (--n <= 0)
			break;
		    else
			dosputc(((c)));
		};
		col += 1;
	    }
	}
    if (n <= 0) {
	if ((*bp == '\0') && (c != '\t') && !((CharTable[0][c & (0400 - 1)]) & 020))
	    dosputc((c));
	else
	    dosputc(('!'));
    }
    if (cursor > Curline->s_length)
	Curline->s_length = cursor;
    //screen_buffer_flush ();	/* DB */
    return !aborted;		/* Didn't abort */
}

void i_set(nline, ncol)
/* register */ int nline, ncol;
{
    Curline = &Screen[nline];
    cursor = Curline->s_line + ncol;
    cursend = &Curline->s_line[CO - 1];
    i_line = nline;
    i_col = ncol;
}

/*
 * Insert `num' lines a top, but leave all the lines BELOW `bottom' alone (at
 * least they won't look any different when we are done). This changes the
 * screen array AND does the physical changes.
 */

void v_ins_line (num, top, bottom)
int num, top, bottom;
{
    /* register */ int i;

    /* Save the screen pointers. */

    for (i = 0; i < num && top + i <= bottom; i++)
	Savelines[i] = Screen[bottom - i];

    /*
     * Num number of bottom lines will be lost. Copy everything down num
     * number of times.
     */

    for (i = bottom; i > top && i - num >= 0; i--)
	Screen[i] = Screen[i - num];

    /* Restore the saved ones, making them blank. */

    for (i = 0; i < num; i++) {
	Screen[top + i] = Savelines[i];
	clrline(Screen[top + i].s_line, Screen[top + i].s_length);
	Screen[top + i].s_length = Screen[top + i].s_line;
    }
    scr_win((int) -num, (unsigned char) top, 0, (unsigned char) bottom, char_per_line - 1);
}

/*
 * Delete `num' lines starting at `top' leaving the lines below `bottom'
 * alone.  This updates the internal image as well as the physical image.
 */

void v_del_line(num, top, bottom)
int num, top, bottom;
{
    /* register */ int i, bot;

    bot = bottom;

    /* Save the lost lines. */

    for (i = 0; i < num && top + i <= bottom; i++)
	Savelines[i] = Screen[top + i];

    /* Copy everything up num number of lines. */

    for (i = top; num + i <= bottom; i++)
	Screen[i] = Screen[i + num];

    /* Restore the lost ones, clearing them. */

    for (i = 0; i < num; i++) {
	Screen[bottom - i] = Savelines[i];
	clrline(Screen[bot].s_line, Screen[bot].s_length);
	Screen[bot].s_length = Screen[bot].s_line;
	bot -= 1;
    }
    scr_win(num, (unsigned char) top, 0, (unsigned char) bottom, char_per_line - 1);
}
