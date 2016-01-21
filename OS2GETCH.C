/* keyboard functions for JOVE under OS/2 */
#define INCL_BASE
#include <os2.h>
#include <conio.h>
#define UNIFIED

extern int UpdModLine;
static unsigned char last_char;
KBDKEYINFO KeyInfo;

/***********************************************/
unsigned int _fastcall getrawinchar (void)
/***********************************************/
{
    long wait_time = 2000L;	/* total wait time */
    long interval = 100L;	/* inerval between kbd poll */
    unsigned char new_char;
    unsigned char ret_char;
    int kbdp = 0;
    static unsigned char minutes = 0;
    DATETIME DateTime;

/* return last character */
    if (last_char != 0) {
	new_char = last_char;
	last_char = 0;
	return (new_char);
    }
    DosGetDateTime(&DateTime);
    while (kbhit() == 0) {
	DosSleep(interval);
	if (UpdModLine == 0) {
	    DosGetDateTime(&DateTime);
	    if (DateTime.minutes != minutes) {
		UpdModLine = 1;
		minutes = DateTime.minutes;
	    }
	}
    }
    KbdCharIn(&KeyInfo, IO_WAIT, 0);
//    if (KeyInfo.chChar <= 0xA8)
    if (KeyInfo.chChar <= 0xFF && KeyInfo.chChar != 0xE0)
	return (KeyInfo.chChar);
    else {
	last_char = KeyInfo.chScan;
	return (0x00ff);
    }
}

/*********************************/
int _fastcall rawkey_ready (void)
/*********************************/
{
    if (last_char != 0)
	return (1);
    else
	return (kbhit());
}

/**************************/
void os2_kbd_open(void)
/**************************/
/* initialize keyboard */
{
    KBDINFO kbdInfo;

    kbdInfo.cb = 0x000A;
    KbdGetStatus(&kbdInfo, 0);
    kbdInfo.fsMask &= ~0x0001;	/* not echo on		*/
    kbdInfo.fsMask |= 0x0002;	/* echo off		*/
    kbdInfo.fsMask &= ~0x0008;	/* cooked mode off	*/
    kbdInfo.fsMask |= 0x0004;	/* raw mode		*/
    kbdInfo.fsMask &= ~0x0100;	/* shift report	off	*/
    KbdSetStatus(&kbdInfo, 0);
}



#ifdef UNIFIED
/* function kbd_char unifies the functionality of GETRAWINCHAR, */
/* and RAWKEY_READY */

typedef enum {
    if_pressed_key,
    get_character
} KBD_REQUEST;

/**************************************************************/
int _fastcall kbd_char(KBD_REQUEST request, int *UpdModLine)
/**************************************************************/
{

    long wait_time = 2000L;	/* total wait time */
    long interval = 100L;	/* inerval between kbd poll */
    unsigned char new_char;
    unsigned char ret_char;
    int kbdp = 0;
    static unsigned char minutes = 0;
    DATETIME DateTime;

    switch (request) {
    case (if_pressed_key):
	if (last_char != 0)
	    return (1);
	else
	    return (kbhit());
	break;
    case (get_character):
	if (last_char != 0) {
	    new_char = last_char;
	    last_char = 0;
	    return (new_char);
	}
	DosGetDateTime(&DateTime);
	while (kbhit() == 0) {
	    DosSleep(interval);
	    if (*UpdModLine == 0) {
		DosGetDateTime(&DateTime);
		if (DateTime.minutes != minutes) {
		    *UpdModLine = 1;
		    minutes = DateTime.minutes;
		}
	    }
	}
	KbdCharIn(&KeyInfo, IO_WAIT, 0);
	if (KeyInfo.chChar <= 0xA8)
	    return (KeyInfo.chChar);

	else {
	    last_char = KeyInfo.chScan;
	    return (0x00ff);
	}
    }

}

#endif
