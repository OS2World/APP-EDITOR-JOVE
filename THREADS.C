/* contains OS/2 threads for xjove */
#define INCL_BASE
#include <os2.h>

/*************************************************/
void screen_thread (void)
/*************************************************/
/* A thread for updating the screen every UPD_INTERVAL */
/* milliseconds */
#define UPD_INTERVAL 100L
{
	while (1) {
	    DosSleep (UPD_INTERVAL);
	    DosEnterCritSec ();
	    VioShowBuf (0, 2000, 0);
	    DosExitCritSec ();
	}
}
