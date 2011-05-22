#include <stdlib.h>

/* The Fasm backend can't handle return statements, so we
 * jump in the cmm test file to this function to return
 */
void c_return(void)
{
	return;
}

