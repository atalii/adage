#include <errno.h>

int ERRNO_ENOENT = ENOENT;

int
get_errno(void)
{
	return errno;
}
