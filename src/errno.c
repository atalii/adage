#include <errno.h>

int ERRNO_EAGAIN = EAGAIN;
int ERRNO_EINVAL = EINVAL;
int ERRNO_ENOENT = ENOENT;
int ERRNO_EPERM  = EPERM;

int
get_errno(void)
{
	return errno;
}
