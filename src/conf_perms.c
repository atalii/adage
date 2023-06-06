#include <stddef.h>
#include <sys/stat.h>

#define CONF_PATH "/etc/adage.conf"

/* Verify that the permissions on the conf file are okay. If it's 0777, well,
 * adage isn't going to do much in the way of security. We want to make sure
 * that only root can write. Returns -1 if there's an error (caller should
 * check errno), 1 if the config file is okay, and 0 if there's a problem with
 * conf file perms.
 *
 * You might be tempted to rewrite this in Ada, but I don't believe that
 * `struct stat` is bit-for-bit standardized, and sys/stat.h gives it to us for
 * free. */
int
check_conf_perms(void)
{
	int r;
	mode_t perms;
	struct stat info;

	r = stat(CONF_PATH, &info);
	if (r < 0)
		return -1; // caller should check errno
	
	// info.st_mode & 0777 contains the permissions. If we mask out just
	// the world-writable bit, we can compare with 0 to verify that the
	// world can't read the config file.
	perms = info.st_mode & 0002;
	return perms == 0;
}
