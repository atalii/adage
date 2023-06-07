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
	mode_t writables;
	struct stat info;

	r = stat(CONF_PATH, &info);
	if (r < 0)
		return -1; // caller should check errno
	
	// info.st_mode & 0777 contains the permissions. Let's mask out just
	// writability perms; we don't actually care about anything else.
	writables = info.st_mode & 0222;

	// If the file isn't owned by root, mask 0200 bit if and only if it
	// isn't writable. Then, do the same for the owning group.
	writables &= ((info.st_uid != 0) << 7) | ((info.st_gid != 0) << 4);

	// After all that, writeables has filtered out all the acceptable
	// writable permissions. Anything left is a security risk.
	return writables == 0;
}
