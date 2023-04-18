#include <errno.h>
#include <termios.h>
#include <unistd.h>

#define STDIN 0

static int sync_term(void);

static struct {
	struct termios inf;
	int fd;
} term_meta;

int
term_init(void)
{
	int r;
	if (!isatty(STDIN))
		return -1;

	term_meta.fd = STDIN;

	r = tcgetattr(term_meta.fd, &term_meta.inf);

	if (r < 0)
		return -errno;
	else
		return 0;
}

int
term_echo_disable(void)
{
	term_meta.inf.c_lflag &= ~ECHO;
	return sync_term();
}

int
term_echo_enable(void)
{
	term_meta.inf.c_lflag |= ECHO;
	return sync_term();
}

static int
sync_term(void)
{
	return tcsetattr(term_meta.fd, TCSADRAIN, &term_meta.inf);
}
