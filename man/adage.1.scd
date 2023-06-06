adage(1)

# NAME

adg - drop and escalate privileges

# SYNOPSIS

*adg* --help++
*adg* [@user] [cmd...]

# DESCRIPTION

Adage runs as an SUID binary and allows one to execute commands as another user,
including as root, given appropriate authorization from the config file at
_/etc/adage.conf_. Adage takes two potential arguments; first, a target user to
drop to, and a command to run. Any or both may be omitted. In absence of a
specified target user, root is assumed. In the absence of a command to run, the
target user's shell is determined from the *passwd* file. Thus, the minimal
invocation of Adage is simply *adg*.

To specify a non-root user to drop to, give the first argument to *adg* in the
form of *@user*; that is, the username following an ampersand. Following
arguments, or, in the case that no user has been specified, all arguments are
interpreted as the command to run. Therefore, one might invoke the following to
temporarily drop to the *nas* user and *touch*(1) a file in _/media/nas_:

	adg @nas touch /media/nas/example-file

# CONFIGURATION

Adage reads _/etc/adage.conf_ to determine what actions are permitted. This may
be world-readable, but must be restricted to root-writable.

# SEE ALSO

*adage.conf*(5)
