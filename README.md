# adage

ada group and user escalation

> Hopefully this goes without saying, but don't install random SUID binaries
> off the internet. Don't install this on any machine unless you
> *really* know what you're doing.

```sh
# run a command as root
$ adg systemctl list-units
[output snipped]
# or as any other user, in this case, the nas user
$ adg @nas ls /media/nas
[output snipped]
```

## Overview

Adage is a nimble alternative to doas and sudo meant to be simple, safe,
proven, and secure. This simplicity is in usage, configuration, and
function: Adage does as little as it can get away with.

## Usage

Install [alire](https://alire.ada.dev/). Then, run `alr build`. Alternatively,
adage is coming maybe some day to a package manager near you.

```
adg [@user] [cmd...]
```

## Configuration

You probably want this configuration in `/etc/adage.conf`:

```
# Permit users of the wheel group to escalate to root.
permit g!wheel as root

# If you want to give your own user absolute power with no password, add
# this line, too. It's convenient, but carries with it the obvious
# implications.
permit u!your-user as *: nopasswd
```

If this isn't the config you want, see `adage.conf(5)`.


## Status

Adage is unstable, unaudited, and developed on the villainous schedule of my
free time. That said, here's what I'm looking to add or vaguely considering:

+ Comprehensive logging. Errors and escalations should be reported to the
  syslog, or, if no socket is accessible, `/var/log`.

+ Persisted authentication. Both doas and sudo allow authentication to persist
  for a bit so that password prompts don't become too annoying. This probably
  represents a nice security/annoyance compromise, and would be good to support.

+ Configuration validation and escalation dry runs. Should be self-explanatory,
  would be a nice QoL feature. (Validation implemented in fff516, dry runs
  still TODO.)

+ PAM. Loading C dynlibs and asking them nicely to authenticate a user isn't a
  great strategy, but it is the current standard. Support with some sandboxing
  may have some utility, but I'm not etnhusiastic about it.

+ More verification. Right now, components are fairly well-coupled, making it
  hard to extract pure functions to verify. As a result, Adage is maybe a bit
  too light on SPARK.
