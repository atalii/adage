# adage

ada group and user escalation

> This README is somewhat aspirational. Lots of it does work, but it's
> unpolished and undocumented. Also, SPARK verification and options aren't
> implemented yet. Don't use Adage in production - or development.

```sh
# run a command as root
$ adg systemctl list-units
[output snipped]
# or as any other user, in this case, the nas user
$ adg @nas ls /media/nas
[output snipped]
```

## Overview

Adage is an alternative to doas and sudo, with a special focus on being small
and safe: It's written in a mix of safe Ada and SPARK, where core
authentication logic is formally verified, and everything else is free from
memory safety bugs.

> Be warned: I'm a dumbass on the internet. I think this is secure,
> but, you know, maybe look it over first.

## Configuration

Adage and it's conf language are very simple. You need no more than the
following:

```
# Permit my-user to authenticate as any user with a password check.
permit u!my-user  as *: keepenv

# Permit my-user to run poweroff without any password.
permit u!my-user  as root for "poweroff": nopasswd

# Reject my-group to authenticate as root.
reject g!my-group as root
```

Every line looks like: `permit/reject actor as role: options`. No rule is more
than a line. An adage invocation succeeds if rules permit it and don't refuse
it. In the case that multiple permit rules match, options from all of them are
combined.

Here's the config you probably want:

```
permit g!wheel as root
```

Or, the following will let your user move across users without a
password:

```
permit u!my-user as *: nopass
```

As for options, we've got quite the small subset I stole from doas:
keepenv (pass in environment variables) and nopass (don't require
password authentication).

The one gotcha is that we borrow from minecraft fanfictions in the
actor syntax. To specify a user, we prepend the name with `u!`, and to
specify a group, we prepend the name with `g!`.

For more complete documentation, consult `adage.conf(5)`.

## Usage

```
usage: adg [@user] cmd

If no @user is specified, adg will default to root.
If no cmd is specified, the target user's shell is spawned.
```

## TODOs

+ [ ] Comprehensive logging
+ [ ] PAM?
+ [ ] A persist option
+ [ ] Conf file validation
