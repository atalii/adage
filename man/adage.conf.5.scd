adage.conf(5)

# NAME

adage.conf - configuration file format for *adage*(1)

# SYNTAX

_/etc/adage.conf_ is organized into lines. Each line may be either empty, a
comment, or a rule. Lines must be terminated by only a LF character: DOS
endings are not permitted and will likely lead to confusing error messages.

## CONCEPTS

adage.conf contains rules that permit or reject a user or group member to
authenticate as some user with some options specified. When *adage*(1) is
invoked, it will permit dropping to the specified target if and only if the
drop is both permitted and not denied. Options from each permitting rule will
be combined.

## COMMENTS AND EMPTY LINES

_/etc/adage.conf_ may contain empty lines and comments. Comments look like the
following:

```
# A "#" followed by some text a newline is ignored.
```

Additionally, empty lines immediately terminated by a LF character are ignored.

## RULES

A rule permits or rejects a user or group member to authenticate as a given
target. In the case of granting permission, some options may be specified. 

Lines have the form shown below:

	\[*permit*|*reject*] [u!user|g!group] *as* [user][: [*keepenv*|*nopass*]...]

In prose, they begin either with *permit* or *reject*, then specify a user or a
group with the markings *u!* or *g!* to differentiate the two. A target user,
which may be the wildcard character *\**, is then specified. Optionally, up to
two unique options may be specified:

- *keepenv*: keep the user's environment variables even after dropping.
- *nopass*: do not require a password to verify the user.

# SEE ALSO

*adage*(1)
