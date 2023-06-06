#!/bin/sh

die() {
	echo "not root, cannot chown/chmod"
	exit 1
}

[ "$(id -u)" = 0 ] || die

chown root:root ./bin/adg
chmod u+s ./bin/adg
