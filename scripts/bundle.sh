#!/bin/sh

die() {
	echo $?
	exit 1
}

alr build --release || die "build failed"

mkdir -p pkg

cp ./bin/adg ./pkg/adg || die "adg copy failed"

scdoc < ./man/adage.1.scd > ./pkg/adage.1 || die "adage.1.scd failed"
scdoc < ./man/adage.conf.5.scd > ./pkg/adage.conf.5 || die "adage.conf.5.scd failed"
