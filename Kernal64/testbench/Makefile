
include Makefile.config

.SILENT:

CC=gcc

all: prereq
	echo "to run the tests use the following targets:"
	echo "testall           - run all VICE tests"
	echo "testx64"
	echo "testx64sc"
	echo "testx128c64"
	echo "testx128"
	echo "testxscpu64"
	echo "testx64dtv"
	echo "testxpet"
	echo "testxcbm2"
	echo "testxcbm5x0"
	echo "testxvic"
	echo "testxplus4"
	echo "testvsid"
	echo "testchameleon"
	echo "testcham20"
#	echo "testhoxs64"
#	echo "testmicro64"
#	echo "testemu64"
#	echo "testyace"

################################################################################
prereq: selftest testlist cmpscreens maketable 
#prereq-chameleon

.PHONY: testlist selftest

selftest:
	make -C selftest

cmpscreens: cmpscreens.c
	$(CC) -W -Wall -O3 -o cmpscreens cmpscreens.c -lm

maketable: maketable.c
	$(CC) -W -Wall -O3 -o maketable maketable.c -lm

################################################################################

testlist: \
	x64-testlist.txt \
	x64sc-testlist.txt \
	x128c64-testlist.txt \
	chameleon-testlist.txt \
	u64-testlist.txt \
	c64rmk2-testlist.txt \
	hoxs64-testlist.txt \
	micro64-testlist.txt \
	emu64-testlist.txt \
	yace-testlist.txt \
	x128-testlist.txt \
	xscpu64-testlist.txt \
	x64dtv-testlist.txt \
	xvic-testlist.txt \
	cham20-testlist.txt \
	xplus4-testlist.txt \
	xpet-testlist.txt \
	xcbm2-testlist.txt \
	xcbm5x0-testlist.txt \
	vsid-testlist.txt \
	z64kc64-testlist.txt \
	z64kc128-testlist.txt \
	z64kc128c64-testlist.txt \
	z64kvic20-testlist.txt \
	denise-testlist.txt \
	kernal64c64-testlist.txt \
    kernal64c128c64-testlist.txt \
    kernal64c128c128-testlist.txt

kernal64c64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit!." >  kernal64c64-testlist.txt
	 cat c64-testlist.in \
        | grep -v "ntsc" \
        | grep -v ",vicii-drean" \
        | grep -v ",plus60k" \
        | grep -v ",plus256k" \
        | grep -v ",isepic" \
        | grep -v ",dqbb" \
        | grep -v ",ramcart128k" \
        | grep -v "ultimax" \
    >> kernal64c64-testlist.txt

kernal64c128c64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit!." >  kernal64c128c64-testlist.txt
	 cat c64-testlist.in \
        | grep -v "ntsc" \
        | grep -v ",vicii-drean" \
        | grep -v ",plus60k" \
        | grep -v ",plus256k" \
        | grep -v ",isepic" \
        | grep -v ",dqbb" \
        | grep -v ",ramcart128k" \
        | grep -v "ultimax" \
    >> kernal64c128c64-testlist.txt

kernal64c128c128-testlist.txt: c128-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  kernal64c128c128-testlist.txt
	cat  c128-testlist.in \
        | grep -v "ntsc" \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",efnram" \
		| grep -v ",ifnram" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> kernal64c128c128-testlist.txt

x64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  x64-testlist.txt
	cat  c64-testlist.in >> x64-testlist.txt

x64sc-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  x64sc-testlist.txt
	cat  c64-testlist.in >> x64sc-testlist.txt

x128c64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  x128c64-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		>> x128c64-testlist.txt

chameleon-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  chameleon-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",mountp64" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> chameleon-testlist.txt

# FIXME
u64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  u64-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",mountd64" \
		| grep -v ",mountcrt" \
		| grep -v ",mountg64" \
		| grep -v ",cia-new" \
		| grep -v ",vicii-ntsc" \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> u64-testlist.txt

c64rmk2-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  c64rmk2-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",screenshot" \
		| grep -v ",vicii-drean" \
		| grep -v ",geo256k" \
		| grep -v ",reu512k" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		| grep -v ".d64" \
		| grep -v ".d81" \
		| grep -v ".g64" \
		| grep -v ".crt" \
		>> c64rmk2-testlist.txt

micro64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  micro64-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",vicii-ntsc" \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> micro64-testlist.txt

# The official cartridge file support list is:
# Action Reply
# Retro Reply
# Easy Flash (1MB)
# Ocean
# System 3
# Dinamic
# Magic Desk
# Super Games
# Zaxxon
# Fun Play
# Simons Basic
# 
# Simple 8K/16K and Ultimax cartridges should work also.
hoxs64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  hoxs64-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",vicii-ntsc" \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",sid-old" \
		| grep -v ",reu512k" \
		| grep -v ",geo256k" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> hoxs64-testlist.txt

emu64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  emu64-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",vicii-ntsc" \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",cia-new" \
		| grep -v ",sid-new" \
		| grep -v ",reu512k" \
		| grep -v ",geo256k" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> emu64-testlist.txt

		
denise-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  denise-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",mountp64" \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",geo256k" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> denise-testlist.txt

#		| grep -v ",screenshot" \
		
yace-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  yace-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",cia-new" \
		| grep -v ",sid-new" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> yace-testlist.txt
		
z64kc64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  z64kc64-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",geo256k" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> z64kc64-testlist.txt
		
z64kc128c64-testlist.txt: c64-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  z64kc128c64-testlist.txt
	cat  c64-testlist.in \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",geo256k" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> z64kc128c64-testlist.txt
		
x128-testlist.txt: c128-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  x128-testlist.txt
	cat  c128-testlist.in >> x128-testlist.txt

z64kc128-testlist.txt: c128-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  z64kc128-testlist.txt
	cat  c128-testlist.in \
		| grep -v ",vicii-ntscold" \
		| grep -v ",vicii-drean" \
		| grep -v ",efnram" \
		| grep -v ",ifnram" \
		| grep -v ",geo256k" \
		| grep -v ",plus60k" \
		| grep -v ",plus256k" \
		| grep -v ",isepic" \
		| grep -v ",dqbb" \
		| grep -v ",ramcart128k" \
		>> z64kc128-testlist.txt
	
xscpu64-testlist.txt: scpu-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  xscpu64-testlist.txt
	cat  scpu-testlist.in >> xscpu64-testlist.txt

x64dtv-testlist.txt: dtv-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  x64dtv-testlist.txt
	cat  dtv-testlist.in >> x64dtv-testlist.txt

xpet-testlist.txt: pet-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  xpet-testlist.txt
	cat  pet-testlist.in >> xpet-testlist.txt

xcbm2-testlist.txt: cbm610-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  xcbm2-testlist.txt
	cat  cbm610-testlist.in >> xcbm2-testlist.txt

xcbm5x0-testlist.txt: cbm510-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  xcbm5x0-testlist.txt
	cat  cbm510-testlist.in >> xcbm5x0-testlist.txt

xvic-testlist.txt: vic20-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  xvic-testlist.txt
	cat  vic20-testlist.in >> xvic-testlist.txt

z64kvic20-testlist.txt: vic20-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  z64kvic20-testlist.txt
	cat  vic20-testlist.in \
		| grep -v ",geo256k" \
		>> z64kvic20-testlist.txt

cham20-testlist.txt: vic20-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  cham20-testlist.txt
	cat  vic20-testlist.in \
		| grep -v ",geo256k" \
		>> cham20-testlist.txt

xplus4-testlist.txt: plus4-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  xplus4-testlist.txt
	cat  plus4-testlist.in >> xplus4-testlist.txt

vsid-testlist.txt: vsid-testlist.in Makefile
	echo "# this is a generated file, do not edit." >  vsid-testlist.txt
	cat  vsid-testlist.in >> vsid-testlist.txt

################################################################################

testall: \
	testx64 \
	testx64sc \
	testx128c64 \
	testx128 \
	testxscpu64 \
	testx64dtv \
	testxplus4 \
	testxvic \
	testxpet \
	testxcbm2 \
	testxcbm5x0 \
	testvsid

testx64: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh x64

testx64sc: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh x64sc

testx128c64: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh x128c64

testx128: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh x128

testxscpu64: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh xscpu64

testx64dtv: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh x64dtv

testxplus4: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh xplus4

testxvic: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh xvic

testxpet: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh xpet

testxcbm2: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh xcbm2

testxcbm5x0: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh xcbm5x0

testvsid: prereq
	EMUDIR="$(VICEDIR)" ./testbench.sh vsid

##############################################################################
	
testhoxs64:	
	EMUDIR="$(HOXS64DIR)" ./testbench.sh hoxs64 --pal --cianew --8580 --8565early
testmicro64:
	EMUDIR="$(MICRO64DIR)" ./testbench.sh micro64

testz64k: testz64kc64 testz64kc128c64 testz64kc128 testz64kvic20
testz64kc64:	
	EMUDIR="$(Z64KDIR)" ./testbench.sh z64kc64
testz64kc128c64:	
	EMUDIR="$(Z64KDIR)" ./testbench.sh z64kc128c64
testz64kc128:	
	EMUDIR="$(Z64KDIR)" ./testbench.sh z64kc128
testz64kvic20:
	EMUDIR="$(Z64KDIR)" ./testbench.sh z64kvic20

testyace:
#	EMUDIR="$(YACEDIR)" ./testbench.sh yace --verbose
	EMUDIR="$(YACEDIR)" ./testbench.sh yace
testdenise:
#	EMUDIR="$(DENISEDIR)" ./testbench.sh denise --resume
	EMUDIR="$(DENISEDIR)" ./testbench.sh denise
testemu64:
#	EMUDIR="$(EMU64DIR)" ./testbench.sh emu64 --verbose
	EMUDIR="$(EMU64DIR)" ./testbench.sh emu64 --resume

testkernal64c64:
	EMUDIR="${KERNAL64DIR}" ./testbench.sh kernal64c64 --verbose --pal
testkernal64c128c64:
	EMUDIR="${KERNAL64DIR}" ./testbench.sh kernal64c128c64 --verbose --pal
testkernal64c128c128:
	EMUDIR="${KERNAL64DIR}" ./testbench.sh kernal64c128c128 --verbose --pal
	
##############################################################################

prereq-chameleon: chameleon-helper.prg chameleon-crtoff.prg chameleon-videomode.prg

chameleon-helper.prg: chameleon-helper.asm
	acme -f cbm -o chameleon-helper.prg chameleon-helper.asm

chameleon-videomode.prg: chameleon-videomode.asm
	acme -f cbm -o chameleon-videomode.prg chameleon-videomode.asm

chameleon-crtoff.prg: chameleon-crtoff.asm
	acme -f plain -o chameleon-crtoff.prg chameleon-crtoff.asm

testchameleon: chameleon-helper.prg prereq
	./testbench.sh chameleon --8565early
# for testing in cartridge mode on "new" c64 / PAL
testchameleoncart: chameleon-helper.prg prereq
	./testbench.sh chameleon --pal --8565early --cianew --8580

testcham20: chameleon-helper.prg prereq
	./testbench.sh cham20 --8k

##############################################################################
# OLD cia
# 6581 bus interface with 8580 alike sounds.
# VIC 6567 and 6569, no gray dots.	
testu64: prereq
	./testbench.sh u64 --resume --ciaold --6581 --pal

################################################################################
# the following must be manually updated to use the right files
################################################################################

# VICE 3.3 -> r35872

.PHONY: results0 results resultswiki

# missing:
#          vsid
#	   yace, emu64

results0: maketable
	./maketable --list c64-testlist.in \
		--results results/x64sc-r37145-result.txt "x64sc r37145" \
		--results results/x64-r37109-result.txt "x64 r37109" \
		--results results/x128c64-r37109-result.txt "x128 (c64) r37109" \
		--results results/hoxs64-1-0-21-2-result.txt "hoxs64 1.0.21.2" \
		--results results/z64kc64-1_2-result.txt "z64k (c64) 1.2" \
		--results results/z64kc128c64-1_2-result.txt "z64k (c128/c64) 1.2" \
		--results results/micro64-20180314-result.txt "micro64 20180314" \
		--results results/denise-20200110-result.txt "denise 20200110" \
		--results results/chameleon-9i-cartridge-result.txt "TC64 9i (cartridge)" \
		--results results/chameleon-9i-standalone-result.txt "TC64 9i (standalone)" \
		--results results/u64-124-pal-result.txt "U64 1.24 (PAL)"

	./maketable --list vic20-testlist.in \
		--results results/xvic-r37109-result.txt "xvic r37109" \
		--results results/z64kvic20-1_2-result.txt "z64k (vic20) 1.2" \
		--results results/cham20-20150818-result.txt "cham20 20150818"

	./maketable --list c128-testlist.in \
		--results results/x128-r37166-result.txt "x128 r37166" \
		--results results/z64kc128-1_2-result.txt "z64k (c128) 1.2"

	./maketable --list plus4-testlist.in \
		--results results/xplus4-r37109-result.txt "xplus4 r37109"

	./maketable --list pet-testlist.in \
		--results results/xpet-r37109-result.txt "xpet r37109"

	./maketable --list dtv-testlist.in \
		--results results/x64dtv-r37109-result.txt "x64dtv r37109"

	./maketable --list scpu-testlist.in \
		--results results/xscpu64-r37109-result.txt "xscpu64 r37109"
		
	./maketable --list cbm510-testlist.in \
		--results results/xcbm5x0-r37109-result.txt "xcbm5x0 r37109"
		
	./maketable --list cbm610-testlist.in \
		--results results/xcbm2-r37109-result.txt "xcbm2 r37109"
		
		
results: maketable
	./maketable --html --list c64-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r37145-result.txt "x64sc r37145" \
		--results results/x64-r37109-result.txt "x64 r37109" \
		--results results/x128c64-r37109-result.txt "x128 (c64) r37109" \
		--results results/hoxs64-1-0-21-2-result.txt "hoxs64 1.0.21.2" \
		--results results/z64kc64-1_2-result.txt "z64k (c64) 1.2" \
		--results results/z64kc128c64-1_2-result.txt "z64k (c128/c64) 1.2" \
		--results results/micro64-20180314-result.txt "micro64 20180314" \
		--results results/denise-20200110-result.txt "denise 20200110" \
		--results results/emu64-20191025-result.txt "emu64 20191025" \
		--results results/yace-2-2-0-47-result.txt "yace 2.2.0.47" \
		--results results/chameleon-9i-cartridge-result.txt "TC64 9i (cartridge)" \
		--results results/chameleon-9i-standalone-result.txt "TC64 9i (standalone)" \
		--results results/u64-124-pal-result.txt "U64 1.24 (PAL)" \
		\
		> results/c64.html
		
	./maketable  --html --list vic20-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xvic-r37109-result.txt "xvic r37109" \
		--results results/z64kvic20-1_2-result.txt "z64k (vic20) 1.2" \
		--results results/cham20-20150818-result.txt "cham20 20150818" \
		\
		> results/vic20.html
		
	./maketable  --html --list c128-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x128-r37166-result.txt "x128 r37166" \
		--results results/z64kc128-1_2-result.txt "z64k (c128) 1.2" \
		\
		> results/c128.html

	./maketable  --html --list plus4-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xplus4-r37109-result.txt "xplus4 r37109" \
		\
		> results/plus4.html

	./maketable  --html --list pet-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xpet-r37109-result.txt "xpet r37109" \
		\
		> results/pet.html

	./maketable  --html --list dtv-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64dtv-r37109-result.txt "x64dtv r37109" \
		\
		> results/dtv.html

	./maketable  --html --list scpu-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xscpu64-r37109-result.txt "xscpu64 r37109" \
		\
		> results/scpu.html
		
	./maketable  --html --list cbm510-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xcbm5x0-r37109-result.txt "xcbm5x0 r37109" \
		\
		> results/cbm5x0.html
		
	./maketable  --html --list cbm610-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xcbm2-r37109-result.txt "xcbm2 r37109" \
		\
		> results/cbm2.html
		
	./maketable  --html --list vsid-testlist.in \
		--ranking "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/vsid-r37109-result.txt "vsid r37109" \
		\
		> results/sid.html
		
# use with something like "make resultswiki | xclip -selection clipboard"
		
resultswiki: maketable
	@echo "== Results =="
	@echo "=== C64 ==="
	./maketable  --wiki --list c64-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/x64sc-r37145-result.txt "x64sc r37145" \
		--results results/x64-r37109-result.txt "x64 r37109" \
		--results results/x128c64-r37109-result.txt "x128 (c64) r37109" \
		--results results/hoxs64-1-0-21-2-result.txt "hoxs64 1.0.21.2" \
		--results results/z64kc64-1_2-result.txt "z64k (c64) 1.2" \
		--results results/z64kc128c64-1_2-result.txt "z64k (c128/c64) 1.2" \
		--results results/micro64-20180314-result.txt "micro64 20180314" \
		--results results/denise-20200110-result.txt "denise 20200110" \
		--results results/emu64-20191025-result.txt "emu64 20191025" \
		--results results/yace-2-2-0-47-result.txt "yace 2.2.0.47" \
		--results results/chameleon-9i-cartridge-result.txt "TC64 9i (cartridge)" \
		--results results/chameleon-9i-standalone-result.txt "TC64 9i (standalone)" \
		--results results/u64-124-pal-result.txt "U64 1.24 (PAL)"
	@echo "=== VIC20 ==="
	./maketable  --wiki --list vic20-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/xvic-r37109-result.txt "xvic r37109" \
		--results results/z64kvic20-1_2-result.txt "z64k (vic20) 1.2" \
		--results results/cham20-20150818-result.txt "cham20 20150818"
	@echo "=== C128 ==="
	./maketable  --wiki --list c128-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/x128-r37166-result.txt "x128 r37166" \
		--results results/z64kc128-1_2-result.txt "z64k (c128) 1.2"
	@echo "=== PLUS4 ==="
	./maketable  --wiki --list plus4-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/xplus4-r37109-result.txt "xplus4 r37109"
	@echo "=== PET ==="
	./maketable  --wiki --list pet-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/xpet-r37109-result.txt "xpet r37109"
	@echo "=== DTV ==="
	./maketable  --wiki --list dtv-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/x64dtv-r37109-result.txt "x64dtv r37109"
	@echo "=== SCPU ==="
	./maketable  --wiki --list scpu-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/xscpu64-r37109-result.txt "xscpu64 r37109"
	@echo "=== CBM5x0 ==="
	./maketable  --wiki --list cbm510-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/xcbm5x0-r37109-result.txt "xcbm5x0 r37109"
	@echo "=== CBM2 ==="
	./maketable  --wiki --list cbm610-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/xcbm2-r37109-result.txt "xcbm2 r37109"
	@echo "=== SID ==="
	./maketable  --wiki --list vsid-testlist.in \
		--ranking "{{Progress|%d|%d|%d|}}" \
		--results results/vsid-r37109-result.txt "vsid r37109"

################################################################################

.PHONY: resultsx64
resultsx64: maketable
	./maketable --html --list c64-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64-r35872-result.txt "x64 r35872" \
		--results results/x64-r37109-result.txt "x64 r37109" \
		\
		> results/x64.html

.PHONY: resultsx64sc
resultsx64sc: maketable
	./maketable --html --list c64-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r35872-result.txt "x64sc r35872" \
		--results results/x64sc-r36320-pal-result.txt "x64sc r36320 PAL" \
		--results results/x64sc-r36320-ntsc-result.txt "x64sc r36320 NTSC" \
		--results results/x64sc-r36351-result.txt "x64sc r36351" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--results results/x64sc-r37109-result.txt "x64sc r37109" \
		--results results/x64sc-r37145-result.txt "x64sc r37145" \
		\
		> results/x64sc.html

.PHONY: resultsx128
resultsx128: maketable
	./maketable --html --list c128-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x128-r35872-result.txt "x128 r35872" \
		--results results/x128-r35990-result.txt "x128 r35990" \
		--results results/x128-r36014-result.txt "x128 r36014" \
		--results results/x128-r36700-result.txt "x128 r36700" \
		--results results/x128-r37109-result.txt "x128 r37109" \
		--results results/x128-r37166-result.txt "x128 r37166" \
		\
		> results/x128.html

.PHONY: resultsx128c64
resultsx128c64: maketable
	./maketable --html --list c64-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x128c64-r35872-result.txt "x128c64 r35872" \
		--results results/x128c64-r35968-result.txt "x128c64 r35968" \
		--results results/x128c64-r36700-result.txt "x128c64 r36700" \
		--results results/x128c64-r37109-result.txt "x128c64 r37109" \
		\
		> results/x128c64.html

.PHONY: resultsxcbm2
resultsxcbm2: maketable
	./maketable --html --list cbm610-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xcbm2-r35872-result.txt "xcbm2 r35872" \
		--results results/xcbm2-r35990-result.txt "xcbm2 r35990" \
		--results results/xcbm2-r36700-result.txt "xcbm2 r36700" \
		--results results/xcbm2-r37109-result.txt "xcbm2 r37109" \
		\
		> results/xcbm2.html

.PHONY: resultsxcbm5x0
resultsxcbm5x0: maketable
	./maketable --html --list cbm510-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xcbm5x0-r35872-result.txt "xcbm5x0 r35872" \
		--results results/xcbm5x0-r35990-result.txt "xcbm5x0 r35990" \
		--results results/xcbm5x0-r36700-result.txt "xcbm5x0 r36700" \
		--results results/xcbm5x0-r37109-result.txt "xcbm5x0 r37109" \
		\
		> results/xcbm5x0.html

.PHONY: resultsx64dtv
resultsx64dtv: maketable
	./maketable --html --list dtv-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64dtv-r35872-result.txt "x64dtv r35872" \
		--results results/x64dtv-r35990-result.txt "x64dtv r35990" \
		--results results/x64dtv-r36700-result.txt "x64dtv r36700" \
		--results results/x64dtv-r37109-result.txt "x64dtv r37109" \
		\
		> results/x64dtv.html

.PHONY: resultsxpet
resultsxpet: maketable
	./maketable --html --list pet-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xpet-r35872-result.txt "xpet r35872" \
		--results results/xpet-r35990-result.txt "xpet r35990" \
		--results results/xpet-r36700-result.txt "xpet r36700" \
		--results results/xpet-r37109-result.txt "xpet r37109" \
		\
		> results/xpet.html

.PHONY: resultsxplus4
resultsxplus4: maketable
	./maketable --html --list plus4-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xplus4-r35872-result.txt "xplus4 r35872" \
		--results results/xplus4-r35990-result.txt "xplus4 r35990" \
		--results results/xplus4-r36700-result.txt "xplus4 r36700" \
		--results results/xplus4-r37109-result.txt "xplus4 r37109" \
		\
		> results/xplus4.html

.PHONY: resultsxscpu64
resultsxscpu64: maketable
	./maketable --html --list scpu-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xscpu64-r35872-result.txt "xscpu64 r35872" \
		--results results/xscpu64-r35990-result.txt "xscpu64 r35990" \
		--results results/xscpu64-r36700-result.txt "xscpu64 r36700" \
		--results results/xscpu64-r37109-result.txt "xscpu64 r37109" \
		\
		> results/xscpu64.html

.PHONY: resultsxvic
resultsxvic: maketable
	./maketable --html --list vic20-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xvic-r35872-result.txt "xvic r35872" \
		--results results/xvic-r35990-result.txt "xvic r35990" \
		--results results/xvic-r36700-result.txt "xvic r36700" \
		--results results/xvic-r37109-result.txt "xvic r37109" \
		\
		> results/xvic.html

.PHONY: resultsvsid
resultsvsid: maketable
	./maketable --html --list vsid-testlist.in \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/vsid-r35990-result.txt "vsid r35990" \
		--results results/vsid-r36700-result.txt "vsid r36700" \
		--results results/vsid-r37109-result.txt "vsid r37109" \
		\
		> results/vsid.html

################################################################################

#results/emu64-result.txt

.PHONY: resultskernal64c64
resultskernal64c64: maketable kernal64c64-testlist.txt
	./maketable --html --list kernal64c64-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/kernal64c64-result.txt "Kernal64 1.6.0" \
		\
		> results/kernal64c64.html

.PHONY: resultskernal64c128c64
resultskernal64c128c64: maketable kernal64c128c64-testlist.txt
	./maketable --html --list kernal64c128c64-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
        --results results/kernal64c64-result.txt "Kernal64 64 1.6.0" \
		--results results/kernal64c128c64-result.txt "Kernal64 128/64 1.6.0" \
		\
		> results/kernal64c128c64.html

.PHONY: resultskernal64c128c128
resultskernal64c128c128: maketable kernal64c128c128-testlist.txt
	./maketable --html --list kernal64c128c128-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
        --results results/kernal64c128c128-result.txt "Kernal64 128 1.6.0" \
		\
		> results/kernal64c128c128.html

.PHONY: resultsz64kc64
resultsz64kc64: maketable z64kc64-testlist.txt
	./maketable --html --list z64kc64-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/z64kc64-20181202-result.txt "z64kc64 20181202" \
		--results results/z64kc64-1_2-result.txt "z64kc64 1.2" \
		\
		> results/z64kc64.html

.PHONY: resultsz64kc128
resultsz64kc128: maketable z64kc128-testlist.txt
	./maketable --html --list z64kc128-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/z64kc128-20181202-result.txt "z64kc128 20181202" \
		--results results/z64kc128-1_2-result.txt "z64kc128 1.2" \
		\
		> results/z64kc128.html

.PHONY: resultsz64kc128c64
resultsz64kc128c64: maketable z64kc128c64-testlist.txt
	./maketable --html --list z64kc128c64-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/z64kc128c64-20181202-result.txt "z64kc128c64 20181202" \
		--results results/z64kc128c64-1_2-result.txt "z64kc128c64 1.2" \
		\
		> results/z64kc128c64.html

.PHONY: resultsz64kvic20
resultsz64kvic20: maketable z64kvic20-testlist.txt
	./maketable --html --list z64kvic20-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xvic-r36700-result.txt "xvic r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/z64kvic20-20181202-result.txt "z64kvic20 20181202" \
		--results results/z64kvic20-1_2-result.txt "z64kvic20 1.2" \
		\
		> results/z64kvic20.html

		
.PHONY: resultshoxs64
resultshoxs64: maketable hoxs64-testlist.txt
	./maketable --html --list hoxs64-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/hoxs64-1-0-16-0-result.txt "hoxs64 1.0.16.0" \
		--results results/hoxs64-1-0-21-0-result.txt "hoxs64 1.0.21.0" \
		--results results/hoxs64-1-0-21-2-result.txt "hoxs64 1.0.21.2" \
		\
		> results/hoxs64.html

.PHONY: resultsdenise
resultsdenise: maketable denise-testlist.txt
	./maketable --html --list denise-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/denise-1-0-5-result.txt "denise 1.0.5" \
		--results results/denise-20191012-result.txt "denise 20191012" \
		--results results/denise-20200110-result.txt "denise 20200110" \
		\
		> results/denise.html
		
.PHONY: resultsmicro64
resultsmicro64: maketable micro64-testlist.txt
	./maketable --html --list micro64-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/micro64-20180314-result.txt "micro64 20180314" \
		\
		> results/micro64.html
		
.PHONY: resultsyace
resultsyace: maketable yace-testlist.txt
	./maketable --html --list yace-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/yace-2-2-0-47-result.txt "yace 2.2.0.47" \
		\
		> results/yace.html
		
.PHONY: resultsemu64
resultsemu64: maketable emu64-testlist.txt
	./maketable --html --list emu64-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--results results/emu64-20191025-result.txt "emu64 20191025" \
		\
		> results/emu64.html
		
################################################################################
		
.PHONY: resultscham
resultscham: maketable chameleon-testlist.txt 
	./maketable --filter-ntscold --html --list chameleon-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--warnvicefail "<small><b>fails on VICE</b></small>" \
		--warnvicfetch "<small><b>VIC may be wrong!</b></small>" \
		--results results/chameleon-9h-result.txt "TC64 9h (standalone)" \
		--results results/chameleon-9i-cartridge-result.txt "TC64 9i (cartridge)" \
		--results results/chameleon-9i-standalone-result.txt "TC64 9i (standalone)" \
		--results results/chameleon-head-standalone-result.txt "TC64 head (standalone)" \
		--results results/chameleon-head-20190802.txt "TC64 head (20190802)" \
		--results results/chameleon-head-20190805.txt "TC64 head (20190805)" \
		--results results/chameleon-head-20191026-result.txt "TC64 head (20191026)" \
		\
		> results/chameleon.html

# use with something like "make resultschamwiki | xclip -selection clipboard"

.PHONY: resultschamwiki
resultschamwiki: maketable chameleon-testlist.txt
	@echo "=== Results ==="
	./maketable --filter-ntscold --wiki --list chameleon-testlist.txt \
		--percentages "{{Progress|%d|%d|%d|}}" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "{{BREAKSEMU}}" \
		--warnvicefail "{{BREAKSEMU}}" \
		--warnvicfetch "{{BREAKSVIC}}" \
		--results results/chameleon-9h-result.txt "TC64 9h (standalone)" \
		--results results/chameleon-9i-cartridge-result.txt "TC64 9i (cartridge)" \
		--results results/chameleon-9i-standalone-result.txt "TC64 9i (standalone)" \
		--results results/chameleon-head-20191026-result.txt "TC64 head (standalone)"

.PHONY: resultscham20
resultscham20: maketable cham20-testlist.txt 
	./maketable --filter-ntscold --html --list cham20-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/xvic-r36700-result.txt "xvic r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--warnvicefail "<small><b>fails on VICE</b></small>" \
		--warnvicfetch "<small><b>VIC may be wrong!</b></small>" \
		--results results/cham20-20150818-result.txt "cham20 20150818" \
		\
		> results/cham20.html

		
################################################################################
		
.PHONY: resultsu64
resultsu64: maketable u64-testlist.txt 
	./maketable --filter-ntscold --html --list u64-testlist.txt \
		--percentages "<small><hr>%d%%<br>%d of %d<small>" \
		--results results/x64sc-r36700-result.txt "x64sc r36700" \
		--firstcolisref "<small><b>fails on VICE</b></small>" \
		--warnvicefail "<small><b>fails on VICE</b></small>" \
		--results results/u64-10c-result.txt "U64 (1.0c)" \
		--results results/u64-118-ntsc-result.txt "U64 PAL (1.18)" \
		--results results/u64-118-pal-result.txt "U64 NTSC (1.18)" \
		--results results/u64-118-result.txt "U64 (1.18)" \
		--results results/u64-121-pal-result.txt "U64 PAL (1.21)" \
		--results results/u64-124-pal-result.txt "U64 PAL (1.24)" \
		\
		> results/u64.html
		
################################################################################
.PHONY: allresults
allresults: results resultsx64 resultsx64sc resultsx128 resultsx128c64 resultsxcbm2 resultsxcbm5x0 resultsx64dtv \
		resultsxpet resultsxplus4 resultsxscpu64 resultsxvic resultsvsid \
		resultsz64kc64 resultsz64kc128 resultsz64kc128c64 resultsz64kvic20 \
		resultshoxs64 resultsdenise resultsmicro64 resultsyace resultsemu64 \
		resultscham resultscham20 resultsu64 

