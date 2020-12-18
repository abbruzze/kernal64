// ===========================================================================
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// 	Taken from https://github.com/darrenfoulds/1nvader-c64 and adapted for k64ass 
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
BasicUpstart($4000,"Main")
		 
val v        = 53248
val joy      = 56320
val cm       = $0400    ; screen memory
val ticm     = $04f0
val ticc     = $d8f0
val ticc2    = $d811
val scrloc   = $0658    ; scroll text loc
val cm2      = $04f0    ; was $0662/$0702
val cm3      = $05e0
val cm4      = $06d0
val br       = $07c0    ; bottom row of txt
val mountsm  = $06f8    ; mount screen mem
val mountcm  = $daf8    ; mount colour mem

val cmem0    = $2000
val cmem1    = $2100
val cmem2    = $2200
val cmem3    = $2300
val cmem4    = $2400
val cmem5    = $2500
val cmem6    = $2600
val cmem7    = $2700

val cset0    = $d000    ; this is where
val cset1    = $d100    ; the 6510 sees
val cset2    = $d200    ; the char data
val cset3    = $d300    ; under the
val cset4    = $d400
val cset5    = $d500
val cset6    = $d600
val cset7    = $d700

val sf1      = $0428
val sf2      = $0530    ; +8 to fill botrow

val sprmem1  = $3200    ; sprite memory
val sprmem2  = $3240
val sprmem3  = $3280
val sprmem4  = $32c0


;-- inv516a ----------------------------

;-- setup ------------------------------

         lda #147
         jsr $ffd2  ; clear screen

         lda #0
         sta $d020  ; border colour
         lda #0
         sta $d021  ; screen colour

         lda #1
         sta $0286  ; character colour

         lda #1     ; colour the screen
         ldx #0     ; col mem white
colscr:   sta $d800,x
         sta $d8c8,x
         sta $d990,x
         sta $da58,x
         sta $db20,x
         inx
         cpx #200
         bne colscr

         jsr charsetup
         jsr sprsetup

         sei        ; turn on interupts

         lda #200   ; sprite pointers
         sta $07f8  ; ms
         lda #201
         sta $07f9  ; p1
         sta $07fa  ; p2
         lda #202
         sta $07fb  ; l1
         sta $07fc  ; l2

       ; jsr drawrows
         jsr drawmounts

;-- title ------------------------------

title:    jsr titlinit ; new game setup

         lda #148   ; set p1+2 x+y
         sta p1x
         lda #196
         sta p2x
         lda #242   ; p1y p2y
         sta p1y    ; touching bottom
         sta p2y
         lda #160   ; set ms x+y
         sta msx
         lda #146   ; was 58
         sta msy
         lda #11
         sta msrow  ; row 11 = 146

         lda #1     ; make ms ???
         sta v+29   ; expand x
         sta v+23   ; expand y
         sta ssflag
         jsr showscr; show score

                    ; should be 7
         lda #7     ; ms +p1 +p2 +xx +xx
         sta v+21   ; turn on sprites
         jsr outp1
         jsr outp2
       ; jsr outms  ; don't work no more
         lda msx    ; do it manually
         sta v
         lda msy
         sta v+1
         lda mscol
         sta v+39

       ; lda #1
       ; sta charcol
       ; jsr coltitle

         jsr tistripe
         jsr drawtitle
         jsr clrstats

       ; jsr soundend

         lda #0
         sta scrcnt

titlea:   jsr vbwait

         inc scrjifs
         lda scrjifs
         cmp #4       ; 3 too fast?
         bne scr99
         lda #0
         sta scrjifs

         ldx scrcnt
         ldy #0       ; print 40 chars
scr02:    lda scrtxt,x ; startin at
         sta scrloc,y ; scrcnt
         inx
         iny
         cpy #40      ; 40 chars
         bne scr02

         inc scrcnt
scr99:
       ; jsr twinkle
         jsr ticolrol ; fancy title fx

         jsr fire1  ; get p1 fire
         lda p1f    ; check p1f
         cmp #1
         bne titleb ; no fire
         lda #1     ; fire p1 start
         sta p1z
         lda #234   ; pop up p1
         sta p1y
         jsr outp1
         jmp clrscr ; begin gameinit
titleb:   jsr fire2  ; get p2 fire
         lda p2f    ; check p2f
         cmp #1
         bne titlea ; no fire wait
         lda #1     ; fire p2 start
         sta p2z
         lda #234   ; pop up p2
         sta p2y
         jsr outp2

clrscr:   lda #32    ; clear scroll
         ldy #0
clrscr2:  sta scrloc,y
         iny
         cpy #40
         bne clrscr2

titlez:   jmp gamestrt

drawtitle: ldx #0    ; draw title
drawti2:  lda tichar,x
         sta ticm,x
         inx
         cpx #240
         bne drawti2
         rts

clrtitl:  ldx #0     ; clear the title
         lda #32
clrtitl2: sta ticm,x
         inx
         cpx #240
         bne clrtitl2
         rts

coltitle: ldx #0
         lda charcol
colti2:   sta ticc,x
         inx
         cpx #240
         bne colti2
         rts

drawmounts:          ; draw mountains
         ldx #0

drwmtsa:  lda mountc,x
         sta mountsm,x

         lda #11    ; default = grey
         sta mountcm,x

         lda mountc,x
         cmp #$5d   ; is it a peak?
         bne drwmtsd
         lda #15    ; white
         sta mountcm,x

drwmtsd:  inx
         cpx #240
         bne drwmtsa

         ldx #160
         lda #9
drwmtsb:  sta mountcm,x
         inx
         cpx #200
         bne drwmtsb

         lda #8
drwmtsc:  sta mountcm,x
         inx
         cpx #240
         bne drwmtsc

         lda #5
         ldx #160
         sta mountcm,x
         ldx #199
         sta mountcm,x

         lda #3        ; colour stats
         ldx #215
drwmtse:  sta mountcm,x
         inx
         cpx #225
         bne drwmtse

         rts

tistripe:          ; striped title colour
         ldx #0
         ldy #0
tsta:     cpy #0    ; 0 is black
         bne tstb
         lda #6    ; 0
         jmp tstx
tstb:     cpy #1    ; 1 is dark grey
         bne tstc
         lda #11   ; 11
         jmp tstx
tstc:     cpy #2    ; 2 is med grey
         bne tstd
         lda #12   ; 12
         jmp tstx
tstd:     cpy #3    ; 3 is lite grey
         bne tste
         lda #15   ; 15
         jmp tstx
tste:     cpy #4    ; 4 is white
         bne tstf
         lda #1
         jmp tstx
tstf:     lda #4    ; leftover 4=purple
tstx:     sta ticc,x ; change the colour
         iny
         cpy #6
         bne tsty
         ldy #0
tsty:     inx      ; 200 leaves last line
         cpx #160 ; 240 for all
         bne tsta
tstz:     rts

ticolrol: ldx #0    ; roll the title col
tcra:     inc ticc,x
         inx
         cpx #240
         bne tcra
tcrz:     rts

;-- game init --------------------------

titlinit: lda #0
         sta goflag
         sta p1d
         sta p1f
         sta p2f
         sta p1z
         sta p2z
         sta msx+1
         sta p1x+1
         sta p2x+1
         sta msrow

         lda #1
         sta p2d
         sta msd
         sta j1z
         sta j2z
         sta ssflag

         lda #2
         sta mscol

         lda #13
         sta p1col

         lda #14
         sta p2col

         lda #148
         sta p1x

         lda #172
         sta msx

         lda #196
         sta p2x

         lda #242    ; touching bottom
         sta p1y
         sta p2y

         rts

gameinit: sed
         lda #0
         sta p1score
         sta p1score+1
         sta p1score+2
         sta p2score
         sta p2score+1
         sta p2score+2
         cld

         sed
         lda #128   ; 128 = 80
         sta hits
         lda #0
         sta hits+1
         cld

         lda #0
         sta p1bf
         sta p2bf

         lda #1
         sta ssflag
         jsr showscr
                     ; should be 2
         lda #2      ; initial ms speed
         sta msmovs
         lda #10     ; should be 10
         sta mssut   ; speedup threshld
         sta mssuc   ; speedup count
gameintz: rts

cntdwn:               ; wait for other p
         lda #51     ; seconds 51-48=3s
         sta secs
         sbc #15     ; get wide chars
         sta cm+179  ; shw cntdwn secs
         adc #2
         sta cm+180  ; other half
         lda #29     ; jiffys 0.5 secs
         sta jifs

cntdwna:  jsr ticolrol;  >title fx

         dec jifs
         lda jifs
         cmp #0

         bne cntdwnb ; go wait

                     ; decrease secs
         lda #29     ; reset jifs
         sta jifs
         dec secs    ; secs=secs-1
         lda secs
         cmp #48     ; 48 = '0'

         beq cntdwne ; lets play

         sbc #16     ; wide numbers
         sta cm+179  ; show secs
         adc #2
         sta cm+180  ; other half

cntdwnb:  jsr vbwait  ; wait 1 frame
         lda p1z     ; need to chk?
         cmp #1
         beq cntdwnc ; nope go away
         jsr fire1   ; yes we do
         inc v+40    ; rainbow if check
         lda p1f
         cmp #1      ; check p1 fire
         bne cntdwnc ; no
         lda #1      ; yes join game
         sta p1z     ; p1ztatus
         lda #234    ; bump p1 up
         sta p1y
         jsr outp1
cntdwnc:  lda p2z     ; need to chk?
         cmp #1
         beq cntdwnd ; nope go away
         jsr fire2   ; yes we do
         inc v+41    ; rainbow if check
         lda p2f
         cmp #1      ; check p2 fire
         bne cntdwnd ; no
         lda #1      ; yes join game
         sta p2z     ; p2ztatus
         lda #234    ; bump p2 up
         sta p2y
         jsr outp2
cntdwnd:  jmp cntdwna

cntdwne:  lda #$47    ; print "??"
         sta cm+179  ;
         lda #$4f
         sta cm+180
                     ; slide stuff away
cntdwni:  jsr ticolrol
         jsr vbwait
         dec msy     ; silde ms up
         dec msy
       ; jsr outms   ; won't work with
                     ; raw msy. must use
                     ; msrow
                     ; do it manually
         lda msy     ; get msy
         sta v+1     ; set spr1 y


cntdsp1:  lda p1z
         cmp #1
         beq cntdsp2
         lda p1y     ; slide p1
         cmp #250
         beq cntdsp2 ; already off scrn
         inc p1y
         jsr outp1

cntdsp2:  lda p2z
         cmp #1
         beq cntdsz
         lda p2y     ; slide p2
         cmp #250    ; y=250
         beq cntdsz  ; already off scrn
         inc p2y
         jsr outp2

cntdsz:   lda msy
         cmp #32     ; should be off top
         bne cntdwni

                     ; reset ms stuff
cntdwnj:  lda #0      ; reset ms x,y
         sta v+29    ; expand
         sta v+23    ; (for ms)
         sta msx
         sta p1f
         sta p2f
         sta l1s
         sta l2s
         sta msrow

       ; lda #58     ; should be 58
       ; sta msy     ; 202 for testing
         ldx msrow   ; get msy from
         lda r2ytab,x  ; row 2 y table
         sta msy

         lda #2
         sta mscol   ; ms is red
         jsr getpoints
         lda #1
         sta ssflag
         jsr showscr

         lda #32      ; clear
         sta cm+179   ; countdown
         sta cm+180

         jsr clrtitl  ; clear title txt
       ; lda #0       ; black color mem
       ; sta charcol  ; set 0=black
       ; jsr coltitle ; color title blk

cntdwnz:  rts

;-- game loop --------------------------

gamestrt: jsr gameinit
         jsr cntdwn
gameloop: jsr vbwait
       ; lda v+30    ; get colision reg
       ; sta v30     ; save to chk latr
         jsr input
         jsr process
         jsr output
         lda goflag  ; chk gameover flg
         cmp #1
         bne gameloop
         jmp gameover

vbwait:  ;inc $d020   ; timing colour
vbwaita:  lda $d012
         cmp #251
         bne vbwaita
        ;dec $d020   ; timing colour
         lda v+30    ; get col reg
         sta v30     ; save to v30
         rts

;-- input ------------------------------

input:    lda p1z     ; get p1 ztatus
         cmp #1
         bne inputd  ; skip if p1 off

         lda l1s     ; chk lazer status
         cmp #0      ; lazer is off, ??
         beq inputa
         lda l1y     ; chk lazer hight
         sbc #151    ; is l1y < 150 ?
         bcc inputa  ; yes, ??
         jmp inputd

inputa:   jsr fire1

inputd:   lda p2z     ; get p2 ztatus
         cmp #1
         bne inputz  ; skip if p2 off

         lda l2s
         cmp #0      ; lazer of, ??
         beq inpute
         lda l2y
         sbc #151    ; is l2y < 200 ?
         bcc inpute  ; yes, ??
         jmp inputz

inpute:   jsr fire2

inputz:   rts

fire1:    lda joy+1   ; remember: 0=fire
         and #16
         cmp #0
         beq f1maybe
f1nope:   lda #0      ; lastcycle=nofire
         sta j1z
         rts
f1maybe:  lda j1z
         cmp #0
         bne f1nope2
         lda #1      ; set p1f?????!
         sta p1f
         sta j1z
         jsr lazbeep1; fire ?????!
         rts
f1nope2:  lda #0
         sta p1f
         rts

fire2:    lda joy     ; remember: 0=fire
         and #16
         cmp #0
         beq f2maybe
f2nope:   lda #0      ; lastcycle=nofire
         sta j2z
         rts
f2maybe:  lda j2z
         cmp #0
         bne f2nope2
         lda #1      ; set p2f?????!
         sta p2f
         sta j2z     ; lastcycle=fire
         jsr lazbeep2 ; p2 ?????!
         rts
f2nope2:  lda #0
         sta p2f
         rts

;-- process ----------------------------

process:
       ; jsr prolzhit
         jsr prohit
         jsr prolazer
         jsr proms
         jsr bump     ; bump is player
         jsr bounce   ; bounce is wall
         lda p1z
         cmp #1
         bne processa
         jsr prop1
processa: lda p2z
         cmp #1
         bne processb
         jsr prop2
processb: jsr proreset
         rts

       ; ------------- new lazer hit
prohit:   lda l1s
         cmp #0      ; is l1 off?
         beq plh1a
         cmp #12     ; is l1 on?
         beq plh1b
         jmp plh1c

plh1a:    lda #0      ; l1s=0 off
         sta l1s
         lda #202    ; laz sprite
         sta $07fb
         jmp plh2

plh1b:    lda #202    ; l1s=12 on&up
         sta $07fb   ; laz sprite

         lda v30     ; chk collision
         and #9
         cmp #9      ; s1(ms) + s4(l1)
         bne plh2    ; no hit, check l2

         dec l1s     ; hit?!!!
         sed         ; add p1score
         clc
         lda p1score
         adc mspts   ; from getpoints
         sta p1score
         lda p1score+1
         adc mspts+1 ; from getpoints
         sta p1score+1
         lda p1score+2
         adc #0      ; carry if needed
         sta p1score+2

         sec
         lda hits    ; decrease hits
         sbc #1
         sta hits
         lda hits+1
         sbc #0
         sta hits+1
         cld

         jmp lzhitb  ; goto pop ms up
       ; jmp plh2

plh1c:    dec l1s     ; 12<l1s>0 exp
         lda #203    ; exp sprite
         sta $07fb   ;
         jmp plh2

       ; ------------- lazer 2 hit check
plh2:     lda l2s
         cmp #0      ; is l2 off?
         beq plh2a
         cmp #12     ; is l2 on?
         beq plh2b
         jmp plh2c

plh2a:    lda #0      ; l2s=0 off
         sta l2s
         lda #202    ; laz sprite
         sta $07fc
         jmp prohitz

plh2b:    lda #202    ; l2s=12 on&up
         sta $07fc   ; laz sprite

         lda v30     ; chk collision
         and #17
         cmp #17     ; s1(ms) + s5(l1)
         bne prohitz ; no hit, done

         dec l2s     ; hit?!!!
         sed         ; add p2score
         clc
         lda p2score
         adc mspts   ; from getpoints
         sta p2score
         lda p2score+1
         adc mspts+1 ; from getpoints
         sta p2score+1
         lda p2score+2
         adc #0      ; carry if needed
         sta p2score+2

         sec
         lda hits    ; decrease hits
         sbc #1
         sta hits
         lda hits+1
         sbc #0
         sta hits+1
         cld

         jmp lzhitb  ; goto pop ms up
       ; jmp prohitz

plh2c:    dec l2s     ; 12<l2s>0 exp
         lda #203    ; exp sprite
         sta $07fc   ;
         jmp prohitz

prohitz:  rts
       ; ------------- end new prohit

prolzhit:             ; was ms hit?
         lda l1s
         cmp #0
         beq lz2hit  ; l1 off, check l2
         cmp #1
         beq plz1a   ; goto active lz1

         lda #203    ; explosion stuff
         sta $07fb   ; exp sprite on
         dec l1s
         lda l1s
         cmp #1
         bne plz1b
         lda #0      ; turn off exp
         sta l1s
         lda #202    ; lz sprite on
         sta $07fb

plz1b:    jmp lz2hit

plz1a:    lda v30     ; get collision
         and #9
         cmp #9      ; s1(ms) + s4(l1)
         bne lz2hit  ; no hit, check l2
         sed         ; add p1score
         clc
         lda p1score
         adc mspts   ; from getpoints
         sta p1score
         lda p1score+1
         adc mspts+1 ; from getpoints
         sta p1score+1
         lda p1score+2
         adc #0      ; carry if needed
         sta p1score+2

         sec
         lda hits    ; decrease hits
         sbc #1
         sta hits
         lda hits+1
         sbc #0
         sta hits+1

         cld
         lda #12     ; was 0
         sta l1s     ; turn off l1
         jmp lzhitb  ; goto kill ms
lz2hit:
         lda l2s
         cmp #0
       ; bne lz2hita
         bne plz1e
         jmp lzhitz

plz1e:    cmp #1
         beq lz2hita ; goto active lz2

         lda #203    ; explosion stuff
         sta $07fc   ; exp sprite on
         dec l2s
         lda l2s
         cmp #1
         bne plz1c
         lda #0      ; turn off exp
         sta l2s
         lda #202    ; lz sprite on
         sta $07fc

plz1c:    jmp lzhitz  ; l2 off, done

lz2hita:  lda v30     ; get collision
         and #17
         cmp #17     ; s1(ms) + s5(l2)
         beq lz2hitb
         jmp lzhitz  ; no hit
lz2hitb:  sed         ; add p2score
         clc
         lda p2score
         adc mspts
         sta p2score
         lda p2score+1
         adc mspts+1
         sta p2score+1
         lda p2score+2
         adc #0      ; carry if needed
         sta p2score+2

         sec
         lda hits    ; decrease hits
         sbc #1
         sta hits
         lda hits+1
         sbc #0
         sta hits+1

         cld
         lda #12     ; was 0
         sta l2s     ; turn off l2

       ; ------------- kill mothership
lzhitb:               ; ms was hit, popup
         jsr expnoz  ; make ?????
         dec mssuc   ; speedup counter
         lda mssuc
         cmp #0      ; time to speedup?
         bne lzhite  ; no
         inc msmovs  ; yes
         lda msmovs
         cmp #10     ; is msmovs = 10
         bne lzhitr  ; no
         lda #2      ; yes, relief!
         sta msmovs  ; set msmovs = 2
         lda #128    ; 128 = 80bcd
         sta hits    ; reset hit cout

lzhitr:   lda mssut
         sta mssuc   ; reset counter
lzhite:   lda #1
         sta ssflag  ; set ssflag
       ; lda msy     ; pop ms up 16px
       ; sbc #16
       ; sta msy
         sed         ; set dec flag
         clc
         lda msrow   ; why sbc #1 ?????
         sbc #1      ; pop msrow up 2
         sta msrow
         cld         ; clr dec flag
         lda #30     ; check msrow
         cmp msrow   ; is msrow < 30?
         bcs lzhitf  ; no, skip
         lda #0      ; yes, make it 0
         sta msrow
lzhitf: ; lda #58     ; is msy >= 58?
       ; cmp msy
       ; bcc lzhitc  ; yes, go away
       ; lda #58     ; no, make it 58
       ; sta msy
       ; lda #0      ; msrow to 0 too
       ; sta msrow
lzhitc:   jsr getpoints ; set new x
         lda msd     ; check msd
         bne lzhitd
         sta msx     ; was going left
         sta msx+1   ; msx x=0
         lda #1      ; go right
         sta msd
         jmp lzhitz
lzhitd:   lda #1      ; was going right
         sta msx+1
         lda #89     ; msx x=344
         sta msx
         lda #0      ; go left
         sta msd
lzhitz:   rts

prolazer:
         lda l1s
       ; cmp #0
       ; beq lazera
         cmp #12     ; l1s=12 active laz
         bne lazera  ; only l1s goes up
         lda l1y     ; l1 up
         sbc #4
         sta l1y
         cmp #50
         bne lazera
         lda #0
         sta l1s     ; l1 off
         lda #202    ; use lz sprite
         sta $07fb   ; not exp

lazera:   lda l2s
       ; cmp #0
       ; beq lazerz
         cmp #12     ;l2s=12 active laz
         bne lazerz
         lda l2y     ; l2 up
         sbc #4
         sta l2y
         lda l2y
         cmp #50
         bne lazerz
         lda #0
         sta l2s     ; l2 off
         lda #202    ; use lz sprite
         sta $07fc   ; not exp
lazerz:   rts

proms:    lda msmovs ; loop this ammount
         sta msm    ; msm is counter
procmsm:  lda msm
         cmp #0
         beq procmsz ; done moving ms
         jsr promsdo ; do the real move
         dec msm
         jmp procmsm ; loop again
procmsz:  rts

promsdo:  lda msd     ; this is the move
         cmp #0      ; routine for real
         beq pmslt
pmsrt:    inc msx     ; move right
         bne pmsbc
         inc msx+1
         jmp pmsbc
pmslt:    lda msx+1   ; move left
         bne pmslta
         dec msx     ; < 256
         jmp pmsbc
pmslta:   dec msx     ; > 255
         lda msx
         cmp #255
         beq pmsltb
         jmp pmsbc
pmsltb:   dec msx+1   ; dec msx+1
         jmp pmsbc
pmsbc:    lda msd     ; ms bounce
         cmp #0
         beq msbltr
msbrtl:   lda msx+1   ; bounce off right
         cmp #0
         beq promszz ; skip
         lda msx
         cmp #89     ; 89+255=344
         bne promszz ; skip
         lda #0
         sta msd     ; set msd=0(left)
       ; lda msy     ; drop down
       ; clc
       ; adc #8
       ; sta msy
         sed         ; drop down msrow
         clc         ; set dec+clr carry
         lda msrow
         adc #1
         sta msrow
         cld         ; clr dec flag
         jsr getpoints ; update points
         jmp promsz
msbltr:   lda msx+1   ; bounce off left
         cmp #0
         bne promszz ; msx+1 set, skip
         lda msx     ; check msx
         cmp #0
         bne promszz ; msx<>0, skip
         lda #1
         sta msd     ; set msd=1(right)
       ; lda msy     ; drop down
       ; clc
       ; adc #8
       ; sta msy
         sed         ; drop msrow down
         clc         ; set dec+clr carry
         lda msrow
         adc #1
         sta msrow
         cld         ; clr dec flag
         jsr getpoints ; update points
         jmp promsz
promsz: ; lda msy     ; check bottom
       ; cmp #234
         lda msrow
         cmp #34
         bcs promsb  ; row = 23 (??? 34)
       ; bcs promsb  ; >= 234
         jmp promszz ; < 234
promsb:   lda #1      ;
         sta goflag
promszz:  rts

bump:                 ; p1/p2 bump check
         lda p1z     ; no p1 dont bump
         cmp #1
         bne bumpz
         lda p2z     ; no p2 dont bump
         cmp #1
         bne bumpz
         lda v30     ; get sp collision
         and #6      ; 2+4=6
         cmp #6
         bne bumpz
         lda p1d
         cmp #0
         bne bumpa
         sta p2d     ; p1 on right
         lda #1
         sta p1d
         sta p1bf    ; set bump flags
         sta p2bf
         jmp bumpz
bumpa:    sta p2d     ; p2 on left
         lda #0
         sta p1d
         lda #1
         sta p1bf    ; set bump flags
         sta p2bf
bumpz:    rts

bounce:   lda p1d     ; p1 bounce
         cmp #0
         beq p1bltr
p1brtl:   lda p1x+1
         cmp #0
         beq bouncep2
         lda p1x     ; x=255+65=320
         cmp #65     ;     right wall
         bne bouncep2
         lda #0
         sta p1d
         lda #1
         sta p1bf    ; set bounce flg
         jmp bouncep2
p1bltr:   lda p1x+1
         cmp #0
         bne bouncep2
         lda p1x
         cmp #24     ; x=24 left wall
         bne bouncep2
         lda #1
         sta p1d
         sta p1bf    ; set bounce flg
bouncep2: lda p2d     ; p2 bounce
         cmp #0      ; same as p1
         beq p2bltr
p2brtl:   lda p2x+1
         cmp #0
         beq bouncez
         lda p2x
         cmp #65
         bne bouncez
         lda #0
         sta p2d
         lda #1
         sta p2bf    ; set bounce flg
         jmp bouncez
p2bltr:   lda p2x+1
         cmp #0
         bne bouncez
         lda p2x
         cmp #24
         bne bouncez
         lda #1
         sta p2d
         sta p2bf    ; set bounce flag
bouncez:  rts

prop1:    lda p1f
         cmp #1      ; check p1 fire
         beq prop1a
         jmp pp1move
prop1a:   lda p1bf    ; check bump flag
         cmp #1      ; if bump
         beq prop1fyr; skip dir change
         lda p1d     ; change direction
         cmp #0
         beq prop1d
         lda #0      ; change to left
         sta p1d
         jmp prop1fyr
prop1d:   lda #1      ; change to right
         sta p1d
         jmp prop1fyr
prop1fyr:             ; fire laser
         lda #12
         sta l1s
         lda p1x
         sta l1x
         lda p1x+1
         sta l1x+1
         lda #226
         sta l1y
         jmp pp1move
pp1move:
         lda p1d
         cmp #0
         beq pp1lt
pp1rt:    inc p1x     ; move right
         bne pp1bc
         inc p1x+1
         jmp pp1bc
pp1lt:    lda p1x+1   ; move left
         bne pp1lta
         dec p1x     ; < 256
         jmp pp1bc
pp1lta:   dec p1x     ; > 255
         lda p1x
         cmp #255
         beq pp1ltb
         jmp pp1bc
pp1ltb:   dec p1x+1  ; it happens earlyr
         jmp pp1bc  ; its not here now
pp1bc:    jmp prop1z ; leftover bounce
prop1z:   rts

prop2:    lda p2f
         cmp #1
         beq prop2a
         jmp pp2move
prop2a:   lda p2bf    ; check bump flag
         cmp #1      ; if set
         beq prop2fyr; skip dir change
         lda p2d     ; change direction
         cmp #0
         beq prop2d
         lda #0
         sta p2d
         jmp prop2fyr
prop2d:   lda #1
         sta p2d
         jmp prop2fyr
prop2fyr:
         lda #12
         sta l2s
         lda p2x
         sta l2x
         lda p2x+1
         sta l2x+1
         lda #226
         sta l2y
         jmp pp2move
pp2move:
         lda p2d
         cmp #0
         beq pp2lt
pp2rt:    inc p2x
         bne pp2bc
         inc p2x+1
         jmp pp2bc
pp2lt:    lda p2x+1
         bne pp2lta
         dec p2x
         jmp pp2bc
pp2lta:   dec p2x
         lda p2x
         cmp #255
         beq pp2ltb
         jmp pp2bc
pp2ltb:   dec p2x+1
         jmp pp2bc
pp2bc:    jmp prop2z

prop2z:   rts

proreset: lda #0      ;
         sta p1f     ; reset fire flags
         sta p2f
         sta p1bf    ; reset bump flags
         sta p2bf
         rts

;-- output -----------------------------

output:
       ; jsr twinkle
         jsr showscr ; show score
         jsr shwstats; show stats
         jsr outms   ; show ms
         lda p1z
         cmp #1
         bne outputa ; skip p1 output
         jsr outp1   ; show p1 l1
         jsr outl1
outputa:  lda p2z
         cmp #1
         bne outputz ; skip p2 output
         jsr outp2   ; show p2 l2
         jsr outl2
outputz:  jsr twinkle
         rts

showscr:  lda ssflag
         bne shsca
         jmp shscz
shsca:    lda #0      ; turn flag off
         sta ssflag

       ; lda msmovs  ; show msmovs
       ; adc #48
       ; sta cm+13

         lda #32     ; clear hiscore
         sta cm+15   ; indicators
         sta cm+24

         lda p1score ; show p1score
         and #%00001111
         clc
         adc #48
         sta cm+6
         lda p1score
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+5

         lda p1score+1
         and #%00001111
         clc
         adc #48
         sta cm+4
         lda p1score+1
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+3

         lda p1score+2
         and #%00001111
         clc
         adc #48
         sta cm+2
         lda p1score+2
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+1    ; end p1score

         lda p2score ; show p2score
         and #%00001111
         clc
         adc #48
         sta cm+38
         lda p2score
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+37

         lda p2score+1
         and #%00001111
         clc
         adc #48
         sta cm+36
         lda p2score+1
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+35

         lda p2score+2
         and #%00001111
         clc
         adc #48
         sta cm+34
         lda p2score+2
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+33   ; end p2score

 ; check p1score for hiscore
         lda p1score+2
         cmp hiscore+2
         bcc chkhip2 ; end
         bne uphi1

         lda p1score+1
         cmp hiscore+1
         bcc chkhip2
         bne uphi1

         lda p1score
         cmp hiscore
         bcc chkhip2

uphi1:    ; update hs with p1score
         lda p1score
         sta hiscore
         lda p1score+1
         sta hiscore+1
         lda p1score+2
         sta hiscore+2

chkhip2:  ; check p2s for hiscore
         lda p2score+2
         cmp hiscore+2
         bcc chkhiz  ; end
         bne uphi2

         lda p2score+1
         cmp hiscore+1
         bcc chkhiz
         bne uphi2

         lda p2score
         cmp hiscore
         bcc chkhiz

uphi2:    ; update hs with p2score
         lda p2score
         sta hiscore
         lda p2score+1
         sta hiscore+1
         lda p2score+2
         sta hiscore+2
chkhiz:   ; done hiscore check

shscc:    lda hiscore ; show hiscore
         and #%00001111
         clc
         adc #48
         sta cm+22
         lda hiscore
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+21

         lda hiscore+1
         and #%00001111
         clc
         adc #48
         sta cm+20
         lda hiscore+1
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+19

         lda hiscore+2
         and #%00001111
         clc
         adc #48
         sta cm+18
         lda hiscore+2
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta cm+17   ; end hiscore

shscz:    rts

outms:    lda mscol   ; get colour
         sta v+39

       ; lda msy     ; get msy
       ; sta v+1     ; set spr1 y

         ldy msrow   ; get msrow
         lda r2ytab,y; get y from table
       ; sta msy     ; store in msy
         sta v+1     ; set spr1 y

         lda msx+1   ; get msx hi-byte
         bne outmsa  ; hi-byte != 0
         lda msx     ; msx <= 255
         sta v
         lda v+16    ; get hx flags
         and #254    ; sp1 hx off
         sta v+16
         rts
outmsa:   lda msx     ; msx > 255
         sta v
         lda v+16    ; get hx flags
         ora #1      ; sp1 hx on
         sta v+16
         rts

outp1:                ; player 1
         lda p1col   ; p1 colour
         sta v+40
         lda p1y
         sta v+3
         lda p1x+1
         bne outp1a
         lda p1x
         sta v+2
         lda v+16
         and #253    ; sp2 hx off
         sta v+16
         rts
outp1a:   lda p1x
         sta v+2
         lda v+16
         ora #2      ; sp1 hx on
         sta v+16
         rts

outp2:                ; player 2
         lda p2col
         sta v+41
         lda p2y
         sta v+5
         lda p2x+1
         bne outp2a
         lda p2x
         sta v+4
         lda v+16
         and #251    ; sp3 hx off
         sta v+16
         rts
outp2a:   lda p2x
         sta v+4
         lda v+16
         ora #4      ; sp3 hx on
         sta v+16
         rts

outl1:                ; laser 1
         lda l1s
         cmp #0
         beq outl1b  ; inactive laser
         lda v+21
         ora #%00001000 ; s4 on
         sta v+21
         lda p1col
         sta v+42
         lda l1y
         sta v+7
         lda l1x+1
         bne outl1a
         lda l1x
         sta v+6
         lda v+16
         and #247    ; sp4 hx off
         sta v+16
         rts
outl1a:   lda l1x
         sta v+6
         lda v+16
         ora #8      ; sp4 hx on
         sta v+16
         rts
outl1b:               ; s4 off
         lda v+21
         and #%11110111
         sta v+21
         rts

outl2:                ; player 2
         lda l2s
         cmp #0
         beq outl2b  ; inactive laser
         lda v+21
         ora #%00010000 ; s5 on
         sta v+21
         lda p2col
         sta v+43
         lda l2y
         sta v+9
         lda l2x+1
         bne outl2a
         lda l2x
         sta v+8
         lda v+16
         and #239    ; sp5 hx off
         sta v+16
         rts
outl2a:   lda l2x
         sta v+8
         lda v+16
         ora #16     ; sp5 hx on
         sta v+16
         rts
outl2b:               ; s5 off
         lda v+21
         and #%11101111
         sta v+21
         rts

shwstats: ; msrow mspts msmov etc
shwmsrow: lda msrow       ; stored as bcd
         and #%00001111
         clc
         adc #48
         sta br+16
         lda msrow
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta br+15

         lda mspts   ; show points
         and #%00001111
         clc
         adc #48
         sta br+21
         lda mspts
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta br+20

         lda mspts+1
         and #%00001111
         clc
         adc #48
         sta br+19
         lda mspts+1
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta br+18

         lda hits
         and #%00001111
         clc
         adc #48
         sta br+24
         lda hits
         lsr
         lsr
         lsr
         lsr
         clc
         adc #48
         sta br+23

       ; lda #48
       ; sta br+23
       ; lda msmovs
       ; adc #48
       ; sta br+24

         rts

clrstats:             ; clear stats row
         lda #32
         ldx #15

clrstsa:  sta br,x
         inx
         cpx #25
         bne clrstsa

         rts

;-- game over --------------------------

gameover: lda #0      ; fancy ending
         sta goflag
gameoa:   jsr vbwait  ; sweep effect
         jsr twinkle

         jsr prop1   ; must keep
         jsr prop2   ; moving p1 p2
         jsr outp1
         jsr outp2
         jsr prolazer; must keep
         jsr outl1   ; moving l1 l2
         jsr outl2

         lda msmovs  ; loop this ammount
         sta msm     ; msm is counter
gameob:
         lda goflag
         cmp #1      ; did ms reach edge
         beq gameoc  ; really gameover
         lda msm
         cmp #0
         beq gameoa  ; done moving ms
         jsr promsdo ; do the real move
         dec msm
         jsr sweepp  ; sweep p1+p2
         jsr outms
         jsr outp1   ; better move p1+p2
         jsr outp2
         jmp gameob  ; loop again

gameoc:   jsr tistripe; show ?????????
         lda #$45    ; ?
         sta ticm+58
         lda #$4f    ; ?
         sta ticm+61
         jsr pause
         lda #$4d    ; ?
         sta ticm+57
         lda #$56    ; ?
         sta ticm+62
         jsr pause
         lda #$41    ; ?
         sta ticm+56
         lda #$45    ; ?
         sta ticm+63
         jsr pause
         lda #$47    ; ?
         sta ticm+55
         lda #$52    ; ?
         sta ticm+64
         jsr pause
         jsr pause
         jsr pause
         jsr pause
         jsr pause

gameod:   lda #32     ; clear ?????????
         sta ticm+58
         sta ticm+61
         jsr pause
         lda #32
         sta ticm+57
         sta ticm+62
         jsr pause
         lda #32
         sta ticm+56
         sta ticm+63
         jsr pause
         lda #32
         sta ticm+55
         sta ticm+64
         jsr pause
         jsr pause
         jsr pause

gameoz:   lda #0
         sta goflag
         sta p1z
         sta p2z
         jmp title

pause:    ldy #6
pausea:   jsr vbwait
         cpy #0
         beq pausez
         dey
         jmp pausea
pausez:   rts

sweepp:               ; col chk ms push
         lda v30
         and #%00000011 ; ms+p1
         cmp #%00000011
         bne sweepp2
         lda msd     ; push p1
         sta p1d     ; same dir
         jsr prop1
         cmp #0      ; if p1d=0 chk p1x
         bne sweepp2 ; else skip
         lda p1x+1
         cmp #1
         beq sweepp2 ; skip if x+1=1
         lda p1x     ; is p1x<12
         cmp #12     ; then p1x+1=1
         bcc sweep1a ; and p1x=330
         jmp sweepp2
sweep1a:  lda #1
         sta p1x+1   ; p1x+1=1
         lda #73
         sta p1x     ; p1x=73(+255=330)

sweepp2:  lda v30
         and #%00000101 ; ms+p2
         cmp #%00000101
         bne sweeppz
         lda msd     ; push p2
         sta p2d     ; same dir
         jsr prop2
         cmp #0      ; ifp2d=0 chk p2x
         bne sweeppz ; else skip

sweeppz:  rts

drawrows: ldx #48     ; draw row numbers
         stx cm+40
         inx
         stx cm+80
         inx
         stx cm+120
         inx
         stx cm+160
         inx
         stx cm+200
         inx
         stx cm2
         inx
         stx cm2+40
         inx
         stx cm2+80
         inx
         stx cm2+120
         inx
         stx cm2+160
         ldx #48
         stx cm2+201
         inx
         stx cm3
         inx
         stx cm3+40
         inx
         stx cm3+80
         inx
         stx cm3+120
         inx
         stx cm3+160
         inx
         stx cm3+200
         inx
         stx cm4
         inx
         stx cm4+40
         inx
         stx cm4+80
         ldx #48
         stx cm4+121
         inx
         stx cm4+160
         rts

getpoints:           ; calculate mspts
       ; this is so embarassing
       ; a lookup table would be so
       ; much easier and faster :(
         lda #0       ; 0000 pts
         sta mspts
         sta mspts+1

         lda msrow
         cmp #0     ; check for row 0
         bne gp1

         lda #0     ; 1000 pts
         sta mspts
         lda #16    ; 5 bit is 1 in tens
         sta mspts+1
         jmp gpz

gp1:      cmp #1     ; check for row 1
         bne gp2

         lda #0     ; 0500 pts
         sta mspts
         lda #5
         sta mspts+1
         jmp gpz

gp2:      cmp #2     ; check for row 2
         bne gp3

         lda #%01110101; 7and5
         sta mspts
         lda #4     ; 475 pts
         sta mspts+1
         jmp gpz

gp3:      cmp #3
         bne gp4

         lda #%01010000; 5and0
         sta mspts
         lda #4     ; 450 pts
         sta mspts+1
         jmp gpz

gp4:      cmp #4
         bne gp5

         lda #%00100101; 2and5
         sta mspts
         lda #4     ; 425  pts
         sta mspts+1
         jmp gpz

gp5:      cmp #5
         bne gp6

         lda #0        ; 0
         sta mspts
         lda #4     ; 400  pts
         sta mspts+1
         jmp gpz

gp6:      cmp #6
         bne gp7

         lda #%01110101; 7and5
         sta mspts
         lda #3     ; 375  pts
         sta mspts+1
         jmp gpz

gp7:      cmp #7
         bne gp8

         lda #%01010000; 5and0
         sta mspts
         lda #3     ; 350  pts
         sta mspts+1
         jmp gpz

gp8:      cmp #8
         bne gp9

         lda #%00100101; 2and5
         sta mspts
         lda #3     ; 325  pts
         sta mspts+1
         jmp gpz

gp9:      cmp #9
         bne gp10

         lda #0        ; 0
         sta mspts
         lda #3     ; 300  pts
         sta mspts+1
         jmp gpz

gp10:     cmp #16     ; ??? 10
         bne gp11

         lda #%01110101; 7and5
         sta mspts
         lda #2     ; 275  pts
         sta mspts+1
         jmp gpz

; 11 and on...

gp11:     cmp #17     ; ??? 11
         bne gp12

         lda #%01010000; 5and0
         sta mspts
         lda #2     ; 250  pts
         sta mspts+1
         jmp gpz

gp12:     cmp #18     ; ??? 12
         bne gp13

         lda #%00100101; 2and5
         sta mspts
         lda #2     ; 225 pts
         sta mspts+1
         jmp gpz

gp13:     cmp #19     ; ??? 13
         bne gp14

         lda #0        ; 0
         sta mspts
         lda #2     ; 200 pts
         sta mspts+1
         jmp gpz

gp14:     cmp #20     ; ??? 14
         bne gp15

         lda #%01110101; 7and5
         sta mspts
         lda #1     ; 175  pts
         sta mspts+1
         jmp gpz

gp15:     cmp #21     ; ??? 15
         bne gp16

         lda #%01010000; 5and0
         sta mspts
         lda #1     ; 150  pts
         sta mspts+1
         jmp gpz

gp16:     cmp #22     ; ??? 16
         bne gp17

         lda #%00100101; 2and5
         sta mspts
         lda #1     ; 125  pts
         sta mspts+1
         jmp gpz

gp17:     cmp #23     ; ??? 17
         bne gp18

         lda #0        ; 0
         sta mspts
         lda #1     ; 100  pts
         sta mspts+1
         jmp gpz

gp18:     cmp #24     ; ??? 18
         bne gp19

         lda #%01110101; 7and5
         sta mspts
         lda #0     ; 075  pts
         sta mspts+1
         jmp gpz

gp19:     cmp #25     ; ??? 19
         bne gp20

         lda #%01010000; 5and0
         sta mspts
         lda #0     ; 050  pts
         sta mspts+1
         jmp gpz

gp20:     cmp #32     ; ??? 20
         bne gp21

         lda #%00100101; 2and5
         sta mspts
         lda #0     ; 025  pts
         sta mspts+1
         jmp gpz

gp21:     lda #1     ; 0001 pts
         sta mspts
         lda #0
         sta mspts+1

gpz:      rts

;---------------------------------------

soundgo1:
         lda attdec
         sta $d405
         lda susrel
         sta $d406
         lda volume
         sta $d418
         lda hifreq
         sta $d400
         lda lofreq
         sta $d401
         ldx wavefm
         inx
         txa
         sta $d404
         rts

soundgo2:
         lda attdec
         sta $d40c
         lda susrel
         sta $d40d
         lda volume
         sta $d418
         lda hifreq
         sta $d407
         lda lofreq
         sta $d408
         ldx wavefm
         inx
         txa
         sta $d40b
         rts

soundgo3:
         lda attdec
         sta $d413
         lda susrel
         sta $d414
         lda volume
         sta $d418
         lda hifreq
         sta $d40e
         lda lofreq
         sta $d40f
         ldx wavefm
         inx
         txa
         sta $d412
         rts

soundend1:
         lda #0
         sta $d404     ; wf1
         rts

soundend2:
         lda #0
         sta $d40b     ; wf2
         rts

soundend3:
         lda #0
         sta $d412     ;wf3
         rts

lazbeep1:
         jsr soundend1
         lda #%00001001 ; 0 9
         sta attdec
         lda #%00000000 ; 0 0
         sta susrel
         lda #15        ; 15
         sta volume
         lda #12        ; 12
         sta hifreq
         lda #8         ; 8
         sta lofreq
         lda #32        ; 32 saw
         sta wavefm

         jsr soundgo1
         rts

lazbeep2:
         jsr soundend2
         lda #%00001001 ; 0 9
         sta attdec
         lda #%00000000 ; 0 0
         sta susrel
         lda #15        ; 15
         sta volume
         lda #13        ; 13
         sta hifreq
         lda #9         ; 9 bit higher
         sta lofreq
         lda #32        ; 32 saw
         sta wavefm

         jsr soundgo2
         rts

expnoz:
         jsr soundend3
         lda #%00011001 ; 1 9
         sta attdec
         lda #%00000000 ; 0 0
         sta susrel
         lda #15        ; 15
         sta volume
         lda #1         ; 1
         sta hifreq
         lda #16        ; 16
         sta lofreq
         lda #128       ; 128 noise
         sta wavefm

         jsr soundgo3
         rts

;---------------------------------------

charsetup:
         sei        ; turn off interupts

         lda #$18   ; *=$2000
         sta $d018

         lda $01    ; swap char rom in
         and #251   ; #%11111011
         sta $01    ; maybe?

         ldx #0

csetupa:  lda cset0,x ; get char data
         sta cmem0,x ; store in charmem

         lda cdat1,x
         sta cmem1,x

         lda cdat2,x
         sta cmem2,x

         lda cset3,x
         sta cmem3,x

         lda cset4,x
         sta cmem4,x

         lda cset5,x
         sta cmem5,x

         lda cset6,x
         sta cmem6,x

         lda cset7,x
         sta cmem7,x

         inx
         beq csetupz
         jmp csetupa

csetupz:  lda $01
         ora #4
         sta $01

         cli
         rts

;---------------------------------------

sprsetup: ; load in sprites from data
         ldx #0
sprseta:
         lda spr1,x
         sta sprmem1,x
         lda spr2,x
         sta sprmem2,x
         lda spr3,x
         sta sprmem3,x
         lda spr4,x
         sta sprmem4,x

         inx
         cpx #64
         bne sprseta
sprsetz:  rts

twinkle:
         inc starcnt
         lda starcnt
         cmp #6      ; 2,4,6,8 stars
         bne twinka
         lda #0
         sta starcnt
       ; jmp twinka
twinka:
         ldx starcnt
         ldy star,x  ; y is star's loc

         txa
         and #%00000001
         cmp #0      ; odd stars in sf2
         bne twinkb
                     ; sf1
         lda sf1,y
         cmp #$2a    ; is it a star?
         bne twinka1 ; no
         lda #32     ; [space]
         sta sf1,y   ; erase old star
twinka1:
         jsr yprnd   ; y = random
         lda sf1,y
         cmp #32     ; is it [space]?
         bne twinkc  ; no (was twinka2)
         lda #$2a    ; star char
         sta sf1,y
         jmp twinkc
twinka2: ;jmp twinkc  ; done for this vb

twinkb:               ; sf2
         lda sf2,y
         cmp #$2a    ; is it a star?
         bne twinkb1 ; no
         lda #32     ; [space]
         sta sf2,y   ; erase old star
twinkb1:
         jsr yprnd   ; y = random
         lda sf2,y
         cmp #32     ; is it [space]?
         bne twinkc  ; no
         lda #$2a    ; star char
         sta sf2,y
twinkc:
         tya         ; move y back to
         sta star,x  ; star's loc

         jmp twinkz

twinkz:
         rts

;---------------------------------------

yprnd:    ; replace y with rand num

         tya
         beq prnddeor
         asl
         beq prnddeor
         bcc prndneor
prnddeor: eor #$1d   ; do eor
prndneor: tay        ; no eor

         rts

;---------------------------------------

hiscore:  .byte 0,0,0 ; 6-digit bcd
mspts:    .byte 0,0
goflag:   .byte 0
ssflag:   .byte 0
v30:      .byte 0
charcol:  .byte 0
ticcnt:   .byte 0     ; ticolrol count
scrcnt:   .byte 0,0
secs:     .byte 0
jifs:     .byte 0
scrjifs:  .byte 0
hits:     .byte 0,0   ; 4-digit bcd

attdec:   .byte 0
susrel:   .byte 0
volume:   .byte 0
hifreq:   .byte 0
lofreq:   .byte 0
wavefm:   .byte 0

p1x:      .byte 0,0
p1y:      .byte 0
p1d:      .byte 0
p1f:      .byte 0
p1score:  .byte 0,0,0 ; 6-digit bcd
p1col:    .byte 0
p1z:      .byte 0     ; p1 state
p1bf:     .byte 0     ; bump/bounce flag
l1x:      .byte 0,0
l1y:      .byte 0
l1s:      .byte 0

p2x:      .byte 0,0
p2y:      .byte 0
p2d:      .byte 0
p2f:      .byte 0
p2score:  .byte 0,0,0
p2col:    .byte 0
p2z:      .byte 0
p2bf:     .byte 0     ; bump/bounce flag
l2x:      .byte 0,0
l2y:      .byte 0
l2s:      .byte 0

msx:      .byte 0,0
msy:      .byte 0
msd:      .byte 0
mscol:    .byte 0
msmovs:   .byte 0   ; how many moves
msm:      .byte 0   ; move counter
mssut:    .byte 0   ; ms supeedup thresh
mssuc:    .byte 0   ; ms sp count
msrow:    .byte 0

j1:       .byte 0
j1z:      .byte 0
j2:       .byte 0
j2z:      .byte 0

star:     .byte 0,32,64,96       ;eight
         .byte 128,160,192,224  ;stars
starcnt:  .byte 0

r2ytab:   .byte 58,66,74,82,90,98,106
         .byte 114,122,130,0,0,0,0,0,0
         .byte 138,146,154,162,170,178
         .byte 186,194,202,210
         .byte 0,0,0,0,0,0
         .byte 218,226,234

scrtxt:   
	 .text upper-ascii "                "
         .text upper-ascii "                "
         .text upper-ascii "      press fire"
         .text upper-ascii " to play     fir"
         .text upper-ascii "e shoots and cha"
         .text upper-ascii "nges cannon dire"
         .text upper-ascii "ction     more p"
         .text upper-ascii "oints when 1nvad"
         .text upper-ascii "er is high up   "
         .text upper-ascii "  1nvader slows "
         .text upper-ascii "down after eight"
         .text upper-ascii "y hits     'darr"
         .text upper-ascii "enthefoulds     "
         .text upper-ascii "thx 'bedfordlvle"
         .text upper-ascii "xp     hi nate a"
         .text upper-ascii "nd tbone/       "

tichar:   .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$ff,$e1,$7b,$e1
         .byte $e1,$20,$e1,$20,$fe
         .byte $e1,$e2,$7b,$e1,$e2
         .byte $e2,$e1,$e2,$7f,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$e1,$e1,$7c,$fe
         .byte $e1,$6c,$7e,$ff,$e1
         .byte $e1,$20,$e1,$e1,$7c
         .byte $7e,$e1,$6c,$ff,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$e1,$e1,$20,$e1
         .byte $e1,$7e,$e1,$7c,$fb
         .byte $e1,$6c,$fe,$e1,$62
         .byte $62,$e1,$20,$e1,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$32,$30,$31,$39
         .byte $20,$44,$41,$52,$52
         .byte $45,$4e,$20,$46,$4f
         .byte $55,$4c,$44,$53,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20

mountc:   ; mountain screen view chars
         .byte $20,$5d,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$5d,$20,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$5d,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$20,$5d,$20

         .byte $5b,$5e,$5c,$5d,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$5d,$20,$20
         .byte $5b,$5e,$5c,$20,$20
         .byte $20,$20,$20,$20,$20
         .byte $5b,$5c,$5b,$5e,$5c
         .byte $5d,$20,$20,$20,$20
         .byte $20,$5d,$5b,$5e,$5c

         .byte $5d,$20,$20,$5c,$5c
         .byte $20,$20,$20,$20,$5b
         .byte $5c,$5b,$5e,$5c,$5b
         .byte $20,$20,$20,$5c,$20
         .byte $20,$20,$20,$20,$5b
         .byte $20,$20,$5c,$20,$5b
         .byte $5e,$5c,$20,$20,$20
         .byte $5b,$5e,$5c,$20,$5d

         .byte $5f,$5c,$20,$20,$20
         .byte $5c,$20,$20,$5b,$20
         .byte $20,$5c,$20,$5b,$20
         .byte $20,$20,$20,$20,$5c
         .byte $20,$20,$20,$5b,$20
         .byte $20,$20,$20,$20,$20
         .byte $20,$20,$5c,$20,$5b
         .byte $20,$20,$20,$5b,$5f

         .byte $40,$5f,$5f,$5f,$5f
         .byte $5f,$5f,$5f,$5f,$5f
         .byte $5f,$5f,$5f,$5f,$5f
         .byte $5f,$5f,$5f,$5f,$5f
         .byte $5f,$5f,$5f,$5f,$5f
         .byte $5f,$5f,$5f,$5f,$5f
         .byte $5f,$5f,$5f,$5f,$5f
         .byte $5f,$5f,$5f,$5f,$40

         .byte $5e,$5e,$5e,$5e,$5e
         .byte $5e,$5e,$5e,$5e,$5e
         .byte $5e,$5e,$5e,$5e,$5e
         .byte $5e,$5e,$5e,$5e,$5e
         .byte $5e,$5e,$5e,$5e,$5e
         .byte $5e,$5e,$5e,$5e,$5e
         .byte $5e,$5e,$5e,$5e,$5e
         .byte $5e,$5e,$5e,$5e,$5e

         ; custom characters
cdat1:
         .byte $00,$00,$00,$00 ;[?????].
         .byte $00,$00,$00,$00
         .byte $00,$01,$06,$01 ;thick 1.
         .byte $01,$01,$01,$00
         .byte $00,$1f,$00,$07 ;thick 2.
         .byte $18,$18,$1f,$00
         .byte $00,$1f,$00,$01 ;thick 3.
         .byte $00,$00,$1f,$00
         .byte $00,$80,$00,$80 ;thickr1.
         .byte $80,$80,$80,$00
         .byte $00,$e0,$18,$e0 ;thickr2.
         .byte $00,$00,$f8,$00
         .byte $00,$e0,$18,$e0 ;thickr3.
         .byte $18,$18,$e0,$00
         .byte $00,$38,$44,$54 ;' = @.
         .byte $5c,$40,$3c,$00
         .byte $ff,$38,$44,$38 ;(
         .byte $44,$44,$38,$ff
         .byte $ff,$38,$44,$44 ;)
         .byte $34,$04,$04,$ff
         .byte $10,$00,$10,$ba ;[star] .
         .byte $10,$00,$10,$00
         .byte $00,$01,$02,$03 ;+
         .byte $04,$05,$06,$07
         .byte $00,$00,$00,$00 ;,
         .byte $00,$00,$10,$20
         .byte $00,$00,$00,$7c ;-
         .byte $00,$00,$00,$00
         .byte $00,$00,$00,$00 ;.
         .byte $00,$00,$10,$00
         .byte $00,$10,$10,$10 ;/ = !
         .byte $10,$00,$10,$00

         .byte $00,$38,$44,$54 ;0
         .byte $54,$44,$38,$00
         .byte $00,$10,$20,$10 ;1
         .byte $10,$10,$10,$00
         .byte $00,$78,$04,$38 ;2
         .byte $40,$40,$7c,$00
         .byte $00,$78,$04,$18 ;3
         .byte $04,$04,$78,$00
         .byte $00,$44,$44,$44 ;4
         .byte $74,$04,$04,$00
         .byte $00,$7c,$40,$78 ;5
         .byte $04,$04,$78,$00
         .byte $00,$30,$40,$78 ;6
         .byte $44,$44,$38,$00
         .byte $00,$7c,$00,$08 ;7
         .byte $10,$10,$10,$00
         .byte $00,$38,$44,$38 ;8
         .byte $44,$44,$38,$00
         .byte $00,$38,$44,$44 ;9
         .byte $34,$04,$04,$00
         .byte $00,$01,$02,$03 ;:
         .byte $04,$05,$06,$07
         .byte $00,$01,$02,$03 ;;
         .byte $04,$05,$06,$07
         .byte $00,$01,$02,$03 ;<
         .byte $04,$05,$06,$07
         .byte $00,$01,$02,$03 ;=
         .byte $04,$05,$06,$07
         .byte $00,$01,$02,$03 ;>
         .byte $04,$05,$06,$07
         .byte $00,$01,$02,$03 ;?
         .byte $04,$05,$06,$07
cdat2:
         .byte $ff,$76,$2c,$34 ;bouncer.
         .byte $2c,$76,$ff,$00
         .byte $00,$04,$0c,$14 ;?
         .byte $24,$5c,$44,$00
         .byte $00,$78,$44,$58 ;?
         .byte $44,$44,$5c,$00
         .byte $00,$3c,$40,$40 ;?
         .byte $40,$40,$7c,$00
         .byte $00,$70,$48,$44 ;?
         .byte $44,$44,$5c,$00
         .byte $00,$7c,$40,$58 ;?
         .byte $40,$40,$7c,$00
         .byte $00,$7c,$40,$58 ;?
         .byte $40,$40,$40,$00
         .byte $00,$3c,$40,$40 ;?
         .byte $44,$44,$7c,$00
         .byte $00,$44,$44,$5c ;?
         .byte $44,$44,$44,$00
         .byte $00,$10,$10,$10 ;?
         .byte $10,$10,$10,$00
         .byte $00,$08,$08,$08 ;?
         .byte $08,$08,$08,$30
         .byte $00,$44,$44,$58 ;?
         .byte $44,$44,$44,$00
         .byte $00,$40,$40,$40 ;?
         .byte $40,$40,$7c,$00
         .byte $00,$44,$2c,$54 ;?
         .byte $44,$44,$44,$00
         .byte $00,$44,$64,$54 ;?
         .byte $4c,$44,$44,$00
         .byte $00,$38,$44,$44 ;?
         .byte $44,$44,$38,$00

         .byte $00,$78,$44,$44 ;?
         .byte $58,$40,$40,$00
         .byte $00,$38,$44,$44 ;?
         .byte $44,$40,$3c,$00
         .byte $00,$78,$44,$44 ;?
         .byte $58,$44,$44,$00
         .byte $00,$3c,$40,$38 ;?
         .byte $04,$04,$78,$00
         .byte $00,$7c,$00,$10 ;?
         .byte $10,$10,$10,$00
         .byte $00,$44,$44,$44 ;?
         .byte $44,$44,$38,$00
         .byte $00,$44,$44,$48 ;?
         .byte $50,$60,$40,$00
         .byte $00,$44,$44,$44 ;?
         .byte $54,$2c,$44,$00
         .byte $00,$44,$44,$18 ;?
         .byte $44,$44,$44,$00
         .byte $00,$44,$44,$44 ;?
         .byte $3c,$04,$38,$00
         .byte $00,$7c,$00,$08 ;?
         .byte $10,$20,$7c,$00
         .byte $01,$02,$06,$0a ;mnt l .
         .byte $18,$2a,$40,$a0
         .byte $80,$40,$a0,$30 ;mnt r .
         .byte $28,$04,$26,$05
         .byte $00,$00,$00,$00 ;mnt top.
         .byte $18,$3c,$6e,$ab
         .byte $ff,$11,$44,$00 ;grd lo .
         .byte $00,$00,$00,$00
         .byte $99,$42,$99,$40 ;rnd hi .
         .byte $aa,$55,$ff,$00

spr1:     ; mothership sprite
         .byte 0,126,0
         .byte 1,255,128
         .byte 3,255,192
         .byte 6,219,96
         .byte 15,255,240
         .byte 15,255,240
         .byte 3,153,192
         .byte 1,0,128
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,255

spr2:     ; cannon sprite
         .byte 0,8,0
         .byte 0,28,0
         .byte 0,28,0
         .byte 3,255,224
         .byte 7,255,240
         .byte 7,255,240
         .byte 7,255,240
         .byte 7,255,240
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,255

spr3:     ; laser sprite
         .byte 0,8,0
         .byte 0,4,0
         .byte 0,8,0
         .byte 0,16,0
         .byte 0,8,0
         .byte 0,4,0
         .byte 0,8,0
         .byte 0,8,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,255

spr4:     ; explosion
         .byte 0,66,0
         .byte 1,36,128
         .byte 0,129,0
         .byte 6,0,96
         .byte 0,129,0
         .byte 1,36,128
         .byte 0,66,0
         .byte 0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,0,0,0,0,0,0,0,0,0
         .byte 0,0,0,255
