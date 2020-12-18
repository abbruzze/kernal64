BasicUpstart($810,"Basic")
//---------------------------------------------------------
//---------------------------------------------------------
// SID Player (Single speed) for KickAssmbler adapted for k64ass
//---------------------------------------------------------
//---------------------------------------------------------
val music = import("nightshift.sid","b").asSID		//<- Here we load the sid file

start:		lda #$00
			sta $d020
			sta $d021
			ldx #0
			ldy #0
			lda #1						//<- Here we get the startsong and init address from the sid file
			jsr music.init	
			if (music.play != 0) {
			sei
			lda #<irq1
			sta $0314
			lda #>irq1
			sta $0315
			lda #$1b
			sta $d011
			lda #$80
			sta $d012
			lda #$7f
			sta $dc0d
			sta $dd0d
			lda #$81
			sta $d01a
			lda $dc0d
			lda $dd0d
			asl $d019
			cli
			}
			jmp _

//---------------------------------------------------------
irq1:  	     asl $d019
			inc $d020
			jsr music.play 									// <- Here we get the play address from the sid file
			dec $d020
			jmp $ea81

//---------------------------------------------------------
			*= music.load "Music"
			.bytelist music.data 								// <- Here we put the music in memory

//----------------------------------------------------------
			// Print the music info while assembling
			
			print "SID Data"
			print "--------"
			print "location=$"+music.load.toHexString
			print "init=$"+music.init.toHexString
			print "play=$"+music.play.toHexString
			print "songs="+music.songs
			print "startSong="+music.startSong
			print "size=$"+music.size.toHexString
			print "name="+music.name
			print "author="+music.author
			print "copyright="+music.released

			print ""
			print "Additional tech data"
			print "--------------------"
			print "header="+music.magicID
			print "header version="+music.version
			print "flags="+music.flags.toBinaryString
			print "speed="+music.speed.toBinaryString

