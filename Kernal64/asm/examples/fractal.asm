// ===========================================================================
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// 	Bitmap Fractal Example for Kick Assembler adapted for k64ass 
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
val imCenter = 0
val reCenter = 0
val zoom = 2        // lower = more zoom

//----------------------------------------------------------------------------
// 				Display the fractal 
//----------------------------------------------------------------------------

BasicUpstart($0810,"Display Program")

	lda #$18
	sta $d018
	lda #$d8
	sta $d016
	lda #$3b
	sta $d011
	
	lda #BLACK
	sta $d021
	sta $d020
	ldx #0
loop:
	for (i=0; i<4; i++) {
		lda #GRAY | LIGHT_GRAY<<4 
		sta $0400+$100*i,x
		lda #DARK_GRAY
		sta $d800+$100*i,x
	}
	inx
	bne loop
	jmp _

//----------------------------------------------------------------------------
// 				Calculate the fractal 
//----------------------------------------------------------------------------
.pc = $2000 "Fractal Image"
val colors = [%11,%01,%10,%00]
def mandelbrot(re,im) {
	var zr = 0
	var zi = 0 
	var iter=0
	for(;(zr*zr+zi*zi)<4 & iter<18;iter++) {
		var newZr = zr*zr-zi*zi + re
		var newZi = 2*zr*zi + im
		zr = newZr
		zi = newZi
	}
	eval colors.get(iter&3) 
} 

def map(x, width, targetCenter, targetWidth) {
	eval (targetCenter-targetWidth/2) + targetWidth * (x/(width-1))
}

def fillFractalData() {
  var fractalData = []
  for (screenY=0; screenY<25; screenY++)
    for (screenX=0; screenX<40; screenX++)
      for (charY=0; charY<8; charY++) {
        var byteValue = $00
	for (charX=0; charX<4; charX++) {
	  var x = charX+screenX*4
	  var y = charY+screenY*8
	  var re = map(x,160,reCenter,zoom)
	  var im = map(y,200,imCenter,zoom)
	  byteValue = byteValue | (mandelbrot(re,im)<<(6-charX*2))	  
	}
	fractalData = fractalData + byteValue
     }
  eval fractalData
}

val ts = time()
print "Filling..."
val fractalData = fillFractalData()
.byte [b <- [0 .. 8*25*40-1] || fractalData.get(b)]
print "Elapsed " + ((time() - ts) / 1000) + " seconds"
