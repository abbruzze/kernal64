// ===========================================================================
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
// 	Matrix Example for Kick Assembler adapted for k64ass 
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
module matrixop {
	def vec_sum(a,b) {
		val size = a.size < b.size ? a.size : b.size
		var res = []
		for(i = 0;i < size;i++) {
			res = res + (a.get(i) + b.get(i))
		}
		eval res
	}

	def vec_scalar(a,b) {
		if (a.size != b.size) error "Cannot apply scalar product: the vectors must have same size"
		var sp = 0
		val size = a.size
		for(i = 0;i < size;i++) sp = sp + a.get(i) * b.get(i)
		eval sp
	}

	def matrix_id(size) {
		var mat = []
		for(r = 0;r < size;r++) {
			var row = []
			dup size row = row + 0
			row.set(r,1)
			mat = mat + row
		}
		eval mat
	}

	def matrix_make(a11,a12,a21,a22) {
		eval [[a11,a12],[a21,a22]]
	}

	def matrix_make(a11,a12,a13,a21,a22,a23,a31,a32,a33) {
		eval [[a11,a12,a13],[a21,a22,a23],[a31,a32,a33]]
	}

	def matrix_col(m,c) {
		if (c >= m.size) error "Cannot apply matrix column extraction: the column index must be less than the matrix's size"
		var col = []
		val size = m.size
		for(r = 0;r < size;r++) {
			col = col + m.get(r).get(c)
		}
		eval col
	}

	def matrix_vec_mul(m,v) {
		if (m.size != v.size) error "Cannot apply matrix/vector product: the matrice and vector must have same size"
		var row = []
		val size = m.size
		for(c = 0;c < size;c++) {
			val v1 = m.get(c)
			row = row + vec_scalar(v1,v)
		}
		eval row
	}

	def matrix_matrix_mul(a,b) {
		if (a.size != b.size) error "Cannot apply matrix/matrix product: the matrices must have same size"
		val size = a.size
		var mat = []
		for(r = 0;r < size;r++) {
			var row = []
			for(c = 0;c < size;c++) {
				val v1 = a.get(r)
				val v2 = matrix_col(b,c)
				row = row + vec_scalar(v1,v2)
			}
			mat = mat + row
		}
		eval mat
	}


	def matrix_3rot(xr,yr,zr) {
		val d1 = cos(xr)
		val d2 = sin(xr)
		val d3 = cos(yr)
		val d4 = sin(yr)
		val d5 = cos(zr)
		val d6 = sin(zr)
		eval [ [ d5 * d3, d5 * d4 * d2 - d6 * d1, d5 * d4 * d1 + d6 * d2, 0 ], 
			   [ d6 * d3, d6 * d4 * d2 + d5 * d1, d6 * d4 * d1 - d5 * d2, 0 ], 
			   [ -d4, d3 * d2, d3 * d1, 0 ], 
			   [ 0,0,0,1 ] 
			 ]
	}

	def matrix_3scale(sx,sy,sz) {
		eval [ [ sx,0,0,0 ], 
			   [ 0,sy,0,0 ], 
			   [ 0,0,sz,0 ], 
			   [ 0,0,0,1 ] 
			 ]
	}

	def matrix_3move(mx,my,mz) {
		eval [ [ 1,0,0,mx ], 
			   [ 0,1,0,my ], 
			   [ 0,0,1,mz ], 
			   [ 0,0,0,1 ] 
			 ]
	}

	def matrix_3per(p) {
		eval [ [ 1,0,0,0 ], 
			   [ 0,1,0,0 ], 
			   [ 0,0,0,0 ], 
			   [ 0,0,1 / p,0 ]
			 ]
	}
}


def macro BasicUpstart(address) {
    .word upstartEnd  // link address
    .word 10   // line num
    .byte $9e  // sys
    .text address.toString
    .byte 0
upstartEnd:
    .word 0  // empty link signals the end of the program
}

.pc = $0801 "Basic Upstart"
BasicUpstart($4000)

//----------------------------------------------------------
//----------------------------------------------------------
//		  Pre Calculated Vector
//----------------------------------------------------------
//----------------------------------------------------------


.pc = $4000 "Main Program"
			sei

			lda #<irq1
			sta $0314
			lda #>irq1
			sta $0315
			asl $d019
			lda #$7b
			sta $dc0d
			lda #$81
			sta $d01a
			lda #$1b
			sta $d011
			lda #$20
			sta $d012
			lda #$00
			sta $d020
			sta $d021
			jsr InitSprites

			cli
		 	jmp _
//----------------------------------------------------------
irq1:  	
			asl $d019
			inc $d020
			
			ldx frameNr
			jsr PlaceSprites
			inc frameNr
			lda #0
			sta $d020
			
			pla
			tay
			pla
			tax
			pla
			rti
frameNr: .byte 0

InitSprites: 
	lda #$ff	// Turn on Sprites
	sta $d015

	lda #$00	// Set single color
	sta $d01c 


	lda #$00	// No x and y expansion
	sta $d017	
	sta $d01d
	
	ldx #7
loop:
	lda #$f			// Set sprite color
	sta $d027,x	
	lda #$0fc0/$40	// Set sprite image
	sta $07f8,x
	dex
	bpl loop
	rts
	
PlaceSprites:

	for (i=0; i<8;i++) {
		lda cubeCoords+i*$200,x
		sta $d000+i*2		
		lda cubeCoords+$100+i*$200,x
		sta $d001+i*2		
	}
	rts	
	

//-----------------------------------------------------------------------------------------
// Objects 
//-----------------------------------------------------------------------------------------
val Cube = [[1,1,1,1],[1,1,-1,1],[1,-1,1,1],[1,-1,-1,1],[-1,1,1,1],[-1,1,-1,1],[-1,-1,1,1],[-1,-1,-1,1]]
//------------------------------------------------------------------------------------------
// Macro for doing the precalculation
//-----------------------------------------------------------------------------------------
def toRadians(d) { eval 2 * 3.14 * d / 360 }

def PrecalcObject(object, animLength, nrOfXrot, nrOfYrot, nrOfZrot) {

	// Rotate the coordinate and place the coordinates of each frams in a list
		var frames = []
		for(frameNr=0; frameNr<animLength;frameNr++) {
			// Set up the transform matrix
			val aX = toRadians(frameNr*360*nrOfXrot/animLength)
			val aY = toRadians(frameNr*360*nrOfYrot/animLength)
			val aZ = toRadians(frameNr*360*nrOfZrot/animLength)
			val zp = 2.5	// z coordinate for the projection plane
			val sm = matrixop::matrix_3scale(120,120,0)
			val mm = matrixop::matrix_3move(0,0,zp+5)
			val rm = matrixop::matrix_3rot(aX,aY,aZ)
			val pm = matrixop::matrix_3per(zp)
			val m = matrixop::matrix_matrix_mul(matrixop::matrix_matrix_mul(matrixop::matrix_matrix_mul(sm,pm),mm),rm)
	
			// Transform the coordinates		
			var coords = []
			for (i=0; i<object.size; i++) {
				val v = matrixop::matrix_vec_mul(m,object.get(i))
				coords = coords + v / v.get(3)
			}
			frames = frames + coords
		}

	// Dump the list to the memory
	for (coordNr=0; coordNr<object.size; coordNr++) {
		for (xy=0;xy<2; xy++) {
			.byte [i <- [0 .. animLength - 1] || $80+round(frames.get(i).get(coordNr).get(xy)) ]
		}
	}
}
//-------------------------------------------------------------------------------
// The vector data
//-----------------------------------------------------------------------------------------
.align $100
cubeCoords: 
eval PrecalcObject(Cube,256,2,-1,1)

//-------------------------------------------------------------------------------
// Sprite bob
//-----------------------------------------------------------------------------------------
.pc = $0fc0 "Sprite data"

	.byte %01110000, %00000000, %00000000
	.byte %11111000, %00000000, %00000000
	.byte %11111000, %00000000, %00000000
	.byte %01110000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000

