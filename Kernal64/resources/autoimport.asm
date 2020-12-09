// VIC COLORS
val BLACK = 0
val WHITE = 1
val RED = 2
val CYAN = 3
val PURPLE = 4
val GREEN = 5
val BLUE = 6
val YELLOW = 7
val ORANGE = 8
val BROWN = 9
val LIGHT_RED = 10
val DARK_GRAY = 11
val DARK_GREY = 11
val GRAY = 12
val GREY = 12
val LIGHT_GREEN = 13
val LIGHT_BLUE = 14
val LIGHT_GRAY = 15
val LIGHT_GREY = 15

// Basic macro
def macro BasicUpstart(address,label) {
    *= $0801 "Basic"
  .byte $0c,$08,$0A,$00,$9e
  if (address >= 10000) { !byte 48 + ((address / 10000) % 10) }
  if (address >=  1000) { !byte 48 + ((address /  1000) % 10) }
  if (address >=   100) { !byte 48 + ((address /   100) % 10) }
  if (address >=    10) { !byte 48 + ((address /    10) % 10) }
  !byte $30 + (address % 10), $00, $00, $00
  
  *= address label
}


// matrix module
module matrix {
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
			eval row.set(r,1)
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
