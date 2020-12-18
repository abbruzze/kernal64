/***********************************************************************
		Simple script demo for KickAssembler adapted for k64ass
***********************************************************************/

print "Hello World!"				// <- .print outputs to the console
print ""

print "Lets do a countdown:"
for(i=10; i>0; i--) 			// <- For and while loops works like in other programming languages
	print i

print ""


var x = 10.5						// <- This is how we define variables (we also did it in the for-loop)			
val y = 3						// <- y is a constant 
print "x*y-5=" + (x*y-5)
print ""

val list = ["Bill", "Joe", "Edgar"]		// <- We can also use structures like lists and hashtables

print "Who is at place number 2:" + list.get(2)
print ""


// Several commands can be written on the same line by using ';'
val a = 10
val b = 20
val c = sqrt(pow(a,2)+ pow(b,2))			// <- We also have the entire java math library
print "Pythagoras says that c is " + c
print ""

print "Sinus:"
var i=0
var spaceStr = "                          "
while (i++<10) {
	val x = 5+5*sin(i*2*3.14/10)
	print spaceStr.substring(0,x)+"o"	
}






