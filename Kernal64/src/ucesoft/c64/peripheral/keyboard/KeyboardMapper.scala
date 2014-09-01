package ucesoft.c64.peripheral.keyboard

trait KeyboardMapper {
	val map : Map[Int,CKey.Key]
	val cmap : Map[Char,(CKey.Key,Boolean)]
}