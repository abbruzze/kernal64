package ucesoft.cbm.peripheral.keyboard

trait KeyboardMapper {
	val map : Map[Int,CKey.Key]
	val cmap : Map[Char,(CKey.Key,Boolean)]
	val keypad_map : Map[Int,CKey.Key]
}