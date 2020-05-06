package ucesoft.cbm.expansion

trait ExpansionPortConfigurationListener {
	def expansionPortConfigurationChanged(game:Boolean,exrom:Boolean) : Unit
}