package ucesoft.cbm.remote

trait RemoteC64 {
  def updateVideo(x1:Int,y1:Int,x2:Int,y2:Int) : Unit
  def isConnected : Boolean
  def stopRemoting() : Unit
}