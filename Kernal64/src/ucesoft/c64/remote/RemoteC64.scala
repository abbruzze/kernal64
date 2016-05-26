package ucesoft.c64.remote

trait RemoteC64 {
  def updateVideo(x1:Int,y1:Int,x2:Int,y2:Int)
  def isConnected : Boolean
  def stopRemoting
}