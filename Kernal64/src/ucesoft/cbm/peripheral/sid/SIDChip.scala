package ucesoft.cbm.peripheral.sid

import java.io.{ObjectInputStream, ObjectOutputStream}

trait SIDChip {
  def clock : Unit
  def read(offset : Int) : Int
  def write(offset : Int,value : Int) : Unit
  def setModel(mode : Int) : Unit
  def output : Int

  def saveState(out:ObjectOutputStream) : Unit
  def loadState(in:ObjectInputStream) : Unit

  def reset : Unit

  def updateBusValue(value : Int) : Unit
}
