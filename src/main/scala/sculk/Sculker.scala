package sculk

import sculk.LoadLevel.LoadLevel


trait Sculker {

  def load(driverPath: String, payloadPath: String, comboPath: String,
           loadLevel: LoadLevel): Unit

  def unload(comboPath: String, unloadPath: String): Unit

  def getDataBytesCapacity(driverPath: String, loadLevel: LoadLevel): Long

}
