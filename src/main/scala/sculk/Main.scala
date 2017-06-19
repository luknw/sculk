package sculk

import sculk.sculkers.WavSculker


object Main {

  val payloadFilePath = "src/test/resources/pics.zip"
  val driverFilePath = "src/test/resources/foo.wav"
  val comboFilePath = "src/test/resources/sculkyFoo.wav"
  val unloadPath = "src/test/resources/unload.zip"

  def main(args: Array[String]): Unit = {
    println("Hello sculk")

    WavSculker.load(driverFilePath, payloadFilePath, comboFilePath, LoadLevel.Medium)
    WavSculker.unload(comboFilePath, unloadPath)
  }

}
