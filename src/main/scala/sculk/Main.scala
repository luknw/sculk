package sculk

import sculk.sculkers.WavSculker


object Main {

  def main(args: Array[String]): Unit = {
    val conf = new ArgumentParser(args)

    if (conf.subcommands.isEmpty) {

      println("Incorrect input. For help use the option --help.")

    } else if (conf.encode.driverPath.isSupplied) {

      val loadLevel = LoadLevel(conf.encode.loadLevel() - 1)
      println("Encoding level used was " + loadLevel)

      WavSculker.load(conf.encode.driverPath(), conf.encode.payloadPath(),
        conf.encode.comboPath(), loadLevel)

    } else if (conf.decode.comboPath.isSupplied) {

      WavSculker.unload(conf.decode.comboPath(), conf.decode.unloadPath())

    } else if (conf.capacity.driverPath.isSupplied) {

      val loadLevel = LoadLevel(conf.capacity.loadLevel() - 1)
      println("Encoding level used was " + loadLevel)

      println(WavSculker.getDataBytesCapacity(conf.capacity.driverPath(), loadLevel).toString + " B")

    }
  }

}
