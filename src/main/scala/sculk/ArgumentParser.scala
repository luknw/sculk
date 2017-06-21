package sculk


import org.rogach.scallop.{ScallopConf, ScallopOption, Subcommand}


class ArgumentParser(arguments: Seq[String]) extends ScallopConf(arguments) {

  val encode = new Subcommand("encode") {

    val driverPath: ScallopOption[String] = opt[String](required = true,
      descr = "Location of the .wav, which will be used to encode the message")

    val payloadPath: ScallopOption[String] = opt[String](required = true,
      descr = "Location of the message")

    val comboPath: ScallopOption[String] = opt[String](required = true,
      descr = "Path where the encoded .wav will be saved")

    val loadLevel: ScallopOption[Int] = opt[Int](default = Some(2),
      descr = "Determines the trade-off between capacity and sculkiness." +
        " Ranges from 1 = best sculkiness to 3 = best capacity." +
        " Defaults to 2.",
      validate = (level: Int) => 1 <= level && level <= 3)
  }

  val decode = new Subcommand("decode") {

    val comboPath: ScallopOption[String] = opt[String](required = true,
      descr = "Path of the .wav file to decode")

    val unloadPath: ScallopOption[String] = opt[String](required = true,
      descr = "Path of the restored message file")
  }

  val capacity = new Subcommand("capacity") {

    val driverPath: ScallopOption[String] = opt[String](required = true,
      descr = "Path of the .wav file to determine capacity for given load")

    val loadLevel: ScallopOption[Int] = opt[Int](default = Some(3),
      descr = "Load, for which to determine the capacity of the .wav file." +
        " Defaults to 3 = maximum capacity.",
      validate = (level: Int) => 1 <= level && level <= 3)
  }

  addSubcommand(encode)
  addSubcommand(decode)
  addSubcommand(capacity)

  verify()
}