import org.rogach.scallop.{ScallopConf, ScallopOption, Subcommand}

/**
  * Created by lucas on 20/06/17.
  */
class ArgumentParser(arguments: Seq[String]) extends ScallopConf(arguments) {

  val encode = new Subcommand("encode") {
    val imagePath: ScallopOption[String] = opt[String](required = true,
      descr = "Location of the image which will be used to encoded the message. Has to end with .png")
    val msgPath: ScallopOption[String] = opt[String](required = true,
      descr = "Location of the message.")
    val outputPath: ScallopOption[String] = opt[String](required = true,
      descr = "Path where the encoded image will be saved. Has to end with .png")
    val level: ScallopOption[Int] = opt[Int](default = Some(0), descr = "How many bits per byte will be used to encode the message. " +
      "Can be from 1 to 8. By default 0 which means the optimal value will be chosen.",
      validate = (e: Int) => e >= 0 && e <= 8)

    validate(imagePath, outputPath) { (i, o) =>
      if(i.endsWith(".png") && o.endsWith(".png")) Right(Unit)
      else Left("image path and output path have to end with .png")
    }
  }

  val decode = new Subcommand("decode") {
    val encodedPath: ScallopOption[String] = opt[String](required = true,
      descr = "Location of the encoded image. Has to end with .png")
    val outputPath: ScallopOption[String] = opt[String](required = true,
      descr = "Path where the decoded message will be saved.")
    val level: ScallopOption[Int] = opt[Int](required = true, descr = "How many bits were " +
      "used per byte to encode the message. Has to be from 1 to 8.",
      validate = (e: Int) => e >= 1 && e <= 8)

    validate(encodedPath) { (e) =>
      if(e.endsWith(".png")) Right(Unit)
      else Left("encoded path has to end with .png")
    }
  }

  val display = new Subcommand("lowestBits") {
    val imagePath: ScallopOption[String] = opt[String](required = true,
      descr = "Location of the image. Has to end with .png")
    val outputPath: ScallopOption[String] = opt[String](required = true,
      descr = "Path where the new image will be saved. Has to end with .png")
    val level: ScallopOption[Int] = opt[Int](required = true,
      default = Some(8), descr = "How many bits of each pixel colour to display.",
      validate = (e: Int) => e >= 1 && e <= 8)

    validate(imagePath, outputPath) { (i, o) =>
      if(i.endsWith(".png") && o.endsWith(".png")) Right(Unit)
      else Left("image path and output path have to end with .png")
    }
  }

  addSubcommand(encode)
  addSubcommand(decode)
  addSubcommand(display)

  verify()
}
