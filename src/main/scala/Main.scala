import java.io.IOException
import java.nio.file.{NoSuchFileException, Paths}

import com.sksamuel.scrimage.Image

/**
  * Created by lucas on 21/06/17.
  */
object Main extends App{

  val conf = new ArgumentParser(args)

  try {
    if (conf.subcommands.isEmpty) {
      println("Incorrect input. For help use the option --help.")
    } else if (conf.encode.imagePath.isSupplied) {
      println("Encoding level used was " +
        PNGSculker.encode(conf.encode.imagePath(), conf.encode.msgPath(),
          conf.encode.outputPath(), conf.encode.level()))
    } else if (conf.decode.encodedPath.isSupplied) {
      PNGSculker.decode(conf.encode.imagePath(), conf.decode.outputPath(),
        conf.encode.level())
    } else if (conf.display.imagePath.isSupplied) {
      PNGSculker.displayNLowestBits(Image.fromPath(Paths.get(conf.display.imagePath())),
        conf.display.outputPath(), conf.display.level())
    }
  } catch {
    case e: NoSuchFileException => println("Couldn't find the file " + e.getMessage)
    case e: IOException => println("An error occurred during reading or writing to a file " + e.getMessage)
  }
}
