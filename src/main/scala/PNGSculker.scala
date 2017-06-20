import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.mutable
import BitFunctions.{takeNLowestBits, _}
/**
  * Created by lucas on 21/06/17.
  */
object PNGSculker {

  def decode(pathToEncoded: String, outputPath: String, encodeLevel: Int): Unit = {
    require(encodeLevel >= 1 && encodeLevel <= 8)

    val bits = new mutable.ArrayBuilder.ofInt
    val encodedFile = new File(pathToEncoded)
    val image = Image.fromFile(encodedFile)

    image.foreach((_, _, p) => {
      bits ++= takeNLowestBits(p.red, encodeLevel)
      bits ++= takeNLowestBits(p.green, encodeLevel)
      bits ++= takeNLowestBits(p.blue, encodeLevel)
    })

    val result = bits.result()

    val size = result.take(32).foldRight((0,1))((e, a) => (e*a._2+a._1, 2*a._2))._1

    val toConvert = result.slice(32, size * 8 + 32).grouped(8).map(bitsToByte(_)).toArray

    val decodedMessage = new FileOutputStream(outputPath)

    decodedMessage.write(toConvert)

    decodedMessage.close()
  }

  def getNumberOfPixels(image: Image): Int = image.height * image.width

  def displayNLowestBits(image: Image, outputFile: String, n: Int): Unit = {
    require(n >= 1 && n <= 8)

    val lowestBits = Image(image.width, image.height)

    for(x <- 0 until image.width; y <- 0 until image.height){
      val p = image.pixel(x, y)
      val rgb = List(p.red, p.green, p.blue).
        map(takeNLowestBits(_, n).
          foldRight((0,1))((e, a) => (e*a._2 + a._1, a._2*2))._1)
        .map(_ * 255 / (1 << n))

      lowestBits.setPixel(x, y, Pixel(rgb(0), rgb(1), rgb(2), p.alpha))
    }

    lowestBits.output(outputFile)
  }

  def encode(pathToOriginal: String, pathToMsg: String, pathToEncoded: String, noOfBits: Int): Int = {
    require(noOfBits >= 0 && noOfBits <= 8)

    val bytes = Files.readAllBytes(Paths.get(pathToMsg))
    val size = bytes.size
    var bits = (intToBits(size) ++ bytes.flatMap(byteToBits)).toList

    val image = Image.fromPath(Paths.get(pathToOriginal))

    val encodeLevel = if(noOfBits != 0) {
      noOfBits
    } else {
      (1 to 8).filter(getNumberOfPixels(image)*3*_/8+1 >= size).fold(8)(math.min)
    }

    require(getNumberOfPixels(image) * 3 * encodeLevel / 8 + 1 >= size)

    image.foreach((x, y, p) => bits match {
      case a if a.nonEmpty =>
        val red = substituteLowestNBits(bits.take(encodeLevel), p.red, encodeLevel)
        bits = bits.drop(encodeLevel)
        val green = substituteLowestNBits(bits.take(encodeLevel), p.green, encodeLevel)
        bits = bits.drop(encodeLevel)
        val blue = substituteLowestNBits(bits.take(encodeLevel), p.blue, encodeLevel)
        bits = bits.drop(encodeLevel)
        image.setPixel(x, y, Pixel(red, green, blue, p.alpha))
      case _ => Unit
    })

    image.output(pathToEncoded)

    encodeLevel
  }

}
