import java.io.File
import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * Created by lucas on 17/06/17.
  */
object scrimage_encode {

  private def getBit(byte: Int, i: Int): Int = (byte & (1 << i)) >>> i

  private def byteToBits(byte: Byte): Iterable[Int] = for(i <- (0 to 7).toArray) yield getBit(byte, 7-i)

  private def intToBits(int: Int): Iterable[Int] = for(i <- (0 to 31).toArray) yield getBit(int, 31 - i)

  private def takeNLowestBits(int: Int, n: Int): Iterable[Int] =
    for(i <- (0 until n).toArray) yield getBit(int, n - i - 1)

  private def substituteLowestNBits(bits: Iterable[Int], number: Int, n: Int): Int =
    (number >>> n) << n | bits.foldRight((0,1))((e, a) => (e*a._2 + a._1, a._2*2))._1

  private def displayNLowestBits(image: Image, outputPath: String, n: Int): Unit = {
    displayNLowestBits(image, new File(outputPath), n)
  }

  private def displayNLowestBits(image: Image, outputFile: File, n: Int): Unit = {
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

  def encode(pathToOriginal: String, pathToMsg: String, pathToEncoded: String, encodeLevel: Int): Unit = {
    require(encodeLevel >= 1 && encodeLevel <= 8)

    val bytes = Files.readAllBytes(Paths.get(pathToMsg)).toStream
    val size = bytes.size
    var bits = (intToBits(size) ++ bytes.flatMap(byteToBits)).toList

    val image = Image.fromPath(Paths.get(pathToOriginal))

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
  }

  def main(args: Array[String]): Unit = {
    println("scrimage")

    encode(args(0), args(1), args(2), 1)

    displayNLowestBits(Image.fromPath(Paths.get(args(2))), "src/test/workspace/testoweEncoded.png", 2)
    displayNLowestBits(Image.fromPath(Paths.get(args(0))), "src/test/workspace/testowe.png", 2)

  }
}
