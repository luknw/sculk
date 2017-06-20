import java.io.{File, FileOutputStream}

import com.sksamuel.scrimage.Image

import scala.collection.mutable

/**
  * Created by lucas on 17/06/17.
  */
object scrimage_decode {

  private def getBit(number: Int, i: Int): Int = (number & (1 << i)) >>> i

  private def bitsToByte(bits: Iterable[Int]): Byte =
    bits.foldRight((0, 1))((e, a) => (e*a._2 + a._1, 2*a._2))._1.toByte

  private def byteToBits(byte: Byte): Iterable[Int] = for(i <- (0 to 7).toArray) yield getBit(byte, 7 - i)

  private def intToBits(int: Int): Iterable[Int] = for(i <- (0 to 31).toArray) yield getBit(int, 31 - i)

  private def takeNLowestBits(int: Int, n: Int): Iterable[Int] =
    for(i <- (0 until n).toArray) yield getBit(int, n - i - 1)

  private def substituteLowestNBits(bits: Array[Int], int: Int, n: Int): Int =
    (int >>> n) << n | bits.foldRight((0, 1))((e, a) => (e*a._2 + a._1, a._2*2))._1

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

    val size = bits.result.take(32).foldRight((0,1))((e, a) => (e*a._2+a._1, 2*a._2))._1

    val toConvert = bits.result.slice(32, size * 8 + 32).grouped(8).map(bitsToByte(_)).toArray

    println(size, intToBits(size).mkString(""))

    val decodedMessage = new FileOutputStream(outputPath)

    decodedMessage.write(toConvert)

    decodedMessage.close()
  }

  def main(args: Array[String]): Unit = {
    println("scrimage")

    decode(args(0), args(1), 1)

    println("Finished")

  }
}
