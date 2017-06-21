/**
  * Created by lucas on 21/06/17.
  */
object BitFunctions {

  def getBit(num: Int, n: Int): Int = {
    require(n >= 0 && n <= 31)
    (num >>> n) & 1
  }

  def byteToBits(byte: Byte): Iterable[Int] = for(i <- 0 to 7) yield getBit(byte, 7-i)

  def bitsToByte(bits: Iterable[Int]): Byte =
    bits.foldRight((0, 1))((e, a) => (e*a._2 + a._1, 2*a._2))._1.toByte

  def intToBits(num: Int): Iterable[Int] = for(i <- 0 to 31) yield getBit(num, 31 - i)

  def takeNLowestBits(num: Int, n: Int): Iterable[Int] = {
    require(n >= 0 && n <= 31)
    for (i <- (0 until n).toArray) yield getBit(num, n - i - 1)
  }

  def substituteLowestNBits(bits: Iterable[Int], number: Int, n: Int): Int = {
    require(n <= bits.size)
    (number >>> n) << n | bits.foldRight((0, 1))((e, a) => (e * a._2 + a._1, a._2 * 2))._1
  }
}
