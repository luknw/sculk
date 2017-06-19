package sculk.utils

object BitUtils {

  type Bit = Byte

  def lsZeros(zeroCount: Int): Long = {
    (-1).toLong << zeroCount.toLong
  }

  def lsOnes(oneCount: Int): Long = {
    ~((-1).toLong << oneCount.toLong)
  }

  def bitAt(byte: Byte, index: Int): Bit = {
    ((byte >>> index) & 1).asInstanceOf[Bit]
  }

  def msBitIterator(byte: Byte): Iterator[Bit] = {
    Iterator.tabulate(8)(i => bitAt(byte, 7 - i))
  }

  def msAsByte(bits: Iterable[Bit]): Byte = {
    bits
      .take(8)
      .foldLeft(0.toByte)((byte, bit) => ((byte << 1) | bit).toByte)
  }

}
