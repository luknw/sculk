package sculk.sculkers

import java.io._
import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

import sculk.LoadLevel.LoadLevel
import sculk.Main._
import sculk.utils.BitUtils._
import sculk.{LoadLevel, Sculker, WavFile, WavFileException}


object WavSculker extends Sculker {

  val bufferFrames = 1024 // minimum 9

  val payloadBitsPerFrame: Map[LoadLevel, Byte] = Map(
    LoadLevel.Low -> 2,
    LoadLevel.Medium -> 4,
    LoadLevel.High -> 8
  )

  // 1 byte for bits per frame, 8 bytes for payload frame count
  val metadataSize: Int = 1 + 8


  override def load(driverPath: String, payloadPath: String, comboPath: String,
                    loadLevel: LoadLevel = LoadLevel.Medium): Unit = {

    val payload = new BufferedInputStream(new FileInputStream(payloadPath))
    val payloadBytes = Files.size(Paths.get(payloadPath))

    ensureValidCapacity(driverPath, payloadPath, loadLevel, payloadBytes)

    val dataBitsPerFrame = payloadBitsPerFrame(loadLevel)
    val bitsPerFrame: Iterator[Int] =
      Iterator.fill(metadataSize)(8) ++ Iterator.continually(dataBitsPerFrame)

    val payloadIterator =
      makePayloadIterator(dataBitsPerFrame, payloadBytes, payload)

    try {
      val driver = WavFile.openWavFile(new File(driverPath))
      val combo = WavFile.newWavFile(new File(comboPath), driver.getNumChannels,
        driver.getNumFrames, driver.getValidBits, driver.getSampleRate)

      val numChannels = driver.getNumChannels
      val buffer = new Array[Long](bufferFrames * numChannels)

      var framesRead = 0
      do {
        framesRead = driver.readFrames(buffer, bufferFrames)

        buffer.transform(x =>
          if (!payloadIterator.hasNext) x
          else (x & lsZeros(bitsPerFrame.next())) | payloadIterator.next()
        )

        val framesWritten = combo.writeFrames(buffer, bufferFrames)

        if (framesWritten != framesRead) {
          throw new IOException(
            s"Written wav frames count doesn't match read frames count." +
              s" Read: $framesRead, written: $framesWritten.")
        }

      } while (framesRead != 0)

      combo.close()
      driver.close()
      payload.close()

    } catch {
      case e@(_: IOException | _: WavFileException) => System.err.println(e)
    }
  }

  private def ensureValidCapacity(driverPath: String, payloadPath: String,
                                  loadLevel: LoadLevel, payloadBytes: Long) = {

    val driverCapacity = getDataBytesCapacity(driverPath, loadLevel)

    if (driverCapacity == 0) {
      throw new IllegalArgumentException(
        s"File $driverPath too small to sculk anything"
      )
    } else if (payloadBytes > driverCapacity) {
      throw new IllegalArgumentException(
        s"File $payloadPath too big to sculk in $driverPath" +
          s" with load ${loadLevel.toString}")
    }
  }

  private def makePayloadIterator(bitsPerFrame: Byte,
                                  payloadBytes: Long,
                                  payload: InputStream): Iterator[Byte] = {

    val payloadFrames = ((payloadBytes * 8).toDouble / bitsPerFrame).ceil.toLong
    val lengthAsBytes = ByteBuffer.allocate(8).putLong(0, payloadFrames)

    val payloadLengthBits = lengthAsBytes.array().iterator.flatMap(msBitIterator)
    val bitsPerFrameBits = msBitIterator(bitsPerFrame)

    val metadataBits = bitsPerFrameBits ++ payloadLengthBits
    val metadataIterator = metadataBits.grouped(8).map(msAsByte)

    val dataBits = Iterator
      .continually(payload.read)
      .takeWhile(_ != -1)
      .flatMap(b => msBitIterator(b.toByte))

    val dataIterator = dataBits.grouped(bitsPerFrame).map(msAsByte)

    val payloadIterator = metadataIterator ++ dataIterator

    payloadIterator
  }

  override def unload(comboPath: String, unloadPath: String): Unit = {

    val unload = new BufferedOutputStream(new FileOutputStream(unloadPath))

    try {
      val combo = WavFile.openWavFile(new File(comboFilePath))

      val numChannels = combo.getNumChannels
      val buffer = new Array[Long](bufferFrames * numChannels)

      var framesRead = combo.readFrames(buffer, bufferFrames)

      val bitsPerFrame: Byte =
        buffer.take(1)
          .map(b => b & lsOnes(8))
          .head.toByte

      var payloadBytesLeft: Long =
        buffer.slice(1, 1 + 8)
          .map(b => b & lsOnes(8))
          .reduceLeft((value, byte) => (value << 8) | byte)

      val payloadBytesInBuffer =
        Math.min(payloadBytesLeft.toInt, buffer.length - metadataSize)

      buffer.slice(metadataSize, metadataSize + payloadBytesInBuffer)
        .map(b => b & lsOnes(bitsPerFrame))
        .foreach(b => unload.write(b.toInt))

      payloadBytesLeft -= payloadBytesInBuffer

      framesRead = combo.readFrames(buffer, bufferFrames)
      while (framesRead != 0) {
        val payloadByteCount = Math.min(payloadBytesLeft.toInt, buffer.length)

        val frameBytes =
          buffer.iterator
            .take(payloadByteCount)
            .map(_.toByte)

        val payloadBits =
          frameBytes.flatMap(
            b => msBitIterator(b).drop(8 - bitsPerFrame)
          )

        val payloadBytes =
          payloadBits.grouped(8).map(msAsByte)

        payloadBytes.foreach(b => unload.write(b))

        payloadBytesLeft -= payloadByteCount

        framesRead = combo.readFrames(buffer, bufferFrames)
      }

      combo.close()
      unload.close()

      if (payloadBytesLeft != 0) {
        throw new EOFException(
          s"Data sculking in $comboPath seems to be incomplete." +
            s" Missing $payloadBytesLeft bytes")
      }
    }

    catch {
      case e@(_: IOException | _: WavFileException) => System.err.println(e)
    }
  }

  override def getDataBytesCapacity(driverPath: String,
                                    loadLevel: LoadLevel = LoadLevel.High): Long = {

    val wavFile = WavFile.openWavFile(new File(driverPath))

    val allChannelsFrames = wavFile.getNumFrames * wavFile.getNumChannels
    val theoreticalCapacity =
      allChannelsFrames * payloadBitsPerFrame(loadLevel) / 8

    val capacity = Math.max(0, theoreticalCapacity - metadataSize)

    wavFile.close()

    capacity
  }

}
