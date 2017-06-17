import java.io.FileOutputStream
import java.nio.file.{Files, Paths}

import ps.tricerato.pureimage.{Input, Output, PNG, RGB, RGBImage}

/**
  * sculk
  * Created by luknw on 2017-06-17
  */

object Sculk extends {
  def main(args: Array[String]): Unit = {
    println("Hello sculk")

    val imageByteArray = Files.readAllBytes(Paths.get("src/test/resources/foo.jpg"))
    val Right(RGBImage(image)) = Input(imageByteArray)

    val rainbowy = image.map((x, y) => {
      val oldPixel = image(x, y)
      RGB.fromChannels(
        oldPixel.red | (1 << 3),
        oldPixel.green | (1 << 5),
        oldPixel.blue | (1 << 7))
    })

    val outputFile = new FileOutputStream("src/test/workspace/rainbowyFoo.png")
    outputFile.write(Output(rainbowy, PNG))
    outputFile.close()
  }

}
