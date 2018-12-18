package func

import java.awt.image.BufferedImage
import scala.math._

class createImage {
  def transformToXY(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    (round((width / 2 * (1 + lon / 180)).floatValue), round((height / 2 * (1 - lat / 90)).floatValue))
  }

  def returnTRGB(color: Int): (Int, Int, Int, Int) = {
    (color & 0xff000000 / 16777216, (color & 0xff0000) / 65536, (color & 0xff00) / 256, color & 0xff)
  }

  def modifyImage(filename: String, source: Array[(Int, String, String, String, Double, Double)]): Unit = {
    val util = new utils.utils
    var img = util.readImage(filename)
    val (width, height): (Int, Int) = (img.getWidth, img.getHeight)
    source.foreach(el => {
      val (x, y): (Int, Int) = transformToXY(el._5, el._6, width, height)
      img = addCircle(img, x, y, 9, 0xFFFF0000)
    })
    util.writeImage(img, "final.png")
  }

  def addRectangle(source: BufferedImage, x: Int, y: Int, size: Int, color: Int): BufferedImage = {
    val middle = size / 2
    for (i <- x - middle until x + middle + size % 2) {
      for (j <- y - middle until y + middle + size % 2) {
        if (i >= 0 && i < source.getWidth() && j >= 0 && j < source.getHeight()) {

          source.setRGB(i, j, color)
        }
      }
    }
    source
  }

  def addCircle(source: BufferedImage, x: Int, y: Int, size: Int, color: Int): BufferedImage = {
    val middle = size / 2
    for (i <- x - middle until x + middle + size % 2) {
      for (j <- y - middle until y + middle + size % 2) {
        if (abs(i - x) + abs(j - y) <= middle) { //circular condition
          if (i >= 0 && i < source.getWidth() && j >= 0 && j < source.getHeight()) {
            source.setRGB(i, j, color)
          }
        }
      }
    }
    source
  }
}
