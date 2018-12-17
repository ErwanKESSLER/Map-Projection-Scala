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

  def modifyImage(filename: String, source: Array[(Int, String, String, String, Double, Double)]) = {
    val util = new utils.utils
    var img = util.readImage(filename)
    val (width, height): (Int, Int) = (img.getWidth, img.getHeight)
    source.foreach(el=>{
      val (x, y): (Int, Int) = transformToXY(el._5, el._6, width, height)
      img=addRectangle(img,x,y)
    })
    util.writeImage(img, "final.png")
  }
  def addRectangle(source:BufferedImage,x:Int,y:Int): BufferedImage={
    for (i <- x - 5 until x + 5) {
      for (j <- y - 5 until y + 5) {
        if (i>=0 && i<source.getWidth() && j>=0 &&j<source.getHeight()){

          source.setRGB(i, j, 0xffff0000) //red
        }
      }
    }
    source
  }
}
