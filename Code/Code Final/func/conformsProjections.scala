package func

import java.awt.image.BufferedImage

import scala.math._

class conformsProjections {

  def toRadians(x:Double):Double={
    Pi*x/180
  }

  def transformToXY(lat: Double, lon: Double): (Int, Int) = {
    //lambert conic
    val phi1=toRadians(46.5)
    val phi2=toRadians(49.0)
    val phi0=toRadians(44)
    val l0=toRadians(0)
    val n=log(cos(phi1)/cos(phi2))/log(tan(Pi/4+phi2/2)/tan(Pi/4+phi1/2))*0.783
    val F=cos(phi1)*pow(tan(Pi/4+phi2/2),n)/n
    val p=F/pow(tan(Pi/4+toRadians(lat)/2),n)
    val p0=F/pow(tan(Pi/4+phi0/2),n)
    (round((p*sin(n*(toRadians(lon)-l0))*353+1030)).toInt,round((p0-p*cos(n*(toRadians(lon)-l0)))*(-353)+710).toInt)
  }

  def returnTRGB(color: Int): (Int, Int, Int, Int) = {
    (color & 0xff000000 / 16777216, (color & 0xff0000) / 65536, (color & 0xff00) / 256, color & 0xff)
  }

  def modifyImage(filename: String, source: Array[(Int, String, String, String, Double, Double)]): Unit = {
    val util = new utils.utils
    var img = util.readImage(filename)
    source.foreach(el => {
      val (x, y): (Int, Int) = transformToXY(el._5, el._6)
      img = addCircle(img, x, y, 9, 0xFFFF0000)
    })
    util.writeImage(img, "final.jpg")
  }

  def showTrace(filename:String): Unit ={
    val util = new utils.utils
    var img = util.readImage(filename)
    for (i<- -180 to 180){
      for (j<- -30 to 90){
        val (x, y): (Int, Int) = transformToXY(j, i)
        img = addCircle(img, x, y, 9, 0xFFFF0000)
      }
    }
    util.writeImage(img, "final.jpg")
  }
  def addPoint(filename: String, lat:Double,lon:Double):Unit={
    val util = new utils.utils
    var img = util.readImage(filename)
    val (x, y): (Int, Int) = transformToXY(lat, lon)
    img = addCircle(img, x, y, 9, 0xFFFF0000)
    util.writeImage(img, "final.jpg")
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
