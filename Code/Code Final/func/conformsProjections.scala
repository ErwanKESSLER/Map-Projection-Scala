package func

import java.awt.image.BufferedImage

import scala.math._

class conformsProjections {

  def toRadians(x:Double):Double={
    Pi*x/180
  }

  def linearTransformation(x:Double,y:Double):(Int,Int)={
    val scaling=353
    val (xShift,yshift)=(1030,710)
    (round(x*scaling+xShift).toInt,round(-y*scaling+yshift).toInt)
  }

  def transformToXY(lat: Double, lon: Double): (Int, Int) = {
    //lambert conic parameters
    val phi1=toRadians(46.5)
    val phi2=toRadians(49.0)
    val phi0=toRadians(44)
    //shifting on latitude depending of what lambert, here lambert93
    val l0=toRadians(0)
    val n=log(cos(phi1)/cos(phi2))/log(tan(Pi/4+phi2/2)/tan(Pi/4+phi1/2))*0.783
    val F=cos(phi1)*pow(tan(Pi/4+phi2/2),n)/n
    val p=F/pow(tan(Pi/4+toRadians(lat)/2),n)
    val p0=F/pow(tan(Pi/4+phi0/2),n)
    val (x,y)=(p*sin(n*(toRadians(lon)-l0)),p0-p*cos(n*(toRadians(lon)-l0)))
    linearTransformation(x,y)
  }

  def modifyImage(filename: String, source: Array[(Int, String, String, String, Double, Double)]): Unit = {
    val util = new utils.utils
    var img = util.readImage(filename)
    source.foreach(el => {
      //condition due to image being cropped at latitude of -30°
      if (el._5>= -30){
        val (x, y): (Int, Int) = transformToXY(el._5, el._6)
        img = addCircle(img, x, y, 9, 0xFFFF0000)
      }

    })
    util.writeImage(img, filename)
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
    util.writeImage(img, filename)
  }

  def addPoint(filename: String, lat: Double, lon: Double): Unit = {
    val util = new utils.utils
    var img = util.readImage(filename)
    val (x, y): (Int, Int) = transformToXY(lat, lon)
    img = addCircle(img, x, y, 9, 0xFFFF0000)
    util.writeImage(img, filename)
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
