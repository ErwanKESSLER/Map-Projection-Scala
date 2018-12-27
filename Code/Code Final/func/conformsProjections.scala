package func

import java.awt.image.BufferedImage

import scala.math._

class conformsProjections {
  //---------------------------------------------Core Functions-------------------------------------------------------//
  def whichProjection[T >: Array[(Int, String, String, String, Double, Double)] with (Double, Double)]
  (typeOfFunction: String = "all", filename: String, typeOfShape: String, color: Int, source: T): Unit = {
    filename match {
      case "mercator.jpg" => {
        val conditions = (-82, 82, -180, 180)
        val tXYMerc = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYMercator(lat, lon, width, height)
        whichToCall(typeOfFunction, filename, tXYMerc, source, color, conditions, typeOfShape)
      }
      case "lambertConic.jpg" => {
        val conditions = (-30, 90, -180, 180)
        val tXYLamCon = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYLambertConic(lat, lon, width, height)
        whichToCall(typeOfFunction, filename, tXYLamCon, source, color, conditions, typeOfShape)
      }
      case whoa => println("You got the wrong guy, sorry : " + whoa.toString)
    }
  }

  def whichToCall[T >: Array[(Int, String, String, String, Double, Double)] with (Double, Double)]
  (typeOfFunction: String, filename: String, transformToXY: (Double, Double, Int, Int) => (Int, Int), source: T,
   color: Int, conditions: (Int, Int, Int, Int), typeOfShape: String): Unit = {
    source match {
      case s: Array[(Int, String, String, String, Double, Double)] => {
        typeOfFunction match {
          case "range" => {
            showTheRangeOfTheProjection(filename, transformToXY, conditions, typeOfShape, color)
          }
          case "all" => {
            addAllAirportsToImage(filename, s, transformToXY, conditions, typeOfShape, color)
          }
          case whoa => println("You got the wrong guy, sorry : " + whoa.toString)
        }
      }
      case s: (Double, Double) => addOneSinglePoint(filename, s, transformToXY, conditions, typeOfShape, color)
    }

  }

  //-------------------------------------------End of Core Functions--------------------------------------------------//

  //----------------------------------------Type of Image Modifications-----------------------------------------------//
  def addAllAirportsToImage(filename: String, source: Array[(Int, String, String, String, Double, Double)],
                            transformToXY: (Double, Double, Int, Int) => (Int, Int), conditions: (Int, Int, Int, Int),
                            typeOfShape: String, color: Int): Unit = {
    val util = new utils.utils
    var img = util.readImage(filename)
    val (width, height): (Int, Int) = (img.getWidth, img.getHeight)
    source.foreach(el => {
      //condition due to image being cropped at latitude of -30°
      if (el._5 >= conditions._1 && el._5 <= conditions._2 && el._6 >= conditions._3 && el._6 <= conditions._4) {
        //Getting the right (x,y) according to the transformation chosen
        val (x, y): (Int, Int) = transformToXY(el._5, el._6, width, height)
        //Type of Shape
        img = shapeIt(img, x, y, color, typeOfShape)
      }
    })
    util.writeImage(img, filename)
  }

  def showTheRangeOfTheProjection(filename: String, transformToXY: (Double, Double, Int, Int) => (Int, Int),
                                  conditions: (Int, Int, Int, Int), typeOfShape: String, color: Int): Unit = {
    val util = new utils.utils
    var img = util.readImage(filename)
    val (width, height): (Int, Int) = (img.getWidth, img.getHeight)
    for (i <- conditions._3 to conditions._4) {
      for (j <- conditions._1 to conditions._2) {
        val (x, y): (Int, Int) = transformToXY(j, i, width, height)
        img = shapeIt(img, x, y, color, typeOfShape)
      }
    }
    util.writeImage(img, filename)
  }

  def addOneSinglePoint(filename: String, source: (Double, Double), transformToXY: (Double, Double, Int, Int) => (Int, Int),
                        conditions: (Int, Int, Int, Int), typeOfShape: String, color: Int): Unit = {
    val (lat, lon) = source
    val util = new utils.utils
    var img = util.readImage(filename)
    val (width, height): (Int, Int) = (img.getWidth, img.getHeight)
    val (x, y): (Int, Int) = transformToXY(lat, lon, width, height)
    if (lat >= conditions._1 && lat <= conditions._2 && lon >= conditions._3 && lon <= conditions._4) {
      img = shapeIt(img, x, y, color, typeOfShape)
      util.writeImage(img, filename)
    }
    else {
      println("The point you are trying to draw is outside the scope of the projection, try another one or change projection.")
    }
  }

  //------------------------------------End of Type of Image Modifications--------------------------------------------//

  //-----------------------------------------------Transformations----------------------------------------------------//
  def toRadians(x: Double): Double = {
    Pi * x / 180
  }

  def linearTransformationLambertConic(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 353
    val (xShift, yshift) = (1030, 710)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def transformToXYLambertConic(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //lambert conic parameters
    val phi1 = toRadians(46.5)
    val phi2 = toRadians(49.0)
    val phi0 = toRadians(44)
    //shifting on latitude depending of what lambert, here lambert93
    val l0 = toRadians(0)
    val n = log(cos(phi1) / cos(phi2)) / log(tan(Pi / 4 + phi2 / 2) / tan(Pi / 4 + phi1 / 2)) * 0.783
    val F = cos(phi1) * pow(tan(Pi / 4 + phi2 / 2), n) / n
    val p = F / pow(tan(Pi / 4 + toRadians(lat) / 2), n)
    val p0 = F / pow(tan(Pi / 4 + phi0 / 2), n)
    val (x, y) = (p * sin(n * (toRadians(lon) - l0)), p0 - p * cos(n * (toRadians(lon) - l0)))
    linearTransformationLambertConic(x, y, width, height)
  }

  def linearTransformationMercator(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 325
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def transformToXYMercator(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    val λ0 = 0
    val x = toRadians(lon - λ0)
    val y = log(tan(Pi / 4 + toRadians(lat) / 2))
    linearTransformationMercator(x, y, width, height)
  }

  //-------------------------------------------End of Transformations-------------------------------------------------//


  //------------------------------------------------Shape Utilities---------------------------------------------------//
  def addSquare(source: BufferedImage, x: Int, y: Int, size: Int, color: Int): BufferedImage = {
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

  def shapeIt(source: BufferedImage, x: Int, y: Int, color: Int, typeOfShape: String): BufferedImage = {
    if (typeOfShape == "circle") {
      addCircle(source, x, y, 9, color)
    }
    else if (typeOfShape == "square") {
      addSquare(source, x, y, 5, color)
    }
    else {
      addCircle(source, x, y, 0, color)
    }
  }

  //---------------------------------------------End of Shape Utilities-----------------------------------------------//
}
