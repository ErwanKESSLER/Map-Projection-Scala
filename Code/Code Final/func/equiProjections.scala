package func

import java.awt.image.BufferedImage
import scala.math._


class equiProjections {
  //-------------------------------------Core Functions---------------------------------------------------//
  def whichProjection(typeOfFunction: String = "all", filename: String, typeOfShape: String, color: Int,
                      source: Either[Array[(Int, String, String, String, Double, Double)], (Double, Double)]): Unit = {
    filename match {
      case "cyndricalLambert.jpg" =>
        val conditions = (-90,90,-180,180)
        val tXYCyndr = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYCyndrLambert(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/"+filename, tXYCyndr, source, color, conditions, typeOfShape)
    }
  }
  def whichToCall(typeOfFunction: String, filename: String, transformToXY: (Double, Double, Int, Int) => (Int, Int),
                  source: Either[Array[(Int, String, String, String, Double, Double)], (Double, Double)], color: Int,
                  conditions: (Int, Int, Int, Int), typeOfShape: String): Unit = {
    typeOfFunction match {
      case "range" => showTheRangeOfTheProjection(filename, transformToXY, conditions, typeOfShape, color)
      case "all" => addAllAirportsToImage(filename, source.left.get, transformToXY, conditions, typeOfShape, color)
      case "one" => addOneSinglePoint(filename, source.right.get, transformToXY, conditions, typeOfShape, color)
      case whoa => println("You got the wrong guy, sorry : " + whoa.toString)
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

  def transformToXYCyndrLambert(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    (1,1)

  }

  //-------------------------------------------End of Transformations-------------------------------------------------//

  //--------------------------------------Start of Linear Transformations---------------------------------------------//

  //---------------------------------------End of Linear Transformations----------------------------------------------//

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
        if (abs(i - x) + abs(j - y) <= middle) {
          //circular condition
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
      addCircle(source, x, y, 2, color)
    }
  }

  //---------------------------------------------End of Shape Utilities-----------------------------------------------//

}
