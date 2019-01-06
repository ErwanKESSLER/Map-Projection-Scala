package func

import java.awt.image.BufferedImage

import scala.math._

class equalAreaProjections {
  //---------------------------------------------Core Functions-------------------------------------------------------//

  def whichProjection(typeOfFunction: String = "all", filename: String, typeOfShape: String, color: Int,
                      source: Either[Array[(Int, String, String, String, Double, Double)], (Double, Double)]): Unit = {
    filename match {
      case "lambertCylindric.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYLamCyl = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYLambertCylindric(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYLamCyl, source, color, conditions, typeOfShape)
      case "behrmann.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYBehr = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYBehrmann(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYBehr, source, color, conditions, typeOfShape)
      case "eckertI.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYEckI = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYEckertI(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYEckI, source, color, conditions, typeOfShape)
      case "eckertII.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYEckII = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYEckertII(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYEckII, source, color, conditions, typeOfShape)
      case "eckertIII.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYEckIII = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYEckertIII(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYEckIII, source, color, conditions, typeOfShape)
      case "eckertIV.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYEckIV = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYEckertIV(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYEckIV, source, color, conditions, typeOfShape)
      case "eckertV.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYEckV = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYEckertV(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYEckV, source, color, conditions, typeOfShape)
      case "eckertVI.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYEckVI = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYEckertVI(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYEckVI, source, color, conditions, typeOfShape)
      case "gallPeters.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYGaPe = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYGallPeters(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYGaPe, source, color, conditions, typeOfShape)
      case "hoboDyer.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYHoDy = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYHoboDyer(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYHoDy, source, color, conditions, typeOfShape)
      case "mollweide.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYMoll = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYMollweide(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYMoll, source, color, conditions, typeOfShape)
      case "sinusoidal.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYSinu = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYSinusoidal(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYSinu, source, color, conditions, typeOfShape)
      case "goode.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYGood = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYGoode(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYGood, source, color, conditions, typeOfShape)
      case "balthasart.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYBalt = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYBalthasart(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYBalt, source, color, conditions, typeOfShape)
      case "toblersWIS.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYTWIS = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYToblersWIS(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/EqualArea/" + filename, tXYTWIS, source, color, conditions, typeOfShape)


      case whoa => println("You got the wrong guy, sorry : " + whoa.toString)
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

  //-----------------------------------------Start of helpers Functions-----------------------------------------------//
  def toRadians(x: Double): Double = {
    Pi * x / 180
  }

  def rotation(alpha: Double, x: Double, y: Double): (Double, Double) = {
    (x * cos(toRadians(alpha)) - y * sin(toRadians(alpha)), y * cos(toRadians(alpha)) + x * sin(toRadians(alpha)))
  }

  //---------------------------------------------End of helpers Functions---------------------------------------------//

  //-----------------------------------------------Transformations----------------------------------------------------//

  def transformToXYLambertCylindric(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Lambert Cylindrical: https://en.wikipedia.org/wiki/Lambert_cylindrical_equal-area_projection
    val (λ0, φ0) = (0, 0)
    val x = toRadians(lon - λ0) * cos(toRadians(φ0))
    val y = sin(toRadians(lat)) / cos(toRadians(φ0))
    linearTransformationLambertCylindric(x, y, width, height)
  }

  def transformToXYBehrmann(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Behrmann 30 : https://en.wikipedia.org/wiki/Behrmann_projection
    val (λ0, φ0) = (0, 30)
    val x = toRadians(lon - λ0) * cos(toRadians(φ0))
    val y = sin(toRadians(lat)) / cos(toRadians(φ0))
    linearTransformationBehrmann(x, y, width, height)
  }

  def transformToXYEckertI(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Eckert I: https://fr.wikipedia.org/wiki/Projection_Eckert_I
    //https://github.com/d3/d3-geo-projection/blob/master/src/eckert1.js
    val (lambda, phi) = (toRadians(lon), toRadians(lat))
    val alpha = sqrt(8 / (3 * Pi))
    val x = alpha * lambda * (1 - abs(phi) / Pi)
    val y = alpha * phi
    linearTransformationEckertI(x, y, width, height)
  }

  def transformToXYEckertII(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Eckert II: https://en.wikipedia.org/wiki/Eckert_II_projection
    //https://github.com/d3/d3-geo-projection/blob/master/src/eckert2.js
    val (lambda, phi) = (toRadians(lon), toRadians(lat))
    val alpha = sqrt(4 - 3 * sin(abs(phi)))
    val x = 2 / sqrt(6 * Pi) * lambda * alpha
    val y = signum(phi) * sqrt(2 * Pi / 3) * (2 - alpha)
    linearTransformationEckertII(x, y, width, height)
  }

  def transformToXYEckertIII(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Eckert III: http://wiki.gis.com/wiki/index.php/Eckert_III_projection
    //https://github.com/d3/d3-geo-projection/blob/master/src/eckert3.js
    val (lambda, phi) = (toRadians(lon), toRadians(lat))
    val alpha = sqrt(Pi * (4 + Pi))
    val x = 2 / alpha * lambda * (1 + sqrt(1 - 4 * phi * phi / (Pi * Pi)))
    val y = 4 / alpha * phi
    linearTransformationEckertIII(x, y, width, height)
  }

  def transformToXYEckertIV(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Eckert IV: https://en.wikipedia.org/wiki/Eckert_IV_projection
    //https://github.com/d3/d3-geo-projection/blob/master/src/eckert4.js
    //https://github.com/OSGeo/proj.4/blob/32f3ef47e55c38b0eabb6d781fee3944d3239414/src/projections/eck4.cpp
    var (lambda, phi) = (toRadians(lon), toRadians(lat))
    val k = (2 + Pi / 2) * sin(phi)
    val delta = phi * phi
    var (flag,i) = (true,0)
    while (i <10 && flag) {
      val (c, s) = (cos(phi), sin(phi))
      val delta = (phi + s * (c + 2) - k) / (2 * c * (1 + c))
      phi -= delta
      if (abs(delta) < 1e-7) {
        flag = false
      }
      i += 1
    }
    val x = 2 / sqrt(Pi*(4+Pi)) * lambda * (1+ cos(phi))
    val y = 2*sqrt(Pi/(4+Pi))*sin(phi)
    linearTransformationEckertIV(x, y, width, height)
  }

  def transformToXYEckertV(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Eckert V: http://desktop.arcgis.com/fr/arcmap/10.3/guide-books/map-projections/eckert-v.htm
    //https://github.com/d3/d3-geo-projection/blob/master/src/eckert5.js
    val (lambda, phi) = (toRadians(lon), toRadians(lat))
    val x = lambda * (1 + cos(phi)) / sqrt(2 + Pi)
    val y = 2 * phi / sqrt(2 + Pi)
    linearTransformationEckertV(x, y, width, height)
  }

  def transformToXYEckertVI(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Eckert VI: https://upload.wikimedia.org/wikipedia/commons/9/90/Ecker_VI_projection_SW.jpg
    //https://github.com/d3/d3-geo-projection/blob/master/src/eckert6.js
    var (lambda, phi) = (toRadians(lon), toRadians(lat))
    var k = (1 + Pi / 2) * sin(phi)
    val delta = phi * phi
    var (flag,i) = (true,0)
    while (i <10 && flag) {
      val (c, s) = (cos(phi), sin(phi))
      val delta = (phi + s - k) / (1 + c)
      phi -= delta
      if (abs(delta) < 1e-7) {
        flag = false
      }
      i += 1
    }
    k=sqrt(2+Pi)
    val x =  lambda * (1 + cos(phi)) / k
    val y =2 * phi / k
    linearTransformationEckertVI(x, y, width, height)
  }

  def transformToXYGallPeters(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Gall–Peters: https://en.wikipedia.org/wiki/Gall%E2%80%93Peters_projection
    val (λ0, φ0) = (0, 45)
    val x = toRadians(lon - λ0) * cos(toRadians(φ0))
    val y = sin(toRadians(lat)) / cos(toRadians(φ0))
    linearTransformationGallPeters(x, y, width, height)
  }

  def transformToXYHoboDyer(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Hobo-Dyer: https://en.wikipedia.org/wiki/Hobo%E2%80%93Dyer_projection
    val (λ0, φ0) = (0, 37.5)
    val x = toRadians(lon - λ0) * cos(toRadians(φ0))
    val y = sin(toRadians(lat)) / cos(toRadians(φ0))
    linearTransformationHoboDyer(x, y, width, height)
  }
  def transformToXYBalthasart(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Balthasart: https://map-projections.net/license/balthasart:mapimg-ssw
    val (λ0, φ0) = (0, 50)
    val x = toRadians(lon - λ0) * cos(toRadians(φ0))
    val y = sin(toRadians(lat)) / cos(toRadians(φ0))
    linearTransformationBalthasart(x, y, width, height)
  }
  def transformToXYSinusoidal(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Sinusoidal: https://en.wikipedia.org/wiki/Sinusoidal_projection
    val (λ0, φ0) = (0, 0)
    val x = toRadians(lon - λ0) * cos(toRadians(lat))
    val y = toRadians(lat)
    linearTransformationSinusoidal(x, y, width, height)
  }
  def transformToXYMollweide(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Mollweide Bromley : https://en.wikipedia.org/wiki/Mollweide_projection
    //https://github.com/d3/d3-geo-projection/blob/bea1297b49a7c9f7009e827ca23062bfb9e6ca18/src/mollweide.js
    val (cx, cy, cp)=(2*sqrt(2) / Pi, sqrt(2), Pi)
    var (lambda, phi) = (toRadians(lon), toRadians(lat))
    val cpsinPhi=cp*sin(phi)
    var delta = phi * phi
    var (flag,i) = (true,60)
    while (i>0 && flag) {
      delta=(phi+sin(phi)-cpsinPhi)/(1+cos(phi))
      phi-=delta
      if (abs(delta) < 1e-7) {
        flag = false
      }
      i-=1
    }
    phi/=2
    val x = cx*lambda*cos(phi)
    val y = cy*sin(phi)
    linearTransformationMollweide(x, y, width, height)
  }
  def transformToXYGoode(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Lambert Cylindrical: https://en.wikipedia.org/wiki/Lambert_cylindrical_equal-area_projection
    val (λ0, φ0) = (0, 0)
    val x = toRadians(lon - λ0) * cos(toRadians(φ0))
    val y = sin(toRadians(lat)) / cos(toRadians(φ0))
    linearTransformationGoode(x, y, width, height)
  }
  def transformToXYToblersWIS(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Toblers Worls in a square: https://map-projections.net/single-view/toblers-world-in-a-square
    // end of the lamebert type: https://beta.observablehq.com/@mbostock/cylindrical-equal-area-projections
    val (λ0, φ0) = (0, 55.654)
    val x = toRadians(lon - λ0) * cos(toRadians(φ0))
    val y = sin(toRadians(lat)) / cos(toRadians(φ0))
    linearTransformationToblersWIS(x, y, width, height)
  }

  //-------------------------------------------End of Transformations-------------------------------------------------//

  //--------------------------------------Start of Linear Transformations---------------------------------------------//

  def linearTransformationLambertCylindric(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 325
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationBehrmann(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 374
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationEckertI(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 170
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationEckertII(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 352
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationEckertIII(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 186
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationEckertIV(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 385
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationEckertV(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 177
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationEckertVI(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 369
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationGallPeters(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 459
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }
  def linearTransformationHoboDyer(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 409
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }
  def linearTransformationBalthasart(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 247
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }
  def linearTransformationMollweide(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling =362
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }
  def linearTransformationSinusoidal(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 326
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }
  def linearTransformationGoode(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 459
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }
  def linearTransformationToblersWIS(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 139
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

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
