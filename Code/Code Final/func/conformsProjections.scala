package func

import java.awt.image.BufferedImage

import scala.math._

class conformsProjections {
  //---------------------------------------------Core Functions-------------------------------------------------------//

  def whichProjection(typeOfFunction: String = "all", filename: String, typeOfShape: String, color: Int,
                      source: Either[Array[(Int, String, String, String, Double, Double)], (Double, Double)]): Unit = {
    filename match {
      case "mercator.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYMerc = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYMercator(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYMerc, source, color, conditions, typeOfShape)
      case "lambertConic.jpg" =>
        val conditions = (-30, 90, -180, 180)
        val tXYLamCon = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYLambertConic(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYLamCon, source, color, conditions, typeOfShape)
      case "mercatorTransverse.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYMercTran = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYMercatorTransverse(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYMercTran, source, color, conditions, typeOfShape)
      case "stereographic.jpg" =>
        val conditions = (-30, 90, -180, 180)
        val tXYStereo = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYStereographic(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYStereo, source, color, conditions, typeOfShape)
      case "peirceQuincuncial.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYPeiQui = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYPeirceQuincuncial(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYPeiQui, source, color, conditions, typeOfShape)
      case "guyou.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYGuyou = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYGuyou(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYGuyou, source, color, conditions, typeOfShape)
      case "adamshemisphere1.jpg" =>
        val conditions = (-90, 90, -90, 90)
        val tXYAHem1 = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYAdamsHemi1(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYAHem1, source, color, conditions, typeOfShape)
      case "adamshemisphere2.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYAHem2 = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYAdamsHemi2(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYAHem2, source, color, conditions, typeOfShape)
      case "adamsWIS1.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYAWIS1 = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYAdamsWIS1(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYAWIS1, source, color, conditions, typeOfShape)
      case "adamsWIS2.jpg" =>
        val conditions = (-90, 90, -180, 180)
        val tXYAWIS2 = (lat: Double, lon: Double, width: Int, height: Int) => transformToXYAdamsWIS2(lat, lon, width, height)
        whichToCall(typeOfFunction, "Sources/" + filename, tXYAWIS2, source, color, conditions, typeOfShape)
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
  def getMachineEpsilon: Double = {
    var machineEpsilon = 0.5

    while (1 + machineEpsilon > 1) {
      machineEpsilon /= 2
    }
    machineEpsilon
  }

  val machineEp: Double = getMachineEpsilon

  def toRadians(x: Double): Double = {
    Pi * x / 180
  }

  def rotation(alpha: Double, x: Double, y: Double): (Double, Double) = {
    (x * cos(toRadians(alpha)) - y * sin(toRadians(alpha)), y * cos(toRadians(alpha)) + x * sin(toRadians(alpha)))
  }

  def ellfunc(cosphi: Double, sinphi: Double, k: Double): Double = {
    //Computes the Legendre incomplete elliptic integral of the
    //first kind when circular functions of the 'phi' argument
    //are already available.
    var (x, y, z): (Double, Double, Double) = (cosphi * cosphi, 1 - k * k * sinphi * sinphi, 1)
    // it calculate the carlson elliptic Integral RF
    // RF(x, y, z) = 1/2 * integral_0^inf dt/sqrt((t+x)*(t+y)*(t+z))
    if (x < 0.0 || y < 0.0 || z < 0.0) {
      println("Negative argument, its an error")
    }
    var (dx, dy, dz): (Double, Double, Double) = (1, 1, 1)
    var mean: Double = 1
    while (abs(dx) > 0.0025 || abs(dy) > 0.0025 || abs(dz) > 0.0025) {
      val (sx, sy, sz) = (sqrt(x), sqrt(y), sqrt(z))
      val len = sx * (sy + sz) + sy * sz
      x = 0.25 * (len + x)
      y = 0.25 * (len + y)
      z = 0.25 * (len + z)
      mean = (x + y + z) / 3
      dx = (mean - x) / mean
      dy = (mean - y) / mean
      dz = (mean - z) / mean
    }
    val e2 = dx * dy - dz * dz
    val e3 = dx * dy * dz
    (1 + (e2 / 24 - 0.1 - 3 * e3 / 44) * e2 + e3 / 14) / sqrt(mean) * sinphi
  }

  def ell_int_5(phi: Double): Double = {
    //jacobi integral of the first kind with k=1/sqrt(5)
    /* Procedure to compute elliptic integral of the first kind
  * where k^2=0.5.  Precision good to better than 1e-7
  * The approximation is performed with an even Chebyshev
  * series, thus the coefficients below are the even values
  * and where series evaluation  must be multiplied by the argument. */
    var i = 8
    var (d1, d2) = (0.0, 0.0)
    /* even coefficients */
    val coeff: Array[Double] = Array[Double](2.19174570831038, 0.0914203033408211, -0.00575574836830288,
      -0.0012804644680613, 5.30394739921063e-05, 3.12960480765314e-05, 2.02692115653689e-07, -8.58691003636495e-07)
    val y2 = 4 * 4 * phi * phi / (Pi * Pi) - 1
    while (i > 1) {
      i -= 1
      val temp = d1
      d1 = y2 * d1 - d2 + coeff(i)
      d2 = temp
    }
    phi * (y2 / 2 * d1 - d2 + 0.5 * coeff(0))
  }

  def addComplex(x1: (Double, Double), x2: (Double, Double)): (Double, Double) = {
    (x1._1 + x2._1, x1._2 + x2._2)
  }

  def mulComplex(x1: (Double, Double), x2: (Double, Double)): (Double, Double) = {
    (x1._1 * x2._1 - x1._2 * x2._2, x1._1 * x2._2 + x1._2 * x2._1)
  }

  def mulConstComplex(x: (Double, Double), k: Double): (Double, Double) = {
    (x._1 * k, x._2 * k)
  }

  def powComplex(x1: (Double, Double), n: Double): (Double, Double) = {
    if (n.isInstanceOf[Int] && n < 10) {
      var result = x1
      for (i <- Range(0, n - 1)) {
        result = mulComplex(result, x1)
      }
      result
    }
    else {
      var x2 = toPolarComplex(x1)
      toAlgebraicComplex((pow(x2._1, n), x2._2 * n))
    }
  }

  def toPolarComplex(x: (Double, Double)): (Double, Double) = {
    (sqrt(x._1 * x._1 + x._2 * x._2), atan2(x._2, x._1))
  }

  def toAlgebraicComplex(x: (Double, Double)): (Double, Double) = {
    (x._1 * cos(x._2), x._1 * sin(x._2))
  }

  def re(x: (Double, Double)): Double = {
    x._1
  }

  def im(x: (Double, Double)): Double = {
    x._2
  }

  def conjugateComplex(x: (Double, Double)): (Double, Double) = {
    (x._1, -x._2)
  }

  def applyComplex(x: (Double, Double), f: Double => Double): Double = {
    f(x._1) + f(x._2)
  }

  def absComplex(x: (Double, Double)): Double = {
    applyComplex(mulComplex(x, conjugateComplex(x)), sqrt)
  }

  //---------------------------------------------End of helpers Functions---------------------------------------------//

  //-----------------------------------------------Transformations----------------------------------------------------//


  def transformToXYLambertConic(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //lambert conic parameters : https://en.wikipedia.org/wiki/Lambert_conformal_conic_projection#Transformation
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


  def transformToXYMercator(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Mercator: https://en.wikipedia.org/wiki/Mercator_projection#Derivation_of_the_Mercator_projection
    val λ0 = 0
    val x = toRadians(lon - λ0)
    val y = log(tan(Pi / 4 + toRadians(lat) / 2))
    linearTransformationMercator(x, y, width, height)
  }


  def transformToXYMercatorTransverse(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    // Mercator Transver or Gauss–Krüger or Gauss conformal :
    // https://en.wikipedia.org/wiki/Transverse_Mercator_projection#Direct transformation formulae
    val k = 1.01
    val l = 0
    val phi = 0
    val clat = cos(toRadians(lat))
    val slon = sin(toRadians(lon - l))
    val slatder = slon * clat
    val x = k * log((1 + slatder) / (1 - slatder)) / 2
    val y = k * atan(tan(toRadians(lat)) / cos(toRadians(lon - l))) - phi
    linearTransformationMercatorTransverse(x, y, width, height, lon, lat)
  }

  def transformToXYStereographic(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //stereographic: http://mathworld.wolfram.com/StereographicProjection.html
    val λ0 = 0
    val deltaλ = toRadians(lat - λ0)
    val sinlat = sin(deltaλ)
    val sinlon = sin(toRadians(lon))
    val coslat = cos(deltaλ)
    val coslon = cos(toRadians(lon))
    val phi1 = toRadians(90)
    val k = 2 / (1 + sin(phi1) * sinlat + cos(phi1) * coslat * coslon)
    val x = k * coslat * sinlon
    val y = k * (cos(phi1) * sinlat - sin(phi1) * coslat * coslon)
    linearTransformationStereographic(x, y, width, height)
  }

  def transformToXYPeirceQuincuncial(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Peirce Quincuncial: https://github.com/cspersonal/peirce-quincuncial-projection/blob/master/peirceQuincuncialProjection.R
    val peirceQuincuncialScale = 3.7081493546027438
    val lon0 = 20 //rotating the data to fit how the projection is centered
    val alpha = 45 //rotating to fit the view of the square
    var lambda = lon + lon0 - alpha + 180
    if (lambda < 0.0 || lambda > 360.0) {
      lambda = lambda - 360 * floor(lambda / 360)
    }
    lambda = toRadians(lambda - 180)
    val phi = toRadians(lat)
    val (coslambda, sinlambda) = (cos(lambda), sin(lambda))
    val cosphisqrt2 = sqrt(2) / 2 * cos(phi)
    val (cosa, cosb) = (cosphisqrt2 * (sinlambda + coslambda), cosphisqrt2 * (sinlambda - coslambda))
    val (sina, sinb) = (sqrt(1 - cosa * cosa), sqrt(1 - cosb * cosb))
    val (cosab, sinab) = (cosa * cosb, sina * sinb)
    val (sin2m, sin2n) = (1 + cosab - sinab, 1 - cosab - sinab)
    var sinm = if (sin2m < 0) 0 else sqrt(sin2m)
    val cosm = if (sin2m > 1) 0 else sqrt(1 - sin2m)
    var sinn = if (sin2n < 0) 0 else sqrt(sin2n)
    val cosn = if (sin2n > 1) 0 else sqrt(1 - sin2n)
    sinm = if (sinlambda < 0) -sinm else sinm
    sinn = if (coslambda > 0) -sinn else sinn
    var (x, y) = (ellfunc(cosm, sinm, sqrt(2) / 2), ellfunc(cosn, sinn, sqrt(2) / 2))
    if (phi < 0) {
      if (lambda < -0.75 * Pi) {
        y = peirceQuincuncialScale - y
      }
      else if (lambda < -0.25 * Pi) {
        x = -peirceQuincuncialScale - x
      }
      else if (lambda < 0.25 * Pi) {
        y = -peirceQuincuncialScale - y
      }
      else if (lambda < 0.75 * Pi) {
        x = peirceQuincuncialScale - x
      }
      else {
        y = peirceQuincuncialScale - y
      }
    }
    //rotating the projection again
    val (x0, y0) = rotation(alpha, x, y)
    linearTransformationPeirceQuincuncial(x0, y0, width, height)
  }

  def transformToXYGuyou(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    //Guyou: http://libproj4.maptools.org/ under source/misc/guyou.c,v
    var lon0 = 90
    if (lon >= 0) {
      lon0 = -90
    }
    val (lambda, phi) = (toRadians(lon + lon0), toRadians(lat))
    if (abs(lambda) - machineEp > Pi / 2) {
      throw new IllegalStateException("out of Bounds")
    }
    if (abs(abs(phi) - Pi / 2) < machineEp) {
      linearTransformationGuyou(0, if (phi < 0) -1.891514965239923 else 1.891514965239923, width, height, lon0 < 0)
    }
    else {
      val (sinlambda, sinphi, cosphi) = (sin(lambda), sin(phi), cos(phi))
      val (a, b) = (acos((cosphi * sinlambda - sinphi) * sqrt(2) / 2), acos((cosphi * sinlambda + sinphi) * sqrt(2) / 2))
      val (sm, sn) = (if (lambda < 0) -1 else 1, if (phi < 0) -1 else 1)
      val (m, n) = (sm * asin(sqrt(abs(1 + cos(a + b)))), sn * asin(sqrt(abs(1 - cos(a - b)))))
      linearTransformationGuyou(if (m.isNaN) sm * 1.891514965239923 else ell_int_5(m), if (n.isNaN) sn * 1.891514965239923 else
        ell_int_5(n), width, height, lon0 < 0)
    }
  }

  def transformToXYAdamsHemi1(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    val (lambda, phi) = (toRadians(lon), toRadians(lat))
    if (abs(lambda) - machineEp > Pi / 2) {
      throw new IllegalStateException("out of Bounds")
    }
    val sinphi = sin(phi)
    val cpsl = cos(phi) * sin(lambda)
    val (sm, sn) = (if (sinphi + cpsl < 0) -1 else 1, if (sinphi - cpsl < 0) -1 else 1)
    val (a, b) = (acos(cpsl), Pi / 2 - phi)
    val (m, n) = (sm * asin(sqrt(abs(1 + cos(a + b)))), sn * asin(sqrt(abs(1 - cos(a - b)))))
    val (x, y) = (ell_int_5(m), ell_int_5(n))
    val (x0, y0) = rotation(45.0, x, y)
    linearTransformationAdamsHemi1(x0, y0, width, height)
  }

  def transformToXYAdamsHemi2(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    var lon0 = lon
    var boo = false
    if (-20 <= lon && lon <= 160) {
      lon0 = lon - 70
      boo = true
    }
    else {
      lon0 += 110
      if (lon0 >= 90) {
        lon0 -= 360
      }
    }
    val (lambda, phi) = (toRadians(lon0), toRadians(lat))
    if (abs(lambda) - machineEp > Pi / 2) {
      throw new IllegalStateException("out of Bounds")
    }
    val sinphi = sin(phi)
    val cpsl = cos(phi) * sin(lambda)
    val (sm, sn) = (if (sinphi + cpsl < 0) -1 else 1, if (sinphi - cpsl < 0) -1 else 1)
    val (a, b) = (acos(cpsl), Pi / 2 - phi)
    val (m, n) = (sm * asin(sqrt(abs(1 + cos(a + b)))), sn * asin(sqrt(abs(1 - cos(a - b)))))
    val (x, y) = (ell_int_5(m), ell_int_5(n))
    val (x0, y0) = rotation(45.0, x, y)
    linearTransformationAdamsHemi2(x0, y0, width, height, boo)
  }

  def transformToXYAdamsWIS1(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    val (lambda, phi) = (toRadians(lon), toRadians(lat))
    val sinphi = tan(0.5 * phi)
    val cpsl = cos(asin(sinphi)) * sin(lambda * 0.5)
    val (sm, sn) = (if (lambda < 0) -1 else 1, if (phi < 0) -1 else 1)
    val (a, b) = (acos((cpsl - sinphi) * sqrt(2) / 2), acos((cpsl + sinphi) * sqrt(2) / 2))
    val (m, n) = (sm * asin(sqrt(abs(1 + cos(a + b)))), sn * asin(sqrt(abs(1 - cos(a - b)))))
    val (x, y) = (ell_int_5(m), ell_int_5(n))
    linearTransformationAdamsWIS1(x, y, width, height)
  }

  def transformToXYAdamsWIS2(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    val (lambda, phi) = (toRadians(lon), toRadians(lat))
    val sinphi = tan(0.5 * phi)
    val cpsl = cos(asin(sinphi)) * sin(lambda * 0.5)
    val (sm, sn) = (if (sinphi + cpsl < 0) -1 else 1, if (sinphi - cpsl < 0) -1 else 1)
    val (a, b) = (acos(cpsl), acos(sinphi))
    val (m, n) = (sm * asin(sqrt(abs(1 + cos(a + b)))), sn * asin(sqrt(abs(1 - cos(a - b)))))
    val (x, y) = (ell_int_5(m), ell_int_5(n))
    val (x0, y0) = rotation(45.0, x, y)
    linearTransformationAdamsWIS2(x0, y0, width, height)
  }

  def transformToXYLee(lat: Double, lon: Double, width: Int, height: Int): (Double, Double) = {
    val (lambda, phi) = (toRadians(lon), toRadians(lat))
    var (w, k, h, z) = ((-0.5, sqrt(3) / 2), (0.0, 0.0), (0.0, 0.0), mulConstComplex((lambda, phi), sqrt(2)))
    val rot = Array(0.0, 1.0, 2.0).map(i => -1 * re(mulComplex(z, powComplex(w, i)))).sortWith(_ < _)(0)
    val n = absComplex(z)
    if (n > 0.3) {
      val y = addComplex(mulConstComplex(mulConstComplex(z, -1), rot), (1, 0))
      // if |z| > 0.5, use the approx based on y = (1-z)
      // McIlroy formula 6 p6 and table for G page 16
      // w1 = gamma(1/3) * gamma(1/2) / 3 / gamma(5/6);
      val w1 = 1.4021821053254548
      val G0 = Array(
        1.15470053837925, 0.192450089729875, 0.0481125224324687, 0.010309826235529, 3.34114739114366e-4, -1.50351632601465e-3,
        -1.23044177962310e-3, -6.75190201960282e-4, -2.84084537293856e-4, -8.21205120500051e-5, -1.59257630018706e-6,
        1.91691805888369e-5, 1.73095888028726e-5, 1.03865580818367e-5, 4.70614523937179e-6, 1.4413500104181e-6, 1.92757960170179e-8,
        -3.82869799649063e-7, -3.57526015225576e-7, -2.2175964844211e-7)
      var G = (0.0, 0.0)
      for (i <- Range(G0.length, 0, -1)) {
        G = addComplex((G0(i), 0), mulComplex(y, G))
      }
      k = mulConstComplex(mulConstComplex(addComplex((w1, 0), mulComplex((-sqrt(y._1), -sqrt(y._2)), G)), rot), rot)
    }
    if (n < 0.5) {
      // if |z| < 0.3
      // https://www.wolframalpha.com/input/?i=series+of+((1-z%5E3))+%5E+(-1%2F2)+at+z%3D0 (and ask for "more terms")
      // 1 + z^3/2 + (3 z^6)/8 + (5 z^9)/16 + (35 z^12)/128 + (63 z^15)/256 + (231 z^18)/1024 + O(z^21)
      // https://www.wolframalpha.com/input/?i=integral+of+1+%2B+z%5E3%2F2+%2B+(3+z%5E6)%2F8+%2B+(5+z%5E9)%2F16+%2B+(35+z%5E12)%2F128+%2B+(63+z%5E15)%2F256+%2B+(231+z%5E18)%2F1024
      // (231 z^19)/19456 + (63 z^16)/4096 + (35 z^13)/1664 + z^10/32 + (3 z^7)/56 + z^4/8 + z + constant
      val H0 = Array(1, 1 / 8, 3 / 56, 1 / 32, 35 / 1664, 63 / 4096, 231 / 19456)
      val z3 = powComplex(z, 3)
      for (i <- Range(H0.length, 0, -1)) {
        h = addComplex((H0(i), 0), mulComplex(h, z3))
      }
      h = mulComplex(h, z)
    }
    if (n < 0.3) return h
    if (n > 0.5) return k
    // in between 0.3 and 0.5, interpolate
    var t = (n - 0.3) / (0.5 - 0.3)
    addComplex(mulConstComplex(k, t), mulConstComplex(h, 1 - t))
  }

  //-------------------------------------------End of Transformations-------------------------------------------------//

  //--------------------------------------Start of Linear Transformations---------------------------------------------//
  def linearTransformationAdamsWIS1(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 130
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationAdamsWIS2(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 92.5
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationAdamsHemi2(x: Double, y: Double, width: Int, height: Int, b: Boolean): (Int, Int) = {
    val scaling = 92
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + ((if (b) 1 else 0) + 0.5) * xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationAdamsHemi1(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 384
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationGuyou(x: Double, y: Double, width: Int, height: Int, b: Boolean): (Int, Int) = {
    val scaling = 267
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + ((if (b) 1 else 0) + 0.5) * xShift).toInt, round(-y * scaling + yshift).toInt)

  }

  def linearTransformationPeirceQuincuncial(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 390
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)

  }

  def linearTransformationStereographic(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 295
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)

  }

  def linearTransformationLambertConic(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 353
    val (xShift, yshift) = (1030, 710)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)
  }

  def linearTransformationMercator(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val scaling = 325
    val (xShift, yshift) = (width / 2, height / 2)
    (round(x * scaling + xShift).toInt, round(-y * scaling + yshift).toInt)

  }

  def linearTransformationMercatorTransverse(x: Double, y: Double, width: Int, height: Int, lon: Double, lat: Double): (Int, Int) = {
    val scaling = 325
    val (xShift, yshift) = (width / 2, height / 2)

    if (lon <= (-91) && lon >= (-180) || lon >= 91 && lon <= 180) {
      if (lat >= 0) {
        (round(x * scaling + xShift).toInt, round(-y * scaling + 15).toInt)
      }
      else {
        (round(x * scaling + xShift).toInt, round(height - y * scaling - 15).toInt)
      }

    } else {
      if (lat >= 0) {
        (round(x * scaling + xShift).toInt, round(-y * scaling + yshift - 5).toInt)
      } else {
        (round(x * scaling + xShift).toInt, round(-y * scaling + yshift + 5).toInt)
      }
    }
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
