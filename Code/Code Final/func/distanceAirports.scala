package func

import scala.math._

class distanceAirports {

  def toRadians(degree: Double): Double = {
    degree *Pi/180
  }

  def distanceHaversine(lat1: Double, lat2: Double, lon1: Double, lon2: Double): Double = {
    /*
          Supposedly the best approximation
          shortest distance on earth surface ignoring hills and using circles.
          (spherical models giving 0.3% errors in worst case)
          a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
          c = 2 ⋅ atan2( √a, √(1−a) )
          d = R ⋅ c
    */
    val R = 6371 //in kilometers and approximately,reminder: we are using a flat surface type sphere
    val φ1: Double = lat1.toRadians
    val φ2: Double = lat2.toRadians
    val Δφ: Double = (lat2 - lat1).toRadians
    val Δλ: Double = (lon2 - lon1).toRadians
    val a = Math.sin(Δφ / 2) * Math.sin(Δφ / 2) + Math.cos(φ1) * Math.cos(φ2) * Math.sin(Δλ / 2) * Math.sin(Δλ / 2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    R * c

  }

  def distanceSphericalLawCosines(lat1: Double, lat2: Double, lon1: Double, lon2: Double): Double = {
    /*
          d = acos( sin φ1 ⋅ sin φ2 + cos φ1 ⋅ cos φ2 ⋅ cos Δλ ) ⋅ R
     */
    val R = 6371
    val φ1 = lat1.toRadians
    val φ2 = lat2.toRadians
    val Δλ = (lon2 - lon1).toRadians
    Math.acos(Math.sin(φ1) * Math.sin(φ2) + Math.cos(φ1) * Math.cos(φ2) * Math.cos(Δλ)) * R
  }

  def distanceEquirectangularApproximation(lat1: Double, lat2: Double, lon1: Double, lon2: Double): Double = {
    /*
          x = Δλ ⋅ cos φm
          y = Δφ
          d = R ⋅ √x² + y²
     */
    val R = 6371
    val Δφ: Double = (lat2 - lat1).toRadians
    val Δφm: Double = (lat2 + lat1).toRadians
    val Δλ: Double = (lon2 - lon1).toRadians
    val x = Δλ * Math.cos(Δφm / 2)
    val y = Δφ
    Math.sqrt(x * x + y * y) * R

  }

  def distancesArray(source:Array[(Int, String, String, String, Double, Double)]):Array[Double]={
    val result:Array[Double]=new Array(source.length*(source.length-1)/2)
    var index:Int=0
    for (i <- source.indices){
      for (j <- 0 until i){ //exclusive bound, to is inclusive tho
        result(index)=distanceHaversine(source(i)._5,source(j)._5,source(i)._6,source(j)._6)
        index=index+1
      }
    }
    result
  }
}
