package com.top12.func

import math._
import scala.collection.mutable.ArrayBuffer

class restrictArea {

  def byCountry(source: Array[(Int, String, String, String, Double, Double)], countries: Set[String]): Array[(Int, String, String, String, Double, Double)] = {
    var result = new ArrayBuffer[(Int, String, String, String, Double, Double)]
    source.foreach(element => {
      if (countries(element._4)) { //We test if the country belong to the set of countries
        result += element
      }
    }
    )
    result.toArray
  }

  def byArea(source: Array[(Int, String, String, String, Double, Double)], firstPoint: (Double, Double), secondPoint: (Double, Double)): Array[(Int, String, String, String, Double, Double)] = {
    val latMin = min(firstPoint._1, secondPoint._1)
    val latMax = max(firstPoint._1, secondPoint._1)
    val lonMin = min(firstPoint._2, secondPoint._2)
    val lonMax = max(firstPoint._2, secondPoint._2)
    var result = new ArrayBuffer[(Int, String, String, String, Double, Double)]
    source.foreach(element =>
      if (element._5 >= latMin && element._6 >= lonMin && element._5 <= latMax && element._6 <= lonMax) { //we check if it is in the area
        result += element
      })
    result.toArray
  }

  def byRadius(source: Array[(Int, String, String, String, Double, Double)], center: (Double, Double), radius: Double): Array[(Int, String, String, String, Double, Double)] = {
    var result = new ArrayBuffer[(Int, String, String, String, Double, Double)]
    val distance = new distanceAirports()
    source.foreach(element =>
      if (distance.distanceHaversine(center._1, element._5, center._2, element._6) <= radius) { //we check if the distance between the 2 points is below or equal to the radius
        result += element
      }

    )
    result.toArray
  }
}
