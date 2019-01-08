package com.top12.func

import java.awt.image.BufferedImage
import scala.math._

class equirectangularProjection {

  def toRadians(x: Double): Double = {
    Pi * x / 180
  }

  def linearTransformation(x: Double, y: Double, width: Int, height: Int): (Int, Int) = {
    val (xScaling, yScaling) = (width.toFloat / 2, height.toFloat / 2)
    val (xShift, yshift) = (xScaling, yScaling)
    (round(x * xScaling * 0.997 + xShift).toInt, round(-y * yScaling * 0.997 + yshift).toInt)
  }

  def transformToXY(lat: Double, lon: Double, width: Int, height: Int): (Int, Int) = {
    // Marinus de Tyr Projection or equirectangular projection
    //parameter for this map
    val (φ0, λ0) = (-1, 0)
    val (x, y) = (cos(toRadians(φ0)) * (lon - λ0) / 180, (lat - φ0) / 90)
    linearTransformation(x, y, width, height)
  }

  def modifyImage(filename: String, source: Array[(Int, String, String, String, Double, Double)]): Unit = {
    val util =new com.top12.utils.utils
    var img = util.readImage(filename)
    val (width, height): (Int, Int) = (img.getWidth, img.getHeight)
    source.foreach(el => {
      val (x, y): (Int, Int) = transformToXY(el._5, el._6, width, height)
      img = addCircle(img, x, y, 9, 0xFFFF0000)
    })
    util.writeImage(img, filename)
  }

  def showTrace(filename: String): Unit = {
    val util =new com.top12.utils.utils
    var img = util.readImage(filename)
    val (width, height): (Int, Int) = (img.getWidth, img.getHeight)
    for (i <- -180 to 180) {
      for (j <- -90 to 90) {
        val (x, y): (Int, Int) = transformToXY(j, i, width, height)
        img = addCircle(img, x, y, 9, 0xFFFF0000)
      }
    }
    util.writeImage(img, filename)
  }

  def addPoint(filename: String, lat: Double, lon: Double): Unit = {
    val util =new com.top12.utils.utils
    var img = util.readImage(filename)
    val (width, height): (Int, Int) = (img.getWidth, img.getHeight)
    val (x, y): (Int, Int) = transformToXY(lat, lon, width, height)
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
