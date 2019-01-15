package com.top12.utils

import java.awt.image.{BufferedImage, DataBufferByte}
import java.io.File
import java.net.URLDecoder

import javax.imageio.ImageIO

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.{AnsiColor, Source}

class utils {
  def printAllElement2DTuple(source: Array[(Int, String, String, String, Double, Double)]): Unit = {
    for (i <- source.indices) {
      println(source(i).productIterator.mkString("; "))
    }
  }

  def printAllElement2DDouble(source: Array[Double]): Unit = {
    for (i <- source.indices) {
      println(source(i))
    }
  }

  def printAllElementBuffer(source: ArrayBuffer[(Int, String, String, String, Double, Double)]): Unit = {
    for (i <- source.indices) {
      println(source(i).productIterator.mkString("; "))
    }
  }

  def airportsNameToNumbers(source: Array[(Int, String, String, String, Double, Double)]): mutable.HashMap[String, Int] = {
    var h: mutable.HashMap[String, Int] = mutable.HashMap.empty[String, Int]
    for (i <- source.indices) {
      h += source(i)._2 -> i
    }
    h
  }

  def airportsIdToNumbers(source: Array[(Int, String, String, String, Double, Double)]): mutable.HashMap[Int, Int] = {
    var h: mutable.HashMap[Int, Int] = mutable.HashMap.empty[Int, Int]
    for (i <- source.indices) {
      h += source(i)._1 -> i
    }
    h
  }

  def showAllCountries(source: Array[(Int, String, String, String, Double, Double)]): Array[String] = {
    var countries: Set[String] = Set()
    source.foreach(el => {
      countries += el._4
    })
    countries.toArray
  }

  def countriesCodeTable(): (mutable.HashMap[String, String], mutable.HashMap[String, String], mutable.HashMap[String, String]) = {
    //this function return three tables, one from official names to alpha-3 and the other one reversed
    //there is way more efficient than that but for simplicity sake, we will go with that.
    //also we return alpha 2 to alpha3
    var (h1, h2, h3): (mutable.HashMap[String, String], mutable.HashMap[String, String],
      mutable.HashMap[String, String]) = (mutable.HashMap.empty[String, String], mutable.HashMap.empty[String, String],
      mutable.HashMap.empty[String, String])
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream("/countriesCode.csv"))
    var iterator = bufferedSource.getLines
    var headers = iterator.next()
    var content = iterator.toArray
    for (i <- content.indices) {
      var c = content(i).split(", (?=\")")
      //country name, alpha3, alpha2
      val (u, v, w): (String, String, String) = (c(0).replaceAll("\"", ""), c(2).replaceAll("\"", ""),
        c(1).replaceAll("\"", ""))
      h1 += (u -> v)
      h2 += (v -> u)
      //alpha2 vers alpha3
      h3 += (w -> v)
    }
    bufferedSource.close()
    (h1, h2, h3)
  }

  def problematicCountries(source: Array[(Int, String, String, String, Double, Double)]): Unit = {
    val countries = showAllCountries(source)
    val (h1, h2, h3) = countriesCodeTable()
    countries.foreach(el => {
      if (!h1.contains(el)) {
        println(el)
      }
    })
  }

  def notOfficialNametoAlpha2(): mutable.HashMap[String, String] = {
    var h1: mutable.HashMap[String, String] = mutable.HashMap.empty[String, String]
    val bufferedSource = Source.fromInputStream(getClass.getResourceAsStream("/countries.dat"))
    var content = bufferedSource.getLines.toArray
    for (i <- content.indices) {
      var c = content(i).split(",(?=[0-9\"-\\\\])")
      val x = c(2).replaceAll("\"", "")
      if (x != "\\N") {
        h1 += (c(0).replaceAll("\"", "") -> x)
      }
      else {
        h1 += (c(0).replaceAll("\"", "") -> c(1).replaceAll("\"", ""))
      }

    }
    bufferedSource.close()
    h1
  }

  def convertName(name: String): String = {
    val h: mutable.HashMap[String, String] = mutable.HashMap("South Sudan" -> "SSD", "East Timor" -> "TLS", "West Bank" -> "PSE",
      "Virgin Islands" -> "VIR", "Myanmar" -> "MMR", "Cote d'Ivoire" -> "CIV", "Palestine" -> "PSE", "Sao Tome and Principe" -> "STP",
      "Johnston Atoll" -> "JTN", "Wake Island" -> "WAK")
    h(name)
  }

  def notOfficialNametoAlpha3(source: Array[(Int, String, String, String, Double, Double)]): mutable.HashMap[String, String] = {
    val countries = showAllCountries(source)
    val h0 = notOfficialNametoAlpha2()
    val (h1, h2, h3) = countriesCodeTable()
    var h4: mutable.HashMap[String, String] = mutable.HashMap.empty[String, String]
    countries.foreach(el => {
      if (h0.contains(el) && h3.contains(h0(el))) {
        val alpha2 = h0(el)
        h4 += (el -> h3(alpha2))
      }
      else {
        h4 += (el -> convertName(el))
      }
    })
    h4
  }

  def readImage(filename: String): BufferedImage = {
    println("Reading " + filename + " from " + getClass.getResource("/" + filename))
    ImageIO.read(getClass.getResource("/" + filename))
  }

  def writeImage(out: BufferedImage, filename: String): Unit = {
    val extension = filename.split("\\.")(1)
    val path = "/Results/" + filename.split("\\.")(0).split("/").dropRight(1).drop(1).mkString("/") + "/"
    val name = filename.split("\\.")(0).split("/").last
    val jarPath = URLDecoder.decode(getClass.getProtectionDomain.getCodeSource.getLocation.getPath, "UTF-8")
    val parent = new File(jarPath.substring(0, jarPath.lastIndexOf("/")) + path)
    parent.mkdirs()
    val file = new File(parent, name + "_result." + extension)
    file.createNewFile()
    ImageIO.write(out, extension, file)
    println(AnsiColor.GREEN + AnsiColor.BLINK + "file saved as " + file + AnsiColor.RESET)
  }

  def returnTRGB(color: Int): (Int, Int, Int, Int) = {
    //transparency then red, green and blue as 2 hexadecimals each
    (color & 0xff000000 / 16777216, (color & 0xff0000) / 65536, (color & 0xff00) / 256, color & 0xff)
  }

  def RGBtoHexa(rgb: (Int, Int, Int)): Int = {
    Integer.parseInt("00" + rgb._1.toHexString.reverse.padTo(2, '0').reverse + rgb._2.toHexString.reverse.padTo(2, '0').reverse +
      rgb._3.toHexString.reverse.padTo(2, '0').reverse, 16)
  }

  //shamefully stolen from https://github.com/tncytop/top-roaddetection/blob/master/src/main/scala/com/tncy/top/image/ImageWrapper.scala
  // @author Motasim
  // @author Francesco Giovannini
  def convertTo2DWithoutUsingGetRGB(image: BufferedImage): Array[Array[Int]] = {
    val pixels = image.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData(): Array[Byte]
    val width = image.getWidth(): Int
    val height = image.getHeight(): Int
    val hasAlphaChannel = image.getAlphaRaster != null: Boolean
    val result = Array.ofDim[Int](height, width): Array[Array[Int]]
    if (hasAlphaChannel) { // The RGB values contain a transparency representation
      val pixelLength = 4: Int
      var col = 0: Int
      var row = 0: Int
      for (pixel: Int <- pixels.indices by pixelLength) {
        var argb = 0: Int
        argb += ((pixels(pixel).asInstanceOf[Int] & 0xff) << 24) // alpha
        argb += (pixels(pixel + 1).asInstanceOf[Int] & 0xff) // blue
        argb += ((pixels(pixel + 2).asInstanceOf[Int] & 0xff) << 8) // green
        argb += ((pixels(pixel + 3).asInstanceOf[Int] & 0xff) << 16) // red
        result(row)(col) = argb
        col += 1
        if (col == width) {
          col = 0
          row += 1
        }
      }
    } else { // The RGB values don't contain a transparency representation
      val pixelLength = 3: Int
      var col = 0: Int
      var row = 0: Int
      for (pixel <- pixels.indices by pixelLength) {
        var argb = 0: Int
        argb += -16777216 // 255 alpha
        argb += (pixels(pixel).asInstanceOf[Int] & 0xff) // blue
        argb += ((pixels(pixel + 1).asInstanceOf[Int] & 0xff) << 8) // green
        argb += ((pixels(pixel + 2).asInstanceOf[Int] & 0xff) << 16) // red
        result(row)(col) = argb
        col += 1
        if (col == width) {
          col = 0
          row += 1
        }
      }
    }
    result
  }


}
