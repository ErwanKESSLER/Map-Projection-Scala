package utils

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source

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

  def showAllCountries(source: Array[(Int, String, String, String, Double, Double)]): Array[String] = {
    var countries: Set[String] = Set()
    source.foreach(el => {
      countries += el._4
    }
    )
    countries.toArray
  }

  def countriesCodeTable(): (HashMap[String, String], HashMap[String, String], HashMap[String, String]) = {
    //this function return two tables, one from official names to alpha-3 and the other one reversed
    //there is way more efficient than that but for simplicity sake, we will go with that.
    //also we return alpha 2 to alpha3
    var (h1, h2, h3): (HashMap[String, String], HashMap[String, String], HashMap[String, String]) = (HashMap.empty[String, String], HashMap.empty[String, String], HashMap.empty[String, String])
    val bufferedSource = Source.fromFile(getClass.getResource("/data/countriesCode.csv").getPath)
    var iterator = bufferedSource.getLines
    var headers = iterator.next()
    var content = iterator.toArray
    for (i <- content.indices) {
      var c = content(i).split(", (?=\")")
      //country name, alpha3, alpha2
      val (u, v, w): (String, String, String) = (c(0).replaceAll("\"", ""), c(2).replaceAll("\"", ""), c(1).replaceAll("\"", ""))
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

  def notOfficialNametoAlpha2(): HashMap[String, String] = {
    var h1: HashMap[String, String] = HashMap.empty[String, String]
    val bufferedSource = Source.fromFile(getClass.getResource("/data/countries.dat").getPath)
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
    val h: HashMap[String, String] = HashMap("South Sudan" -> "SSD","East Timor"->"TLS","West Bank"->"PSE",
      "Virgin Islands"->"VIR","Myanmar"->"MMR","Cote d'Ivoire"->"CIV","Palestine"->"PSE","Sao Tome and Principe"->"STP",
      "Johnston Atoll"->"JTN","Wake Island"->"WAK")
    h(name)
  }

  def notOfficialNametoAlpha3(source: Array[(Int, String, String, String, Double, Double)]):  HashMap[String, String] = {
    val countries = showAllCountries(source)
    val h0 = notOfficialNametoAlpha2()
    val (h1, h2, h3) = countriesCodeTable()
    var h4: HashMap[String, String] = HashMap.empty[String, String]
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
  def readImage(filename:String):BufferedImage={
   ImageIO.read(new File(getClass.getResource("/data/"+filename).getPath))
  }
  def writeImage(out:BufferedImage,filename:String):Unit={
    ImageIO.write(out, "png", new File(getClass.getResource("/data/").getPath+filename))
  }
}
