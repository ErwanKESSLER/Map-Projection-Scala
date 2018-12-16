package utils

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Set
import scala.collection.mutable.HashMap
import scala.io.Source

class utils {
  def printAllElement2DTuple(source: Array[(Int, String, String, String, Double, Double)]):Unit = {
    for (i <- source.indices) {
      println(source(i).productIterator.mkString("; "))
    }
  }
  def printAllElement2DDouble(source:Array[Double]):Unit={
    for (i<-source.indices){
      println(source(i))
    }
  }
  
  def printAllElementBuffer(source:ArrayBuffer[(Int, String, String, String, Double, Double)]):Unit={
    for (i<-source.indices){
      println(source(i).productIterator.mkString("; "))
    }
  }

  def showAllCountries(source: Array[(Int, String, String, String, Double, Double)]):Array[String]={
    var countries:Set[String]=Set()
    source.foreach(el=>{
      countries+= el._4
    }
    )
    countries.toArray
  }
  def countriesCodeTable():(HashMap[String,String],HashMap[String,String])= {
    //this function return two tables, one from official names to alpha-3 and the other one reversed
    //there is way more efficient than that but for simplicity sake, we will go with that.
    var (h1,h2):(HashMap[String,String],HashMap[String,String])=(HashMap.empty[String,String],HashMap.empty[String,String])
    val bufferedSource = Source.fromFile(getClass.getResource("/data/countriesCode").getPath)
    var iterator=bufferedSource.getLines
    var headers=iterator.next()
    var content = iterator.toArray
    for (i<-content.indices){
      var c=content(i).split(", (?=\")")
      val (u,v):(String,String)=(c(0).replaceAll("\"", ""),c(2).replaceAll("\"", ""))
      h1+=(u->v)
      h2+=(v->u)
    }
    bufferedSource.close()
    (h1,h2)
  }

}
