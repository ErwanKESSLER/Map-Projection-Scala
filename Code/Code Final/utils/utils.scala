package utils


class utils {
  def printAllElement2DTuple(source: Array[(Int, String, String, String, Double, Double)]) = {
    for (i <- source.indices) {
      println(source(i).productIterator.mkString("; "))
    }
  }
  def printAllElement2DDouble(source:Array[Double])={
    for (i<-source.indices){
      println(source(i))
    }
  }
  

}
