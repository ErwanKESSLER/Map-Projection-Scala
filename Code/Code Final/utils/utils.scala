package utils
class utils {
  def printAllElement2D(source: Array[(Int, String, String, String, Double, Double)]) = {
    for (i <- source.indices) {
      println(source(i).productIterator.mkString("; "))
    }
  }
}
