

import scala.io.Source
import scala.util.{Failure, Success, Try}

object EtapeOne extends App {

  //METHODS

  def read(): (Array[String], Int, Source) = {
    //Reading the file, here we are loading it into a buffer then we convert it as an iterable via getLines and directly as
    // an Array as we will manipulate it as it. We will need to close at the end the file so we pass the reference along,
    // its possible to implement an interface for that but we are lazy

    val bufferedSource = Source.fromFile(getClass.getResource("/airports.dat").getPath)
    var content = bufferedSource.getLines.toArray
    (content, content.length, bufferedSource) //content.length===content.size btw, history...
  }

  def splitSimple(source: Array[String]): Array[Array[String]] = {
    //here we use the look ahead operator to find specific part we know start a sequence of valid data, we
    //could also only take what isnt a valid start but could be tedious
    val result = Array.ofDim[String](source.length, source(0).split(",(?=[0-9\"-\\\\])").length)
    for (i <- source.indices) {
      result(i) = source(i).split(",(?=[0-9\"-\\\\])")
    }
    result
  }

  def loadAirport(source: Array[String], correctHeaders: Array[Int]):Array[(Int, String, String, String, Double, Double)] = {
    val airports = splitSimple(source)
    val result:Array[(Int, String, String, String, Double, Double)]=new Array(airports.length)
    for (i <- airports.indices) {
      var current = airports(i)
      result(i) = (toInt(current(correctHeaders(0))), current(correctHeaders(1)), current(correctHeaders(2)),
        current(correctHeaders(3)), toDouble(current(correctHeaders(4))), toDouble(current(correctHeaders(4))))
    }
    result
  }

  def toInt(s: String):Int = Try(s.trim.toInt) match {
    case Success(i) => i
    case Failure(t) => 0
  }
  def toDouble(s: String): Double = Try(s.trim.toDouble) match {
    case Success(i) => i
    case Failure(t) => 0.0
  }
  def extractColumnArray(source: Array[Array[String]], column: Int) = {
    val t: Array[Array[String]] = source.transpose
    t(column)
  }

  def extractColumnArrayAlternate(source: Array[Array[String]], column: Int) = {
    source map (_ (column))
  }

  def close(file: Source): Unit = {
    file.close()
  }

  //DEBUGGING

  def printAllElement2D(source: Array[Array[String]]) = {
    for (i <- source.indices) {
      println(source(i).mkString("; "))
    }
  }

  def printAllElement(array: Array[String]) = {
    println(array.mkString("; "))
  }

  def printElement2DTuple(source: Array[(Int, String, String, String, Double, Double)])={
    for (i <- source.indices) {
      println(source(i).productIterator.mkString("; "))
    }
  }
  //TESTS

  def testSplitting(source: Array[String], i: Int): Unit = {
    println(source(i).split(",(?=[0-9\"-\\\\])").mkString(" "))
  }

  def checkElements(source: Array[Array[String]]): Unit = {
    source.foreach(el =>
      if (el.length != 14) {
        printAllElement(el)
      })
  }

  def test(source: Array[String]) {
    print("hey ,\\N".split(""",\\""").mkString("    "))
    print(" is \"hey ,\\N\" splitted on the ,\\ ")
    println
    print("hey ,\\N".split(",\\\\").mkString("    "))
    println(" is also\"hey ,\\N\" splitted on the ,\\")
    println("Below is the first line of the data splitted")
    testSplitting(source, 0)
    val splittedData: Array[Array[String]] = splitSimple(source)
    println("Below are all the elements not matched, pls extend regex")
    checkElements(splittedData)
    println("No more elements")
  }

  def Main(): Unit = {

    val (content, size, file) = read()
    val headersAirports = Array("ID", "Name", "City", "Country", "IATA", "ICAO", "Latitude", "Longitude", "Altitude", "Timezone",
      "DaylightSavingTime", "TimeZone", "Type", "Source")
    val correctHeaders = Array("ID", "Name", "City", "Country", "Latitude", "Longitude")
    val correspondingNumbers = Array(0, 1, 2, 3, 6, 7)

    var airports = loadAirport(content, correspondingNumbers)
    printElement2DTuple(airports)

    close(file)
  }

  Main()
}
