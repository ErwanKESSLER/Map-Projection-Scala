package func
import scala.io.Source

class loadAirports  {
  def loadAirport(filename:String):Array[(Int, String, String, String, Double, Double)] = {
    //We are reading the content of the file to a buffer
    val bufferedSource = Source.fromFile(getClass.getResource("/"+filename).getPath)
    //We convert that buffer to an iterable then fill an Array with it
    var content = bufferedSource.getLines.toArray
    //We create an empty Array of the appropriate return type of the correct size
    val result:Array[(Int, String, String, String, Double, Double)]=new Array(content.length)
    //We iterate over all the element of our data
    for (i<-content.indices){
      //We use a splitter that will look ahead to find pattern that match beginning of correct data to split, thus
      //eliminating bad comma separation, a simple match over non ", " sequences with ,(?=[^ ]) would have been enough.
      var c=content(i).split(",(?=[0-9\"-\\\\])")
      //We fill our result Array with only the data that is interesting taking no precaution with type parsing
      result(i)=(c(0).trim.toInt,c(1),c(2),c(3),c(6).trim.toDouble,c(7).trim.toDouble)
    }
    bufferedSource.close()
    result
  }
}
