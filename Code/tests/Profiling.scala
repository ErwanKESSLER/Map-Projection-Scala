
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

object profiling extends App {

  def loadAirport():ArrayBuffer[(Int, String, String, String, Double, Double)]={
    val bufferedSource = Source.fromFile(getClass.getResource("/airports.dat").getPath)
    val result:ArrayBuffer[(Int, String, String, String, Double, Double)]=new ArrayBuffer()
    for (el<-bufferedSource.getLines){
      val c=el.split(",(?=[0-9\"-\\\\])")
      result.append((c(0).trim.toInt,c(1),c(2),c(3),c(6).trim.toDouble,c(7).trim.toDouble))
    }
    bufferedSource.close()
    result

  }

  def loadAirport2():ArrayBuffer[(Int, String, String, String, Double, Double)]={
    val bufferedSource = Source.fromFile(getClass.getResource("/airports.dat").getPath)
    var content = bufferedSource.getLines.toArray
    val result:ArrayBuffer[(Int, String, String, String, Double, Double)]=new ArrayBuffer()
    content.foreach(el=>{
      val c=el.split(",(?=[0-9\"-\\\\])")
      result.append((c(0).trim.toInt,c(1),c(2),c(3),c(6).trim.toDouble,c(7).trim.toDouble))
    })
    bufferedSource.close()
    result

  }

  def loadAirport3():Array[(Int, String, String, String, Double, Double)] = {
    val bufferedSource = Source.fromFile(getClass.getResource("/airports.dat").getPath)
    var content = bufferedSource.getLines.toArray
    val result:Array[(Int, String, String, String, Double, Double)]=new Array(content.length)
    for ((el,i) <- content.zipWithIndex){
      var c=el.split(",(?=[0-9\"-\\\\])")
      result(i)=(c(0).trim.toInt,c(1),c(2),c(3),c(6).trim.toDouble,c(7).trim.toDouble)
    }
    bufferedSource.close()
    result
  }
  def loadAirport4():Array[(Int, String, String, String, Double, Double)] = {
    val bufferedSource = Source.fromFile(getClass.getResource("/airports.dat").getPath)
    var content = bufferedSource.getLines.toArray
    val result:Array[(Int, String, String, String, Double, Double)]=new Array(content.length)
    for (i<-content.indices){
      var c=content(i).split(",(?=[0-9\"-\\\\])")
      result(i)=(c(0).trim.toInt,c(1),c(2),c(3),c(6).trim.toDouble,c(7).trim.toDouble)
    }
    bufferedSource.close()
    result
  }

  def time[R](block: => R,n:Int): R = {
    var t=0.0
    var result=block
    for (i<- 0 to n) {
      val t0 = System.nanoTime()
      var result = block // call-by-name
      val t1 = System.nanoTime()
      t=t+(t1-t0)
      result
    }
    println("Elapsed time :" + (t/n/1000000).toString +" ms")
    result
  }
  def test(): Unit ={
    println("Using ArrayBuffer to append elements as read then iterated took: ")
    var airports1 = time ({loadAirport()},2000)
    println("Using an ArrayBuffer as return type but using an Array as a temporary buffer")
    var airports2=time({loadAirport2()},2000)
    println("Transforming the buffer to an iterable then to an array and then copying element to the resulting array took :")
    var airports3=time({loadAirport3()},2000)
    println("Using an array as temporary buffer and copying to an array without the zipindex ")
    var airports4=time({loadAirport4()},2000)

  }
  test()
  /*Results:
    loadAirport : 14.84ms
    loadAirport2 : 14.73ms
    loadAirportTemp: 15.52ms
    loadAirportTemp2: 15.07ms
   */
}
