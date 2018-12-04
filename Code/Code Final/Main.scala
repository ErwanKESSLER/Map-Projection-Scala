package runtime
import func.loadAirports
import utils.utils
import func.distanceAirports
object Main extends App{
  def Main()={
      val loadAirport=new func.loadAirports()
      val distance=new func.distanceAirports()
      val stats=new func.Stats()
      val util = new utils.utils
      //Example of use of step 1 code
      val airports=loadAirport.loadAirport(filename = "airports.dat")
      //util.printAllElement2DTuple(airports)

      println

      //Example of distance between 2 airports
      val (lat1,lat2,lon1,lon2)=(-6.081689834590001,-5.20707988739,145.391998291,145.789001465)
      println(distance.distanceHaversine(lat1,lat2,lon1,lon2).toString+" km")

      println

      //Example of creating the distance array
      val distances=distance.distancesArray(airports)
      //util.printAllElement2DDouble(distances)

      //Example Stats
      println(stats.distanceMoyenne(distances))
      println(stats.distanceMax(distances))
      println(stats.distanceMin(distances))
      println(stats.ecartType(distances))
      println(stats.distanceMediane1(distances))
      println(stats.distanceMediane2(distances))

  }
  Main()
}