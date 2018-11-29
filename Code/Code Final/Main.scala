package runtime
import func.loadAirports
import utils.utils
object Main extends App{
  def Main()={
      val loadAirport=new func.loadAirports()
      val util = new utils.utils
      //Exemple of use of step 1 code
      val airports=loadAirport.loadAirport(filename = "airports.dat")
      util.printAllElement2D(airports)

  }
  Main()
}