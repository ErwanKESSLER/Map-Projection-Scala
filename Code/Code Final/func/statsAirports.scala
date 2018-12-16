package func
import math._
import scala.util.Random


class statsAirports {

    def quickSelect(source:Array[Double],n:Int, rand: Random = new Random): Double = {
        val pivot = rand.nextInt(source.length);
        val (left, right) = source.partition(_ < source(pivot))
        if (left.length == n) {
            source(pivot)
        } 
        else if (left.length < n) {
            quickSelect(right, n - left.length, rand)
        } 
        else {
            quickSelect(left, n, rand)
        }
    }




    def distanceMin(source:Array[Double]):Double={
        var mini:Double=source(0)
        for ( i <- source.indices ) {
            if ( mini > source(i) ) {
                mini=source(i)
            }
        }
        mini
    }

    def distanceMax(source:Array[Double]):Double={
        var maxi:Double=0
        for ( i <- source.indices ) {
            if ( maxi < source(i) ) {
                maxi=source(i)
            }
        }
        maxi
    }

    def distanceMoyenne(source:Array[Double]):Double={
        var moyenne:Double=0
        for (i <- source.indices ) {
            moyenne+=source(i)
        }
        moyenne/source.length
    }

    def ecartType(source:Array[Double]):Double={
        val moyenne:Double=distanceMoyenne(source)
        var s:Double=0
        for (i <- source.indices ) {
            s+= pow(source(i) - moyenne,2)
        }
        sqrt(s/(source.length-1))
    }

    def distanceMediane1(source:Array[Double]):Double={
        val res:Array[Double]=source.sorted
        res(res.length/2)
    }

    def distanceMediane2(source:Array[Double]):Double={ // Plus rapide que Mediane1
        quickSelect(source,source.length/2)
        }

}