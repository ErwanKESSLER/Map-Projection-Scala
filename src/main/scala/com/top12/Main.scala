package com.top12

object Main {
  def main(args: Array[String]) = {

    val loadAirport = new func.loadAirports
    val distance = new func.distanceAirports
    val stats = new func.statsAirports
    val util = new com.top12.utils.utils
    val restriction = new func.restrictArea
    val density = new func.densityAirports
    val equidistant = new func.equirectangularProjection
    val conformal = new func.conformalProjections
    val equalArea = new func.equalAreaProjections
    //Example of use of step 1 code
    val airports = loadAirport.loadAirport(filename = "airports.dat")
    /*density.loadCSV("populations.csv")
    println(util.showAllCountries(airports).mkString("\n"))
    println(util.countriesCodeTable())*/
    conformal.whichProjection("all", "guyou.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    val gui = new func.gui
    gui.top.visible=true
    /*

        conformal.whichProjection("all", "mercator.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "lambertConic.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "mercatorTransverse.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "stereographic.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "peirceQuincuncial.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "guyou.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "adamshemisphere1.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "adamshemisphere2.jpg", "dot", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "adamsWIS1.jpg", "dot", util.RGBtoHexa(255, 0, 0), Left(airports))
        conformal.whichProjection("all", "adamsWIS2.jpg", "dot", util.RGBtoHexa(255, 0, 0), Left(airports))

    *//*
    equalArea.whichProjection("all", "lambertCylindric.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "behrmann.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "eckert1.jpg", "dot", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "eckert2.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "eckert3.jpg", "dot", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "eckert4.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "eckert5.jpg", "dot", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "eckert6.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "gallPeters.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "mollweide.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "sinusoidal.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "balthasart.jpg", "dot", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "hoboDyer.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
    equalArea.whichProjection("all", "toblersWIS.jpg", "dot", util.RGBtoHexa(255, 0, 0), Left(airports))
    *//*
        equalArea.whichProjection("all", "equalEarth.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))
        equalArea.whichProjection("all", "goodeHomolosine.jpg", "circle", util.RGBtoHexa(255, 0, 0), Left(airports))*/
    /* (48.9666,2.333) //PARIS
     (16.55,18.505) //KABOUL
     (40.71,-74.00) //WISCONSIN
     (37.37,-84.32) //NEW YORK*/
    //util.problematicCountries(airports)
    //util.notOfficialNametoAlpha3(airports)
    //println(density.Densite(airports, "populations.csv"))
    /*
     //util.printAllElement2DTuple(airports)
    //Example of distance between 2 airports
    val (lat1,lat2,lon1,lon2)=(-6.081689834590001,-5.20707988739,145.391998291,145.789001465)
    println(distance.distanceHaversine(lat1,lat2,lon1,lon2).toString+" km")
    println("country")
    val countries=restriction.byCountry(airports,Set("France","Spain"))
    println(countries)
    util.printAllElement2DTuple(countries)
    println("area")
    val area=restriction.byArea(airports,(20,20),(-20,-20))
    util.printAllElement2DTuple(area)
    println("radius")
    val radius=restriction.byRadius(airports,(0,0),1000)
    util.printAllElement2DTuple(radius)
    //Example of creating the distance array
    val distances=distance.distancesArray(airports)
    //util.printAllElement2DDouble(distances)
    //Example Stats
   println(stats.distanceMoyenne(distances))
    println(stats.distanceMax(distances))
    println(stats.distanceMin(distances))
    println(stats.ecartType(distances))
    //println(stats.distanceMediane1(distances))
    println(stats.distanceMediane2(distances))
    */
  }
}