package com.top12.func

import scala.io.Source
import scala.collection.mutable

class densityAirports {
  def loadCsv(file: String): mutable.HashMap[String, Double] = { // Récupérer les mesures avec lequelles on va faire la densité (et uniquement ce qui nous intéresse)
    // On stocke le résultat dans une hashmap pour accéder plus vite aux infos
    val h: mutable.HashMap[String, Double] = mutable.HashMap.empty[String, Double]
    // On ouvre le fichier
    val bufferedSource = Source.fromResource(file)
    // On recupere un iterateur qui contient chaque ligne du buffer
    val iterator = bufferedSource.getLines
    // On elimine les headers
    iterator.next()
    // On convertit l'iterateur en Array pour accéder plus facilement au resultat
    val content = iterator.toArray
    content.foreach(el => {
      // On split sur tout les virgules qui sont suivis d'un guillemet
      val ligne = el.split(",(?=\")")
      // On selectionne le deuxieme et le dernier elements de la ligne dont on retire les " et ,
      // Afin d'avoir l'alpha3 (iso 3166) et la metrique la plus recente (pop ou surface)
      val (alpha3, nombre) = (ligne(1).replaceAll("\"", ""), ligne.last.replaceAll("[,\"]", ""))
      // On ajoute a la hashmap la relation cle/valeur en oubliant pas de convertir en un nombre la metrique
      h += alpha3 -> nombre.trim.toDouble
    })
    h
  }

  def Densite(source: Array[(Int, String, String, String, Double, Double)], filename: String): mutable.HashMap[String, Double] = {
    // Hashmap de sortie
    val hashMapDensite: mutable.HashMap[String, Double] = mutable.HashMap.empty[String, Double]
    // Hashmap de comptage des aeroports
    val hashMapAirports: mutable.HashMap[String, Int] = mutable.HashMap.empty[String, Int]
    source.foreach(el => {
      // Si le pays existe on incremente sinon on initialise un couple cle/valeur avec le pays et 1
      if (hashMapAirports.contains(el._4)) {
        hashMapAirports(el._4) += 1
      }
      else {
        hashMapAirports += el._4 -> 1
      }
    })
    // On charge les donnees correspondantes a la metrique
    val h = loadCsv(filename)
    val util =new com.top12.utils.utils
    // On appelle une méthode pour convertir les noms non officiels de pays en alpha3
    val conversionPaysversAlpha3 = util.notOfficialNametoAlpha3(source)
    for (paysEtnombre <- hashMapAirports) {
      // On convertit le nom du pays en alpha3 qui est unique a chaque "pays"
      val alpha3 = conversionPaysversAlpha3(paysEtnombre._1)
      // On recupere la metrique associee au alpha3
      val metrique = h(alpha3)
      // On fait le rapport entre le nombre d'aéroport et la metrique
      hashMapDensite += paysEtnombre._1 -> (paysEtnombre._2.toDouble / metrique)
    }
    print(hashMapDensite)
    // On renvoie le résultat de la densité sous forme de hashmap avec les noms de pays utilisés dans aiports.dat
    hashMapDensite
  }
}
