package func

import scala.collection.mutable.HashMap
import scala.io.Source
import scala.collection.mutable

class densityAirports {

  def loadCsv(file: String): mutable.HashMap[String, Double] = { // Récupérer les mesures avec lequelles on va faire la densité (et uniquement ce qui nous intéresse)
    // On stocke le résultat dans une hashmap pour accéder plus vite aux infos
    val h: mutable.HashMap[String, Double] = mutable.HashMap.empty[String, Double]
    // On ouvre le fichier
    val bufferedSource = Source.fromFile(getClass.getResource("/data/" + file).getPath)
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


  def Densite(file: Array[(Int, String, String, String, Double, Double)], file2: String): HashMap[String, Double] = { // file c'est airports.dat , file2 c'est superficie ou la pop
    val hashMapDensite: HashMap[String, Double] = HashMap.empty[String, Double] // HashMap pour accéder plus facilement aux éléments
  val hashMapAirports: HashMap[String, Int] = HashMap.empty[String, Int] // Idem (pour compter les aéroports
    file.foreach(el => { // boucle for sur le airports.dat
      if (hashMapAirports.contains(el._4)) { // Si l'aéroports et dans un pays qu'on a pas rentré auparavant, on l'initialise à 1 sinon on augmente de 1 le nombre d'aéroports du pays
        hashMapAirports(el._4) += 1
      }
      else {
        hashMapAirports += el._4 -> 1
      }
    })
    val h = loadCsv(file2) // On charge les données sur les aéroports qui nous intéressent seulement
    val util = new utils.utils
    val conversionPaysversAlpha3 = util.notOfficialNametoAlpha3(file) // méthode pour convertir les noms non officiels de pays en alpha3
    for (paysEtnombre <- hashMapAirports) { // On parcourt la hashmap des aéroports
      val alpha3 = conversionPaysversAlpha3(paysEtnombre._1) // On convertit le nom du pays en alpha3 pour récupérer l'info de la mesure dans la hashmap
    val mesure = h(alpha3)
      hashMapDensite += paysEtnombre._1 -> (paysEtnombre._2 / mesure) // On fait le rapport entre le nombre d'aéroport et la mesure (pop ou superficie)
    }
    hashMapDensite // On renvoie le résultat de la densité sous forme de hashmap avec les noms de pays utilisés dans aiports.dat
  }
}
