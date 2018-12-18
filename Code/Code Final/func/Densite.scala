package func
import scala.collection.mutable.HashMap
import scala.io.Source


class Densite {

  def loadCsv(file: String): HashMap[String, Double] = { // Récupérer les mesures avec lequelles on va faire la densité (et uniquement ce qui nous intéresse)
    val h: HashMap[String, Double] = HashMap.empty[String, Double] // On stocke le résultat dans une hashmap pour accéder plus vite aux infos
  val bufferedSource = Source.fromFile(getClass.getResource("/data/" + file).getPath) // On ouvre le fichier
  val iterator = bufferedSource.getLines // la boucle for sur les lignes
    iterator.next()
    val content = iterator.toArray // On convertit la ligne en Array pour accéder plus facilement au resultat
    content.foreach(el => {
      val ligne = el.split(",(?=\")") // On split avec ,"
      val (alpha3, nombre) = (ligne(1).replaceAll("\"", ""), ligne.last.replaceAll("[,\"]", "")) // On remplace les " par rien pour avoir le nom du pays sans guillemets
      h += alpha3 -> nombre.trim.toDouble // On doit avoir une hashmap de String et de Double donc on convertit nombre en Double
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
