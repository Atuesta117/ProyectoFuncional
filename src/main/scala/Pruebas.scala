import Comete._
import Opinion._
import common._

// Objeto principal de pruebas del proyecto.
// Se movió a src/main/scala para poder ejecutarlo con: sbt "runMain Pruebas"
object Pruebas extends App {

//-------------------Pruebas rho ------------------------
  //Funciones de creencias genericas de prueba
  // Build uniform belief state.
  def uniformBelief(nags: Int): SpecificBelief = {
    Vector.tabulate(nags)((i: Int) =>
      (i + 1).toDouble / nags.toDouble
    )
  }

  // Builds mildly polarized belief state, in which
  // half of agents has belief decreasing from 0.25, and
  // half has belief increasing from 0.75, all by the given step.
  def midlyBelief(nags: Int): SpecificBelief = {
    val middle = nags / 2

    Vector.tabulate(nags)((i: Int) =>
      if (i < middle)
        math.max(0.25 - 0.01 * (middle - i - 1), 0)
      else
        math.min(0.75 - 0.01 * (middle - i), 1)
    )
  }

  // Builds extreme polarized belief state, in which half
  // of the agents has belief 0, and half has belief 1.
  def allExtremeBelief(nags: Int): SpecificBelief = {
    val middle = nags / 2

    Vector.tabulate(nags)((i: Int) =>
      if (i < middle) 0.0 else 1.0
    )
  }

  // Builds three-pole belief state, in which each
  // one third of the agents has belief 0, one third has belief 0.5,
  // and one third has belief 1.
  def allTripleBelief(nags: Int): SpecificBelief = {
    val oneThird = nags / 3
    val twoThird = (nags / 3) * 2

    Vector.tabulate(nags)((i: Int) =>
      if (i < oneThird) 0.0
      else if (i >= twoThird) 1.0
      else 0.5
    )
  }

  // Builds consensus belief state, in which all
  // agents have same belief.
  def consensusBelief(b: Double)(nags: Int): SpecificBelief = {
    Vector.tabulate(nags)((i: Int) => b)
  }
  val sbext = allExtremeBelief(100)

  val sbcons = consensusBelief(0.2)(100)

  val sbunif = uniformBelief(100)

  val sbtriple = allTripleBelief(100)

  val sbmidly = midlyBelief(100)
  val rho1 = rho(1.2, 1.2)
  val rho2 = rho(2.0, 1.0)

  val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
  val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

  rho1(sbext, dist1)
  rho2(sbext, dist1)
  rho1(sbext, dist2)
  rho2(sbext, dist2)

  rho1(sbcons, dist1)
  rho2(sbcons, dist1)
  rho1(sbcons, dist2)
  rho2(sbcons, dist2)

  rho1(sbunif, dist1)
  rho2(sbunif, dist1)
  rho1(sbunif, dist2)
  rho2(sbunif, dist2)

  rho1(sbtriple, dist1)
  rho2(sbtriple, dist1)
  rho1(sbtriple, dist2)
  rho2(sbtriple, dist2)

  rho1(sbmidly, dist1)
  rho2(sbmidly, dist1)
  rho1(sbmidly, dist2)

  println(rho2(sbmidly, dist2))
//-----------------Fin Pruebas rho ------------------------

  //-------------------Pruebas showWeightedGraph ------------------------

  val grafoPrueba: SpecificWeightedGraph = (
    (i: Int, j: Int) =>
      if (i == j) 1.0
      else if (i < j) 1.0 / (j - i).toDouble
      else 0.0,
    5
  )

  println("Prueba showWeightedGraph:")
  showWeightedGraph(grafoPrueba).foreach(println)


def i1(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) =>
    if (i == j) 1.0
    else if (i < j) 1.0 / (j - i).toDouble
    else 0.0,
   nags)
}

def i2(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) =>
    if (i == j) 1.0
    else if (i < j) (j - i).toDouble / nags.toDouble
    else (nags - (i - j)).toDouble / nags.toDouble,
   nags)
}

// --- Instancias específicas para 10 y 20 agentes ---

val i1_10 = i1(10)
val i2_10 = i2(10)
val i1_20 = i1(20)
val i2_20 = i2(20)
val res23 = showWeightedGraph(i1_10);
val res25 = showWeightedGraph(i2_10)
  println("Pruebas del enunciado")
  res23.foreach(println)
  println("---------------------------")
  res25.foreach(println)
println(res23);
println(res25);


  //-----------------Fin Pruebas showWeightedGraph ------------------------



  //-------------------- 2.3.2 — Pruebas confBiasUpdate (Jhonnier) --------------------
  // Verifica la actualización de creencias con el grafo i1 de 10 agentes.
  // Se comparan las polarizaciones antes y después de actualizar.
  // Valores esperados del enunciado:
  //   uniforme: rho antes ~0.383, después ~0.38
  //   mildly:   rho antes y después ~0.435
  println("Pruebas de confBiasUpdate");
  val sbu_10 = uniformBelief(10)
  println(confBiasUpdate(sbu_10, i1_10))
  println(rho1(sbu_10, dist1))
  println(rho1(confBiasUpdate(sbu_10, i1_10), dist1))

  val sbm_10 = midlyBelief(10)
  println(confBiasUpdate(sbm_10, i1_10))
  println(rho1(sbm_10, dist1))
  println(rho1(confBiasUpdate(sbm_10, i1_10), dist1))


  //-------------------Fin Pruebas confBiasUpdate----------------------

  //-------------------- 2.3.3 — Pruebas simulate (Jhonnier) --------------------
  // Simula 2 pasos de tiempo (t=2) y calcula la polarización en cada instante.
  // for-comprehension: recorre cada creencia de la secuencia devuelta por simulate
  // y empareja (creencia, polarización) como en el ejemplo del enunciado.
  // Valores esperados (creencia uniforme): 0.383 -> 0.38 -> 0.335
  // Valores esperados (creencia mildly):    0.435 -> 0.435 -> 0.377
  println("Pruebas de simulate")
  val evolUniforme = for {
    b <- simulate(confBiasUpdate, i1_10, sbu_10, 2)
  } yield (b, rho1(b, dist1))
  evolUniforme.foreach(println)

  val evolMidly = for {
    b <- simulate(confBiasUpdate, i1_10, sbm_10, 2)
  } yield (b, rho1(b, dist1))
  evolMidly.foreach(println)
  //--------------------Fin Pruebas simulate----------------------

}
