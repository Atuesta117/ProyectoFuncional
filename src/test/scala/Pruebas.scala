import Comete._
import Opinion._

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

  //-----------------Fin Pruebas showWeightedGraph ------------------------

}
