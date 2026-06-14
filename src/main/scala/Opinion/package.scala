import Comete._


package object Opinion {

  // --------------------FUNCION rho----------------------------
  
  // --------------------DATOS----------------------------------
  type SpecificBelief = Vector[Double]

  // Si b: SpecificBelief, para cada i en Int,
  // b(i) es un número entre 0 y 1
  // que indica cuánto cree el agente i
  // en la veracidad de la proposición p.
  // El número de agentes es b.length.
  // Si existe i: b(i) < 0 o b(i) > 1, b está mal definida.
  // Para i en Int \ A, b(i) no tiene sentido.

  type GenericBelief = Int => SpecificBelief

  // Si gb: GenericBelief, entonces gb(n) = b
  // tal que b: SpecificBelief.
  //es el tipo de las funciones generadoras de creencias


  type AgentsPolMeasure =
    (SpecificBelief, DistributionValues) => Double

  // Si rho: AgentsPolMeasure y sb: SpecificBelief y
  // d: DistributionValues,
  // rho(sb, d) es la polarización de los agentes
  // de acuerdo a esa medida.
  

//----------------rho----------------------------

  //La siguiente función calcula la polarización comete normalizada adaptada a la población de agentes de la red. Practicamente calcula la polarización de la red teniendo en cuenta la distribución de opiniones de los agentes y los valores asociados a cada opinión, utilizando la medida COMETE normalizada.
  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    (sb: SpecificBelief, d: DistributionValues) => {
      val k = d.length
      val n = sb.length

      // Mapea la creencia de la red hacia la distribución
      val pi = Vector.tabulate(k) { i =>
        val count = sb.count { v =>
          if (i == 0) v >= 0.0 && v < d(1) / 2.0
          else if (i == k - 1) v >= (d(k - 2) + d(k - 1)) / 2.0 && v <= 1.0
          else v >= (d(i - 1) + d(i)) / 2.0 && v < (d(i) + d(i + 1)) / 2.0
        }
        count.toDouble / n.toDouble
      }

      val dist = (pi, d)

      // Calcula la medida COMETE normalizada y la evalúa
      val comete = rhoCMT_Gen(alpha, beta)
      val cometeNorm = normalizar(comete)

      cometeNorm(dist)
    }
  }









}
