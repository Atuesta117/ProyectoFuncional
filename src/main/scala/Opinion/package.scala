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
  // --------------------FUNCION DE INFLUENCIA----------------------------

  type WeightedGraph = (Int, Int) => Double

  type SpecificWeightedGraph =(WeightedGraph, Int)

  type GenericWeightedGraph = Int => SpecificWeightedGraph

  def showWeightedGraph(swg: SpecificWeightedGraph ): IndexedSeq[IndexedSeq[Double]] = {

    val (wg, nags) = swg
    for{
      i <- 0 until nags
    } yield for {
          j <- 0 until nags
    } yield wg(i,j)
  
  }



  // -------------------- 2.3.2 y 2.3.3 — Jhonnier --------------------
  // Sección dinámica del modelo: actualización de creencias y simulación.

  // Tipo de función de alto orden para representar cualquier regla de actualización.
  // Recibe la creencia actual y el grafo, y devuelve la creencia del siguiente paso.
  // Se usa en simulate para poder pasar confBiasUpdate (u otra regla) como parámetro.
  type FunctionUpdate =
    (SpecificBelief, SpecificWeightedGraph) => SpecificBelief

  // 2.3.2 — Actualización con sesgo de confirmación (Confirmation Bias).
  //
  // Fórmula del enunciado, para cada agente i:
  //   nb(i) = b(i) + sum_{j in Ai} beta_ij * I(j,i) * (b(j) - b(i)) / |Ai|
  // donde:
  //   Ai     = agentes j con influencia positiva sobre i  (wg(j,i) > 0)
  //   beta_ij = 1 - |b(j) - b(i)|  (más confianza si las opiniones son cercanas)
  //   I(j,i)  = peso del grafo wg(j,i)
  //
  // Técnicas usadas:
  //   - for-comprehension: recorre cada agente i (iterador)
  //   - pattern matching: si Ai está vacío, la creencia no cambia
  //   - inmutabilidad: se construye un nuevo Vector, no se modifica b
  def confBiasUpdate(b: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val (wg, nags) = swg
    (for {
      i <- 0 until nags
    } yield {
      // Conjunto Ai: índices de agentes que influyen en i
      val Ai = for {
        j <- 0 until nags
        if wg(j, i) > 0
      } yield j

      Ai match {
        case ai if ai.isEmpty => b(i)
        case ai =>
          val sumatoria = (for {
            j <- ai
          } yield {
            val beta_ij = 1 - math.abs(b(j) - b(i))
            beta_ij * wg(j, i) * (b(j) - b(i))
          }).sum
          b(i) + sumatoria / ai.length
      }
    }).toVector
  }

  // 2.3.3 — Simulación de la evolución de la polarización.
  //
  // Aplica la función de actualización fu, t veces sobre la creencia inicial b0:
  //   b0  -> creencia en t=0
  //   b1  = fu(b0, swg)
  //   b2  = fu(b1, swg)
  //   ...
  //   bt  = fu(b_{t-1}, swg)
  //
  // Devuelve IndexedSeq con t+1 creencias (desde t=0 hasta t=t).
  //
  // Técnicas usadas:
  //   - recursión con función auxiliar aux (requerido por el curso)
  //   - acumulador acc: va guardando cada creencia intermedia sin mutar estado
  //   - caso base k > t: detiene la recursión y devuelve el historial
  def simulate(fu: FunctionUpdate,
               swg: SpecificWeightedGraph,
               b0: SpecificBelief,
               t: Int): IndexedSeq[SpecificBelief] = {
    def aux(b: SpecificBelief, k: Int, acc: Vector[SpecificBelief]): Vector[SpecificBelief] =
      if (k > t) acc
      else aux(fu(b, swg), k + 1, acc :+ b)

    if (t < 0) Vector.empty
    else aux(b0, 0, Vector.empty)
  }

  // -------------------- Fin 2.3.2 y 2.3.3 --------------------

}
