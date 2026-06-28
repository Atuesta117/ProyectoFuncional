import scala.collection.parallel.CollectionConverters._

import Comete._
import Opinion._
import common._

def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
  (sb: SpecificBelief, d: DistributionValues) => {
    val k = d.length
    val n = sb.length

    // Paralelizamos tanto la iteración sobre los intervalos (k) 
    // como el conteo sobre las creencias (n) usando .par
    val pi = Vector.tabulate(k).par.map { i =>
      val count = sb.par.count { v => 
        if (i == 0) v >= 0.0 && v < d(1) / 2.0
        else if (i == k - 1) v >= (d(k - 2) + d(k - 1)) / 2.0 && v <= 1.0
        else v >= (d(i - 1) + d(i)) / 2.0 && v < (d(i) + d(i + 1)) / 2.0
      }
      count.toDouble / n.toDouble
    }.seq.toVector // Volvemos a convertir a Vector secuencial para mantener el tipo
    
    val dist = (pi, d)
    
    val comete = rhoCMT_Gen(alpha, beta)
    val cometeNorm = normalizar(comete)
    
    cometeNorm(dist)
  }
}