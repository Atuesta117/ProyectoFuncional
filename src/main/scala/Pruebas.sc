import Benchmark._
import Opinion._

// ── Creencias genéricas ──────────────────────────────────────────────────────
def uniformBelief(nags: Int): SpecificBelief =
  Vector.tabulate(nags)(i => (i + 1).toDouble / nags.toDouble)

def midlyBelief(nags: Int): SpecificBelief = {
  val middle = nags / 2
  Vector.tabulate(nags)(i =>
    if (i < middle) math.max(0.25 - 0.01 * (middle - i - 1), 0)
    else            math.min(0.75 - 0.01 * (middle - i), 1)
  )
}

def allExtremeBelief(nags: Int): SpecificBelief = {
  val middle = nags / 2
  Vector.tabulate(nags)(i => if (i < middle) 0.0 else 1.0)
}

def allTripleBelief(nags: Int): SpecificBelief = {
  val oneThird = nags / 3
  val twoThird = (nags / 3) * 2
  Vector.tabulate(nags)(i =>
    if (i < oneThird)    0.0
    else if (i >= twoThird) 1.0
    else 0.5
  )
}

def consensusBelief(b: Double)(nags: Int): SpecificBelief =
  Vector.tabulate(nags)(_ => b)

// ── Grafos de influencia ─────────────────────────────────────────────────────
def i1(nags: Int): SpecificWeightedGraph =
  ((i: Int, j: Int) =>
    if (i == j) 1.0
    else if (i < j) 1.0 / (j - i).toDouble
    else 0.0,
    nags)

def i2(nags: Int): SpecificWeightedGraph =
  ((i: Int, j: Int) =>
    if (i == j) 1.0
    else if (i < j) (j - i).toDouble / nags.toDouble
    else (nags - (i - j)).toDouble / nags.toDouble,
    nags)

// ── Instancias de grafos y medidas ───────────────────────────────────────────
val dist1  = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2  = Vector(0.0, 0.2,  0.4,  0.6,  0.8, 1.0)
val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0, 1.0)

val i1_10 = i1(10)
val i2_10 = i2(10)
val i1_20 = i1(20)
val i2_20 = i2(20)

// ── Creencias de prueba puntuales (100 agentes) ──────────────────────────────
val sbext   = allExtremeBelief(100)
val sbcons  = consensusBelief(0.2)(100)
val sbunif  = uniformBelief(100)
val sbtriple = allTripleBelief(100)
val sbmidly = midlyBelief(100)

// ── Pruebas rho ──────────────────────────────────────────────────────────────
val res_rho1_ext_d1   = rho1(sbext,    dist1)
val res_rho2_ext_d1   = rho2(sbext,    dist1)
val res_rho1_ext_d2   = rho1(sbext,    dist2)
val res_rho2_ext_d2   = rho2(sbext,    dist2)
val res_rho1_cons_d1  = rho1(sbcons,   dist1)
val res_rho2_cons_d1  = rho2(sbcons,   dist1)
val res_rho1_cons_d2  = rho1(sbcons,   dist2)
val res_rho2_cons_d2  = rho2(sbcons,   dist2)
val res_rho1_unif_d1  = rho1(sbunif,   dist1)
val res_rho2_unif_d1  = rho2(sbunif,   dist1)
val res_rho1_unif_d2  = rho1(sbunif,   dist2)
val res_rho2_unif_d2  = rho2(sbunif,   dist2)
val res_rho1_trip_d1  = rho1(sbtriple, dist1)
val res_rho2_trip_d1  = rho2(sbtriple, dist1)
val res_rho1_trip_d2  = rho1(sbtriple, dist2)
val res_rho2_trip_d2  = rho2(sbtriple, dist2)
val res_rho1_mid_d1   = rho1(sbmidly,  dist1)
val res_rho2_mid_d1   = rho2(sbmidly,  dist1)
val res_rho1_mid_d2   = rho1(sbmidly,  dist2)
val res_rho2_mid_d2   = rho2(sbmidly,  dist2)

// ── Pruebas showWeightedGraph ────────────────────────────────────────────────
val res_show_i1_10 = showWeightedGraph(i1_10)
val res_show_i2_10 = showWeightedGraph(i2_10)

// ── Pruebas confBiasUpdate ───────────────────────────────────────────────────
val sbu_10 = uniformBelief(10)
val sbm_10 = midlyBelief(10)

val res_cbu_unif      = confBiasUpdate(sbu_10, i1_10)
val res_rho_unif_pre  = rho1(sbu_10, dist1)
val res_rho_unif_post = rho1(res_cbu_unif, dist1)

val res_cbu_midly     = confBiasUpdate(sbm_10, i1_10)
val res_rho_midly_pre = rho1(sbm_10, dist1)
val res_rho_midly_post = rho1(res_cbu_midly, dist1)

// ── Pruebas simulate ─────────────────────────────────────────────────────────
val evolUniforme = for {
  b <- simulate(confBiasUpdate, i1_10, sbu_10, 2)
} yield (b, rho1(b, dist1))

val evolMidly = for {
  b <- simulate(confBiasUpdate, i1_10, sbm_10, 2)
} yield (b, rho1(b, dist1))

// ── Comparación confBiasUpdate vs confBiasUpdatePar ──────────────────────────

/*
val sbms = for {
  n    <- 2 until 16
  nags  = math.pow(2, n).toInt
} yield midlyBelief(nags)

val i132768 = i1(32768)
val i232768 = i2(32768)

//Con hasta 256 agentes

compararFuncionesAct(
  sbms.take(sbms.length / 2),
  i232768,
  confBiasUpdate,
  confBiasUpdatePar
)
*/
//con hasta 32768 agentes
val sbms1 = for {
  n    <- 2 until 12        // 2^2=4 hasta 2^11=2048
  nags  = math.pow(2, n).toInt
} yield midlyBelief(nags)

val i12048 = i1(2048)
val i22048 = i2(2048)

val cmp_i2 = compararFuncionesAct(
  sbms1,
  i22048,
  confBiasUpdate,
  confBiasUpdatePar
)