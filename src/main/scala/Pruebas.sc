import Benchmark._
import Opinion._
import Comete._

// Si este worksheet muestra errores en rojo, usa PruebasApp.scala en su lugar:
// clic derecho → Run 'PruebasApp'

import java.io.PrintWriter
import java.nio.file.Paths

// ── Utilidades de validación ───────────────────────────────────────────────────
def approxEq(a: Double, b: Double, tol: Double = 0.001): Boolean =
  math.abs(a - b) <= tol

def checkDouble(nombre: String, obtenido: Double, esperado: Double, tol: Double = 0.001): String = {
  val ok = approxEq(obtenido, esperado, tol)
  f"[$nombre] ${if (ok) "OK" else "FALLO"}  esperado=$esperado%.3f  obtenido=$obtenido%.3f"
}

def checkVector(nombre: String, obtenido: Vector[Double], esperado: Vector[Double], tol: Double = 0.001): String = {
  val ok = obtenido.length == esperado.length &&
    obtenido.zip(esperado).forall { case (a, b) => approxEq(a, b, tol) }
  s"[$nombre] ${if (ok) "OK" else "FALLO"}  (longitud ${obtenido.length})"
}

def guardarLineas(ruta: String, lineas: Seq[String]): Unit = {
  val pw = new PrintWriter(ruta, "UTF-8")
  try lineas.foreach(pw.println)
  finally pw.close()
}

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
    if (i < oneThird)       0.0
    else if (i >= twoThird) 1.0
    else                    0.5
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

// ── Datos comunes ────────────────────────────────────────────────────────────
val dist1   = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2   = Vector(0.0, 0.2,  0.4,  0.6,  0.8, 1.0)
val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

val rho1    = rho(1.2, 1.2)
val rho2    = rho(2.0, 1.0)
val polSec  = rho(1.2, 1.2)
val polPar  = rhoPar(1.2, 1.2)

val i1_10   = i1(10)
val i2_10   = i2(10)

val sbext    = allExtremeBelief(100)
val sbcons   = consensusBelief(0.2)(100)
val sbunif   = uniformBelief(100)
val sbtriple = allTripleBelief(100)
val sbmidly  = midlyBelief(100)

val sbu_10 = uniformBelief(10)
val sbm_10 = midlyBelief(10)

// ══════════════════════════════════════════════════════════════════════════════
// 2.1.1 — Validación Comete (valores del PDF)
// ══════════════════════════════════════════════════════════════════════════════
val pi_max        = Vector(0.5, 0.0, 0.0, 0.0, 0.5)
val pi_min        = Vector(0.0, 0.0, 1.0, 0.0, 0.0)
val pi_der        = Vector(0.4, 0.0, 0.0, 0.0, 0.6)
val pi_izq        = Vector(0.6, 0.0, 0.0, 0.0, 0.4)
val pi_int1       = Vector(0.0, 0.5, 0.0, 0.5, 0.0)
val pi_int2       = Vector(0.25, 0.0, 0.5, 0.0, 0.25)
val pi_int3       = Vector(0.25, 0.25, 0.0, 0.25, 0.25)
val pi_conscentro = pi_min
val pi_consder    = Vector(0.0, 0.0, 0.0, 0.0, 1.0)
val pi_consigz    = Vector(1.0, 0.0, 0.0, 0.0, 0.0)

val cmt1      = rhoCMT_Gen(1.2, 1.2)
val cmt1_norm = normalizar(cmt1)

val validacionComete = Seq(
  checkDouble("cmt1 pi_max",        cmt1((pi_max, likert5)),        0.379),
  checkDouble("cmt1 pi_min",        cmt1((pi_min, likert5)),        0.0),
  checkDouble("cmt1 pi_der",        cmt1((pi_der, likert5)),        0.327),
  checkDouble("cmt1 pi_izq",        cmt1((pi_izq, likert5)),        0.327),
  checkDouble("cmt1 pi_int1",       cmt1((pi_int1, likert5)),       0.165),
  checkDouble("cmt1 pi_int2",       cmt1((pi_int2, likert5)),       0.165),
  checkDouble("cmt1 pi_int3",       cmt1((pi_int3, likert5)),       0.237),
  checkDouble("cmt1 pi_conscentro", cmt1((pi_conscentro, likert5)), 0.0),
  checkDouble("cmt1 pi_consder",    cmt1((pi_consder, likert5)),    0.0),
  checkDouble("cmt1 pi_consigz",    cmt1((pi_consigz, likert5)),    0.0),
  checkDouble("cmt1_norm pi_max",        cmt1_norm((pi_max, likert5)),        1.0),
  checkDouble("cmt1_norm pi_min",        cmt1_norm((pi_min, likert5)),        0.0),
  checkDouble("cmt1_norm pi_der",        cmt1_norm((pi_der, likert5)),        0.863),
  checkDouble("cmt1_norm pi_izq",        cmt1_norm((pi_izq, likert5)),        0.863),
  checkDouble("cmt1_norm pi_int1",       cmt1_norm((pi_int1, likert5)),       0.435),
  checkDouble("cmt1_norm pi_int2",       cmt1_norm((pi_int2, likert5)),       0.435),
  checkDouble("cmt1_norm pi_int3",       cmt1_norm((pi_int3, likert5)),       0.625),
  checkDouble("cmt1_norm pi_conscentro", cmt1_norm((pi_conscentro, likert5)), 0.0),
  checkDouble("cmt1_norm pi_consder",    cmt1_norm((pi_consder, likert5)),    0.0),
  checkDouble("cmt1_norm pi_consigz",    cmt1_norm((pi_consigz, likert5)),    0.0)
)

// ══════════════════════════════════════════════════════════════════════════════
// 2.2.1 — Validación rho (valores del PDF)
// ══════════════════════════════════════════════════════════════════════════════
val validacionRho = Seq(
  checkDouble("rho1 sbext dist1",   rho1(sbext,    dist1), 1.0,   0.01),
  checkDouble("rho2 sbext dist1",   rho2(sbext,    dist1), 1.0,   0.01),
  checkDouble("rho1 sbext dist2",   rho1(sbext,    dist2), 1.0,   0.01),
  checkDouble("rho2 sbext dist2",   rho2(sbext,    dist2), 1.0,   0.01),
  checkDouble("rho1 sbcons dist1",  rho1(sbcons,   dist1), 0.0,   0.01),
  checkDouble("rho2 sbcons dist1",  rho2(sbcons,   dist1), 0.0,   0.01),
  checkDouble("rho1 sbcons dist2",  rho1(sbcons,   dist2), 0.0,   0.01),
  checkDouble("rho2 sbcons dist2",  rho2(sbcons,   dist2), 0.0,   0.01),
  checkDouble("rho1 sbunif dist1",  rho1(sbunif,   dist1), 0.38,  0.01),
  checkDouble("rho2 sbunif dist1",  rho2(sbunif,   dist1), 0.188, 0.01),
  checkDouble("rho1 sbunif dist2",  rho1(sbunif,   dist2), 0.377, 0.01),
  checkDouble("rho2 sbunif dist2",  rho2(sbunif,   dist2), 0.172, 0.01),
  checkDouble("rho1 sbtriple dist1", rho1(sbtriple, dist1), 0.617, 0.01),
  checkDouble("rho2 sbtriple dist1", rho2(sbtriple, dist1), 0.448, 0.01),
  checkDouble("rho1 sbtriple dist2", rho1(sbtriple, dist2), 0.617, 0.01),
  checkDouble("rho2 sbtriple dist2", rho2(sbtriple, dist2), 0.448, 0.01),
  checkDouble("rho1 sbmidly dist1",  rho1(sbmidly,  dist1), 0.784, 0.01),
  checkDouble("rho2 sbmidly dist1",  rho2(sbmidly,  dist1), 0.58,  0.01),
  checkDouble("rho1 sbmidly dist2",  rho1(sbmidly,  dist2), 0.773, 0.01),
  checkDouble("rho2 sbmidly dist2",  rho2(sbmidly,  dist2), 0.528, 0.01)
)

// ══════════════════════════════════════════════════════════════════════════════
// 2.3.2 / 2.3.3 — Validación confBiasUpdate y simulate
// ══════════════════════════════════════════════════════════════════════════════
val res_cbu_unif = confBiasUpdate(sbu_10, i1_10)
val res_cbu_midly = confBiasUpdate(sbm_10, i1_10)

val esperado_cbu_unif = Vector(
  0.1, 0.155, 0.24333333333333332, 0.34, 0.44,
  0.5416666666666666, 0.6442857142857142, 0.7475,
  0.8511111111111112, 0.955
)

val esperado_cbu_midly = Vector(
  0.21, 0.21505, 0.22343333333333334, 0.23265, 0.2422,
  0.6549825, 0.7069635714285715, 0.733586875,
  0.7523952777777778, 0.7677165833333334
)

val evolUniforme = for {
  b <- simulate(confBiasUpdate, i1_10, sbu_10, 2)
} yield (b, rho1(b, dist1))

val evolMidly = for {
  b <- simulate(confBiasUpdate, i1_10, sbm_10, 2)
} yield (b, rho1(b, dist1))

val validacionDinamica = Seq(
  checkVector("confBiasUpdate uniforme", res_cbu_unif, esperado_cbu_unif),
  checkDouble("rho uniforme pre",  rho1(sbu_10, dist1),      0.383, 0.01),
  checkDouble("rho uniforme post", rho1(res_cbu_unif, dist1), 0.38,  0.01),
  checkVector("confBiasUpdate midly", res_cbu_midly, esperado_cbu_midly),
  checkDouble("rho midly pre",  rho1(sbm_10, dist1),       0.435, 0.01),
  checkDouble("rho midly post", rho1(res_cbu_midly, dist1), 0.435, 0.01),
  checkDouble("simulate uniforme t=0 rho", evolUniforme(0)._2, 0.383, 0.01),
  checkDouble("simulate uniforme t=1 rho", evolUniforme(1)._2, 0.38,  0.01),
  checkDouble("simulate uniforme t=2 rho", evolUniforme(2)._2, 0.335, 0.01),
  checkDouble("simulate midly t=0 rho",    evolMidly(0)._2,    0.435, 0.01),
  checkDouble("simulate midly t=1 rho",    evolMidly(1)._2,    0.435, 0.01),
  checkDouble("simulate midly t=2 rho",    evolMidly(2)._2,    0.377, 0.01)
)

val resumenValidacion = validacionComete ++ validacionRho ++ validacionDinamica
resumenValidacion.foreach(println)

// ══════════════════════════════════════════════════════════════════════════════
// 2.4.3 — Benchmarks para el informe (las 3 comparaciones del PDF)
// ══════════════════════════════════════════════════════════════════════════════
val sbms = for {
  n    <- 2 until 16
  nags  = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes = for {
  n    <- 2 until 16
  nags  = math.pow(2, n).toInt
} yield allExtremeBelief(nags)

val sbts = for {
  n    <- 2 until 16
  nags  = math.pow(2, n).toInt
} yield allTripleBelief(nags)

val i2_32768 = i2(32768)
val pasosSim   = 10
val sbmsAct    = sbms.take(sbms.length / 2)

val cmpMedidasPol = compararMedidasPol(sbms, likert5, polSec, polPar)

val cmpFuncionesAct = compararFuncionesAct(
  sbmsAct,
  i2_32768,
  confBiasUpdate,
  confBiasUpdatePar
)

val cmpSimulacion = compararSimulacion(
  sbmsAct,
  i2_32768,
  pasosSim,
  confBiasUpdate,
  confBiasUpdatePar
)

val archivosHtml = Seq(4, 6, 8).flatMap { i =>
  val n = sbms(i).length
  val creencias = Seq(sbms(i), sbes(i), sbts(i))
  val nombreEvol = s"SimulacionSec-$n"
  val nombreCmp  = s"SimulacionCmp-$n"
  simEvolucion(creencias, i2_32768, pasosSim, polSec, confBiasUpdate, likert5, nombreEvol)
  simEvolucionComparativa(
    creencias, i2_32768, pasosSim, polSec,
    confBiasUpdate, confBiasUpdatePar, likert5, nombreCmp
  )
  Seq(nombreEvol + ".html", nombreCmp + ".html")
}

// ── Tablas para el informe ───────────────────────────────────────────────────
val rutaTablas = Paths.get(System.getProperty("user.dir"), "tablas_informe.txt").toString

def esOk(linea: String): Boolean = linea.contains("] OK")

val tablaMedidasPol = {
  val cabecera = f"${"n"}%8s  ${"p_sec"}%12s  ${"p_par"}%12s  ${"t_sec(ms)"}%14s  ${"t_par(ms)"}%14s  ${"aceleracion"}%12s"
  val filas = cmpMedidasPol.map { case (n, p1, p2, t1, t2, acel) =>
    f"$n%8d  $p1%12.4f  $p2%12.4f  ${t1.value}%14.4f  ${t2.value}%14.4f  $acel%12.4f"
  }
  cabecera +: filas
}

val tablaFuncionesAct = {
  val cabecera = f"${"n"}%8s  ${"t_sec(ms)"}%14s  ${"t_par(ms)"}%14s  ${"aceleracion"}%12s"
  val filas = cmpFuncionesAct.map { case (n, t1, t2, acel) =>
    f"$n%8d  ${t1.value}%14.4f  ${t2.value}%14.4f  $acel%12.4f"
  }
  cabecera +: filas
}

val tablaSimulacion = {
  val cabecera = f"${"n"}%8s  ${"pasos"}%8s  ${"t_sec(ms)"}%14s  ${"t_par(ms)"}%14s  ${"aceleracion"}%12s"
  val filas = cmpSimulacion.map { case (n, pasos, t1, t2, acel) =>
    f"$n%8d  $pasos%8d  ${t1.value}%14.4f  ${t2.value}%14.4f  $acel%12.4f"
  }
  cabecera +: filas
}

val lineasInforme = Seq(
  "═══════════════════════════════════════════════════════════════",
  " VALIDACIÓN — comparación con valores del enunciado (PDF)",
  "═══════════════════════════════════════════════════════════════",
  ""
) ++ resumenValidacion ++ Seq(
  "",
  s"Total pruebas: ${resumenValidacion.length}",
  s"Exitosas:      ${resumenValidacion.count(esOk)}",
  s"Fallidas:      ${resumenValidacion.count(_.contains("FALLO"))}",
  "",
  "═══════════════════════════════════════════════════════════════",
  " TABLA 1 — compararMedidasPol (rho secuencial vs rhoPar)",
  "═══════════════════════════════════════════════════════════════",
  ""
) ++ tablaMedidasPol ++ Seq(
  "",
  "═══════════════════════════════════════════════════════════════",
  " TABLA 2 — compararFuncionesAct (confBiasUpdate vs confBiasUpdatePar)",
  " Agentes probados: " + cmpFuncionesAct.map(_._1).mkString(", "),
  "═══════════════════════════════════════════════════════════════",
  ""
) ++ tablaFuncionesAct ++ Seq(
  "",
  "═══════════════════════════════════════════════════════════════",
  s" TABLA 3 — compararSimulacion ($pasosSim pasos)",
  "═══════════════════════════════════════════════════════════════",
  ""
) ++ tablaSimulacion ++ Seq(
  "",
  "═══════════════════════════════════════════════════════════════",
  " GRÁFICOS HTML",
  "═══════════════════════════════════════════════════════════════",
  ""
) ++ archivosHtml.map(f => s"  $f") ++ Seq(
  "",
  "Nota: la aceleración es t_sec / t_par. Valores > 1 indican que la versión",
  "paralela es más rápida. Los tiempos provienen de org.scalameter."
)

guardarLineas(rutaTablas, lineasInforme)

println(s"\nTablas guardadas en: $rutaTablas")
println("\n── TABLA 1: compararMedidasPol ──")
tablaMedidasPol.foreach(println)
println("\n── TABLA 2: compararFuncionesAct ──")
tablaFuncionesAct.foreach(println)
println("\n── TABLA 3: compararSimulacion ──")
tablaSimulacion.foreach(println)
println(s"\n── Gráficos HTML: ${archivosHtml.length} archivos ──")
archivosHtml.foreach(println)
