import Benchmark._
import Opinion._
import Comete._

import java.io.PrintWriter
import java.nio.file.Paths

/** Ejecutable alternativo a Pruebas.sc (worksheet).
  *
  * En IntelliJ: clic derecho → Run 'PruebasApp'
  *
  * Modos (Program arguments en Run Configuration):
  *   (vacío)    → validación + tablas de desempeño para el informe
  *   --rapido   → solo validación del PDF
  *   --graficos → solo gráficos HTML de simulación (sin benchmarks)
  *   --tabla3   → solo Tabla 3 (compararSimulacion; ~5-15 min)
  */
object PruebasApp extends App {

  // Evita el error "Unable to create a system terminal" de JLine/Scalameter en IntelliJ (Windows)
  System.setProperty("jline.terminal", "org.jline.terminal.impl.DumbTerminal")

  val modo = args.headOption.getOrElse("")
  val soloGraficos       = modo == "--graficos"
  val soloTabla3         = modo == "--tabla3"
  val ejecutarBenchmarks = modo != "--rapido" && !soloGraficos

  def esOk(linea: String): Boolean = linea.contains("] OK")

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

  // ── Validación Comete ──────────────────────────────────────────────────────
  val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
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
    checkDouble("cmt1_norm pi_der",        cmt1_norm((pi_der, likert5)),        0.863, 0.002),
    checkDouble("cmt1_norm pi_izq",        cmt1_norm((pi_izq, likert5)),        0.863, 0.002),
    checkDouble("cmt1_norm pi_int1",       cmt1_norm((pi_int1, likert5)),       0.435),
    checkDouble("cmt1_norm pi_int2",       cmt1_norm((pi_int2, likert5)),       0.435),
    checkDouble("cmt1_norm pi_int3",       cmt1_norm((pi_int3, likert5)),       0.625),
    checkDouble("cmt1_norm pi_conscentro", cmt1_norm((pi_conscentro, likert5)), 0.0),
    checkDouble("cmt1_norm pi_consder",    cmt1_norm((pi_consder, likert5)),    0.0),
    checkDouble("cmt1_norm pi_consigz",    cmt1_norm((pi_consigz, likert5)),    0.0)
  )

  // ── Validación rho ─────────────────────────────────────────────────────────
  val dist1   = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
  val dist2   = Vector(0.0, 0.2,  0.4,  0.6,  0.8, 1.0)
  val rho1    = rho(1.2, 1.2)
  val rho2    = rho(2.0, 1.0)
  val polSec  = rho(1.2, 1.2)
  val polPar  = rhoPar(1.2, 1.2)

  val sbext    = allExtremeBelief(100)
  val sbcons   = consensusBelief(0.2)(100)
  val sbunif   = uniformBelief(100)
  val sbtriple = allTripleBelief(100)
  val sbmidly  = midlyBelief(100)

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

  // ── Validación confBiasUpdate y simulate ───────────────────────────────────
  val i1_10  = i1(10)
  val sbu_10 = uniformBelief(10)
  val sbm_10 = midlyBelief(10)

  val res_cbu_unif  = confBiasUpdate(sbu_10, i1_10)
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
  println("═══ VALIDACIÓN ═══")
  resumenValidacion.foreach(println)
  val ok  = resumenValidacion.count(esOk)
  val bad = resumenValidacion.count(_.contains("FALLO"))
  println(s"\nResumen: $ok OK, $bad FALLO de ${resumenValidacion.length} pruebas")

  def ejecutarBenchmarksYGuardar(resumen: Seq[String], soloTabla3: Boolean = false): Unit = {
    val rutaTablas = Paths.get(System.getProperty("user.dir"), "tablas_informe.txt").toString
    val pasosSim = 10

    val sbms = for {
      n    <- 2 until 16
      nags  = math.pow(2, n).toInt
    } yield midlyBelief(nags)

    val i2_32768 = i2(32768)
    val sbmsAct  = sbms.take(sbms.length / 2)

    def fmtTablaMedidasPol(rows: Seq[(Int, Double, Double, org.scalameter.Quantity[Double], org.scalameter.Quantity[Double], Double)]) = {
      val cabecera = f"${"n"}%8s  ${"p_sec"}%12s  ${"p_par"}%12s  ${"t_sec(ms)"}%14s  ${"t_par(ms)"}%14s  ${"aceleracion"}%12s"
      cabecera +: rows.map { case (n, p1, p2, t1, t2, acel) =>
        f"$n%8d  $p1%12.4f  $p2%12.4f  ${t1.value}%14.4f  ${t2.value}%14.4f  $acel%12.4f"
      }
    }

    def fmtTablaFuncionesAct(rows: Seq[(Int, org.scalameter.Quantity[Double], org.scalameter.Quantity[Double], Double)]) = {
      val cabecera = f"${"n"}%8s  ${"t_sec(ms)"}%14s  ${"t_par(ms)"}%14s  ${"aceleracion"}%12s"
      cabecera +: rows.map { case (n, t1, t2, acel) =>
        f"$n%8d  ${t1.value}%14.4f  ${t2.value}%14.4f  $acel%12.4f"
      }
    }

    def fmtTablaSimulacion(rows: Seq[(Int, Int, org.scalameter.Quantity[Double], org.scalameter.Quantity[Double], Double)]) = {
      val cabecera = f"${"n"}%8s  ${"pasos"}%8s  ${"t_sec(ms)"}%14s  ${"t_par(ms)"}%14s  ${"aceleracion"}%12s"
      cabecera +: rows.map { case (n, pasos, t1, t2, acel) =>
        f"$n%8d  $pasos%8d  ${t1.value}%14.4f  ${t2.value}%14.4f  $acel%12.4f"
      }
    }

    def guardarInforme(
        tablaMedidasPol: Option[Seq[String]],
        tablaFuncionesAct: Option[Seq[String]],
        tablaSimulacion: Option[Seq[String]],
        archivosHtml: Seq[String] = Nil
    ): Unit = {
      val lineas = Seq(
        "VALIDACIÓN — comparación con valores del enunciado (PDF)",
        ""
      ) ++ resumen ++ Seq(
        "",
        s"Total pruebas: ${resumen.length}",
        s"Exitosas:      ${resumen.count(esOk)}",
        s"Fallidas:      ${resumen.count(_.contains("FALLO"))}",
        ""
      ) ++ tablaMedidasPol.map(t => Seq("TABLA 1 — compararMedidasPol (rho secuencial vs rhoPar)", "") ++ t ++ Seq("")).getOrElse(
        Seq("(Tabla 1 pendiente)", "")
      ) ++ tablaFuncionesAct.map(t => Seq("TABLA 2 — compararFuncionesAct (confBiasUpdate vs confBiasUpdatePar)", "") ++ t ++ Seq("")).getOrElse(
        Seq("(Tabla 2 pendiente)", "")
      ) ++ tablaSimulacion.map(t => Seq(
        s"TABLA 3 — compararSimulacion (simulate con confBiasUpdate vs confBiasUpdatePar, $pasosSim pasos)",
        ""
      ) ++ t ++ Seq("")).getOrElse(
        Seq(s"(Tabla 3 pendiente — compararSimulacion en curso o no ejecutada)", "")
      ) ++ (if (archivosHtml.nonEmpty) Seq(
        "GRÁFICOS HTML — evolución de polarización",
        "  SimulacionSec-*.html  : evolución con actualización secuencial (3 creencias iniciales)",
        "  SimulacionCmp-*.html  : secuencial vs paralelo en el mismo gráfico",
        ""
      ) ++ archivosHtml.map(f => s"  $f") ++ Seq("") else Nil) ++ Seq(
        "Nota: la aceleración es t_sec / t_par. Valores > 1 indican que la versión",
        "paralela es más rápida. Los tiempos provienen de org.scalameter."
      )
      guardarLineas(rutaTablas, lineas)
      println(s"   (guardado parcial en $rutaTablas)")
      Console.out.flush()
    }

    var tablaMedidasPol: Option[Seq[String]] = None
    var tablaFuncionesAct: Option[Seq[String]] = None
    var cmpSimulacion: Seq[(Int, Int, org.scalameter.Quantity[Double], org.scalameter.Quantity[Double], Double)] = Nil

    if (!soloTabla3) {
      println("\n═══ BENCHMARKS (puede tardar varios minutos) ═══")

      println("→ compararMedidasPol (rho vs rhoPar)...")
      println("   (Scalameter: ~1-3 min, sin salida intermedia — es normal)")
      Console.out.flush()
      val cmpMedidasPol = compararMedidasPol(sbms, likert5, polSec, polPar)
      println(s"   listo (${cmpMedidasPol.length} mediciones)")
      tablaMedidasPol = Some(fmtTablaMedidasPol(cmpMedidasPol))
      guardarInforme(tablaMedidasPol, None, None)

      println("→ compararFuncionesAct (confBiasUpdate vs confBiasUpdatePar)...")
      Console.out.flush()
      val cmpFuncionesAct = compararFuncionesAct(
        sbmsAct,
        i2_32768,
        confBiasUpdate,
        confBiasUpdatePar
      )
      println(s"   listo (${cmpFuncionesAct.length} mediciones)")
      tablaFuncionesAct = Some(fmtTablaFuncionesAct(cmpFuncionesAct))
      guardarInforme(tablaMedidasPol, tablaFuncionesAct, None)
    } else {
      println("\n═══ SOLO TABLA 3 (compararSimulacion) ═══")
    }

    println(s"\n→ compararSimulacion (simulate sec vs par, $pasosSim pasos)...")
    println("   IMPORTANTE: la Tabla 3 aparece en tablas_informe.txt solo cuando termine esta fase.")
    println("   Verás una línea por cada n (4, 8, 16, …, 256). Puede tardar 5-15 min.")
    Console.out.flush()
    cmpSimulacion = compararSimulacion(
      sbmsAct,
      i2_32768,
      pasosSim,
      confBiasUpdate,
      confBiasUpdatePar
    )
    println(s"   listo (${cmpSimulacion.length} mediciones)")
    val tablaSimulacion = fmtTablaSimulacion(cmpSimulacion)

    if (soloTabla3) {
      val rutaTabla3 = Paths.get(System.getProperty("user.dir"), "tabla3_informe.txt").toString
      guardarLineas(
        rutaTabla3,
        Seq(s"TABLA 3 — compararSimulacion ($pasosSim pasos)", "") ++ tablaSimulacion
      )
      println(s"\nTabla 3 guardada en: $rutaTabla3")
      println("\n── TABLA 3: compararSimulacion ──")
      tablaSimulacion.foreach(println)
      println("\nCopia esta tabla a la sección 3.4 del informe. Tablas 1 y 2 no se modifican.")
      return
    }

    guardarInforme(tablaMedidasPol, tablaFuncionesAct, Some(tablaSimulacion))

    val sbes = for {
      n    <- 2 until 16
      nags  = math.pow(2, n).toInt
    } yield allExtremeBelief(nags)

    val sbts = for {
      n    <- 2 until 16
      nags  = math.pow(2, n).toInt
    } yield allTripleBelief(nags)

    val indicesGraficos = Seq(4, 6, 8)
    val archivosHtml = indicesGraficos.flatMap { i =>
      val n = sbms(i).length
      val creencias = Seq(sbms(i), sbes(i), sbts(i))
      val nombreEvol = s"SimulacionSec-$n"
      val nombreCmp  = s"SimulacionCmp-$n"

      println(s"→ generando $nombreEvol.html y $nombreCmp.html ($n agentes)...")
      Console.out.flush()

      simEvolucion(
        creencias, i2_32768, pasosSim, polSec, confBiasUpdate, likert5, nombreEvol
      )
      simEvolucionComparativa(
        creencias, i2_32768, pasosSim, polSec,
        confBiasUpdate, confBiasUpdatePar, likert5, nombreCmp
      )

      Seq(nombreEvol + ".html", nombreCmp + ".html")
    }

    guardarInforme(tablaMedidasPol, tablaFuncionesAct, Some(tablaSimulacion), archivosHtml)

    println(s"\nTablas guardadas en: $rutaTablas")
    println("\n── TABLA 1: compararMedidasPol ──")
    tablaMedidasPol.foreach(_.foreach(println))
    println("\n── TABLA 2: compararFuncionesAct ──")
    tablaFuncionesAct.foreach(_.foreach(println))
    println("\n── TABLA 3: compararSimulacion ──")
    tablaSimulacion.foreach(println)
    println(s"\n── Gráficos HTML: ${archivosHtml.length} archivos generados ──")
    archivosHtml.foreach(f => println(s"   $f"))
  }

  def generarGraficosSimulacion(): Unit = {
    val pasosSim = 10
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
    val indicesGraficos = Seq(4, 6, 8) // 64, 256 y 1024 agentes

    println("\n═══ GRÁFICOS HTML (simulación completa, 10 pasos) ═══")
    val archivosHtml = indicesGraficos.flatMap { i =>
      val n = sbms(i).length
      val creencias = Seq(sbms(i), sbes(i), sbts(i))
      val nombreEvol = s"SimulacionSec-$n"
      val nombreCmp  = s"SimulacionCmp-$n"

      println(s"→ generando $nombreEvol.html y $nombreCmp.html ($n agentes)...")
      Console.out.flush()

      simEvolucion(
        creencias, i2_32768, pasosSim, polSec, confBiasUpdate, likert5, nombreEvol
      )
      simEvolucionComparativa(
        creencias, i2_32768, pasosSim, polSec,
        confBiasUpdate, confBiasUpdatePar, likert5, nombreCmp
      )

      Seq(nombreEvol + ".html", nombreCmp + ".html")
    }

    val dir = System.getProperty("user.dir")
    println(s"\n${archivosHtml.length} archivos generados en: $dir")
    archivosHtml.foreach(f => println(s"   $f"))
    println("\nAbre los .html con el navegador y captura la pantalla para el informe.")
  }

  if (soloGraficos) {
    generarGraficosSimulacion()
  } else if (!ejecutarBenchmarks) {
    println("\n(Solo validación. Para tablas: sin argumentos. Para solo Tabla 3: --tabla3. Para gráficos: --graficos)")
  } else {
    ejecutarBenchmarksYGuardar(resumenValidacion, soloTabla3 = soloTabla3)
  }
}
