// Configuración del proyecto y dependencias necesarias para compilar.
// scalameter-core: usado por Benchmark para medir tiempos de ejecución.
// plotly-render: usado por Benchmark.simEvolucion para generar gráficos HTML.
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "Taller Paralelismo de Tareas y Datos"
  )

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  ("com.storm-enroute" %% "scalameter-core" % "0.21").cross(CrossVersion.for3Use2_13),
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
  "org.scalameta" %% "munit" % "0.7.26" % Test,
  "org.plotly-scala" %% "plotly-render" % "0.8.4"
)