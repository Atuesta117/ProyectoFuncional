// Configuración del proyecto y dependencias necesarias para compilar.
// scalameter-core: usado por Benchmark para medir tiempos de ejecución.
// plotly-render: usado por Benchmark.simEvolucion para generar gráficos HTML.
ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "ProyectoFuncional",
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter-core" % "0.21",
      "org.plotly-scala" %% "plotly-render" % "0.8.5"
    )
  )
