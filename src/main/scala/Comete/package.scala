package object Comete {
 
//vector (una lista indexada) de números decimales
type DistributionValues = Vector[Double]

//vector de decimales
type Frequency = Vector[Double]

//Una tupla (un par ordenado) que junta las dos cosas anteriores.Representa una Distribución de Probabilidad completa (frecuencias + valores).
type Distribution = (Frequency, DistributionValues)


//Este tipo recibe una Distribution y te devuelve un solo número decimal (Double).
type MedidaPol = Distribution => Double


  //------------------------------------------------FUNCIONES----------------------------------------------//
//A continuación se presenta la implementación de las dos primeras dimensiones en su variante puramente secuencial


//técnica de recursión llamada búsqueda ternaria (ternary search) para encontrar el mínimo de la función convexa en el intervalo [min, max], Se asume que la función es convexa.

def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
  // Si la diferencia entre max y min es menor que la precisión, devolvemos el punto medio 
  if (max - min < prec) {
    (min + max) / 2.0
  } else {
    // Dividimos el intervalo en tres partes
    val m1 = min + (max - min) / 3.0
    val m2 = max - (max - min) / 3.0
    
    // Evaluamos y descartamos el tercio donde no está el mínimo
    if (f(m1) < f(m2)) {
      min_p(f, min, m2, prec)
    } else {
      min_p(f, m1, max, prec)
    }
  }
}

//La siguiente función Genera la medida de polarización COMETE parametrizada con alpha y beta, donde si las opiniones están muy separadas dará un valor alto, de lo contrario dará un valor bajo. Función de orden superior, devuelve una función que calcula la polarización de una distribución dada. La función interna rho_aux calcula el valor de la medida para un punto p específico, y luego se encuentra el mínimo de esta función en el intervalo [0, 1] utilizando la búsqueda ternaria.

def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
  (d: Distribution) => {
    //pi --> frecuencia de cada opinión, y --> valor de cada opinión
    val (pi, y) = d
    val k = pi.length
    
    // Función auxiliar que calcula el valor para un punto p. Se recorre todos los valores de la distribución
    def rho_aux(p: Double): Double = {
      (0 until k).map { i =>
        math.pow(pi(i), alpha) * math.pow(math.abs(y(i) - p), beta)
      }.sum
    }
    
    // Encontramos el mínimo de rho_aux en el intervalo 
    val p_min = min_p(rho_aux, 0.0, 1.0, 0.00001)
    rho_aux(p_min)
  }
}

//La siguiente función Calcula la medida normalizada dividiendo el resultado sobre el peor caso posible (donde la polarización es máxima, es decir, el 50% de la probabilidad está en 0.0 y el 50% restante en 1.0). Practicamente convierte la medida de polarización en una escala de 0 a 1, donde 0 representa ausencia de polarización y 1 representa el peor caso de polarización.

def normalizar(m: MedidaPol): MedidaPol = {
  (d: Distribution) => {
    val (pi, y) = d
    val k = pi.length
    
    // Construimos la distribución del peor caso de polarización 
    val pi_worst = Vector.tabulate(k)(i => if (i == 0 || i == k - 1) 0.5 else 0.0)
    val d_worst = (pi_worst, y)
    
    val maxPol = m(d_worst)
    
    // Dividimos por la polarización del peor caso. si md=0 no hay polarizacion.
    if (maxPol == 0.0) 0.0 else m(d) / maxPol
  }
}


}
