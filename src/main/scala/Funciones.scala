import scala.annotation.tailrec
/**
  * Objeto singleton para probar la funcionalidad del triangulo
  * de Pascal
  */
object Funciones {
  /**
    * Metodo main: en realidad no es necesario porque el desarrollo
    * deberia guiarse por los tests de prueba
    *
    * @param args
    */
  def main(args: Array[String]) {
    println("................... Triangulo de Pascal ...................")

    // Se muestran 10 filas del trinagulo de Pascal
    for (row <- 0 to 10) {
      // Se muestran 10 10 columnas
      for (col <- 0 to row)
        print(calcularValorTrianguloPascal(col, row) + " ")

      // Salto de linea final para mejorar la presentacion
      println()
    }

    // Se muestra el valor que debe ocupar la columna 5 en la fila 10
    println(calcularValorTrianguloPascal(5, 10))
    println(calcularValorTrianguloPascal(0, 0))

    // probamos la compensacion de parentesis
    val listaChar : List[Char] = List('(',')','a','(','b',')')
    println(chequearBalance(listaChar))

    // probamos la busqueda binaria
    val listaEnteros = Array(1,2,4,6,8,9,12,14,23,26,32,47,78,91)
    def criterioOrdenacion = (a: Int, b: Int) => {a < b}
    println(busquedaBinaria(listaEnteros, 3, criterioOrdenacion))
  }

  /**
    * Ejercicio 1: funcion para generar el triangulo de Pascal
    *
    * @param columna
    * @param fila
    * @return
    */
  def calcularValorTrianguloPascal(fila: Int, columna: Int): Int = {
    // en caso de ser una posicion de los extremos
    if (columna == fila || columna == 0) 1

    // en caso de ser una posicion interna
    else calcularValorTrianguloPascal(fila-1, columna) + calcularValorTrianguloPascal(fila-1, columna-1)
  }

  /**
    * Ejercicio 2: funcion para chequear el balance de parentesis
    *
    * @param cadena cadena a analizar
    * @return valor booleano con el resultado de la operacion
    */
  def chequearBalance(cadena: List[Char]): Boolean = {
     @annotation.tailrec
     def go(cad: List[Char], acumulador: Int) : Boolean = {
        if(cad.isEmpty && acumulador == 0) true
        else if(cad.isEmpty && acumulador !=0) false
        else if(acumulador < 0) false
        else if(cad.head == '(') go(cad.tail, acumulador+1)
        else if(cad.head == ')') go(cad.tail, acumulador-1)
        else go(cad.tail, acumulador)
     }
    go(cadena, 0)
  }

  /**
    * Ejercicio 3: funcion para determinar las posibles formas de devolver el
    * cambio de una determinada cantidad con un conjunto de monedas
    *
    * @param cantidad
    * @param monedas
    * @return contador de numero de vueltas posibles
    */
  def contarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int = {
    def cambios(cant: Int, mon: List[Int]): Int = {
      if (cant == 0) 1
      else if (mon.isEmpty) 0
      else if (cant < mon.head) 0
      else {
        var acum = 0
        for (i <- 0 to cant by mon.head){
          acum += cambios(cant-i, mon.tail)
        }
        acum
      }
    }

    val monedasOrdenadas = monedas.sorted
    if (cantidad > 0) cambios(cantidad, monedas)
    else 0

  }

  /**
   * Metodo generico para busqueda binaria
   * @param coleccion conjunto de datos sobre los que buscar
   * @param aBuscar elemento a buscar
   * @param criterio para comparar dos elementos de tipo A
   * @tparam A parametro de tipo
   * @return posicion del valor buscado o -1 en caso de no hallarlo
   */
  def busquedaBinaria[A](coleccion : Array[A], aBuscar: A,
                         criterio : (A,A) => Boolean) : Int = {

    @annotation.tailrec
    def go(col : Array[A], acum: Int): Int = {
      val posDelmedio = col.length / 2
      val valorDelMedio = col(posDelmedio)
      if (valorDelMedio == aBuscar) acum+posDelmedio
      else if (col.length == 1) -1
      else {
        if (criterio(valorDelMedio,aBuscar) == true){
          if (posDelmedio+1 == col.length) -1
          else go(col.slice(posDelmedio+1,col.length),acum+posDelmedio+1)
        }
        else{
          if (posDelmedio == 0) -1
          else go(col.slice(0,posDelmedio),acum)
        }
      }
    }

    if (coleccion.length == 0) -1
    else go(coleccion,0)
  }
}
















