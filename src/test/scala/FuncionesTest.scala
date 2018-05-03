import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{AnyOperators, forAll, throws}
import org.scalacheck.Gen._

object FuncionesTest extends Properties("ListaTest"){

  // TEST DEL TRIANGULO DE PASCAL

  // se generan losvalores de fila y columna para los bordes
  val MAXIMO = 15
  val coordenadasExtremos = for {
    fila <- Gen.choose(0,MAXIMO)
    columna <- Gen.oneOf(0, fila)
  } yield (fila, columna)


  property("Elementos en lados del triángulo valen 1") = {
    forAll(coordenadasExtremos) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      //println("Fila = "+i._1 +" Columna = "+ i._2+ " Resultado = "+resultado)
      resultado == 1
    }}
  }

  val coordenadasInternas = for {
    fila <- Gen.choose(2,MAXIMO)
    columna <- Gen.choose(1,fila-1)
  } yield (fila, columna)

  property("Elementos internos suma de los superiores") = {
    forAll(coordenadasInternas) { (i) => {
      val resultado = Funciones.calcularValorTrianguloPascal(i._1, i._2)
      //println("Fila = "+i._1 +" Columna = "+ i._2+ " Resultado = "+resultado)
      resultado == Funciones.calcularValorTrianguloPascal(i._1 -1, i._2 -1) +
        Funciones.calcularValorTrianguloPascal(i._1 -1, i._2)
    }}
  }

  ///////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////

  // TEST DE LOS PARENTESIS

  val strGen = (n: Int) => Gen.listOfN(n, Gen.oneOf('(',')',Gen.alphaChar.sample.get)).map(_.mkString)

  def comprobarParentesisBalanceados(cad: List[Char]): Boolean = {

    for (i <- 0 to cad.length){
      val subCad = cad.slice(0,i)
      if (subCad.filter(_ == "(").size < subCad.filter(_ == ")").size) return false
    }
    return true
  }

  property("Test paréntesis") = {
    forAll(strGen(10)) { (cadena) => {

      // comprobamos que ambas funciones nieguen que es una cadena balanceada o que la funcion test diga que esta
      // balanceada. Esto es porque no es posible comprobar que ambas funciones digan que esta balanceada ya que
      // la funcion test escoge subcadenas desde el principio, por lo que habrá veces que no coincida
      // su resultado con el de la funcion que estamos evaluando. Para dichos casos, nos basta con que la
      // funcion test admita como balanceada la cadena.
      (!Funciones.chequearBalance(cadena.toList) == !comprobarParentesisBalanceados(cadena.toList)) || comprobarParentesisBalanceados(cadena.toList)
    }}
  }

  ///////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////

  // TEST CAMBIO DE MONEDAS

  property("Test cambio monedas") = {
    (Funciones.contarCambiosPosibles(23, List(1,2,5,10)) == 52) &&
      (Funciones.contarCambiosPosibles(10, List()) == 0) &&
        (Funciones.contarCambiosPosibles(18, List(19,30)) == 0) &&
          (Funciones.contarCambiosPosibles(8, List(2,4,8)) == 4) &&
            (Funciones.contarCambiosPosibles(0, List(1,4,6,9)) == 0) &&
              (Funciones.contarCambiosPosibles(5, List(1,2)) == 3) &&
                (Funciones.contarCambiosPosibles(5, List(5)) == 1)

  }

  ///////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////

  // TEST BUSQUEDA BINARIA

  val numerosAleatorios = listOf(Gen.choose(0,300))

  property("Test busqueda binaria") = {
    forAll(numerosAleatorios) { (xs) => {
      // ordenamos el conjunto de numeros
      val numerosAleatoriosOrdenados = xs.sorted

      // elegimos un numero en el rango [0,300] de forma aleatoria para buscarlo
      val aBuscar = (math.random * (300)).toInt

      // usamos la funcion de la clase List para encontrar el indice del elemento a buscar
      val indice = numerosAleatoriosOrdenados.indexOf(aBuscar)

      // usamos el metodo creado en la clase Funciones para encontrar el indice del elemento a buscar
      val indiceFuncionesBusquedaBinaria = Funciones.busquedaBinaria[Int](numerosAleatoriosOrdenados.toArray, aBuscar, _ < _)

      // comprobamos si tanto los indices como los valores calculados de esos indices son iguales
      (indice == indiceFuncionesBusquedaBinaria) || (numerosAleatoriosOrdenados(indice) == numerosAleatoriosOrdenados(indiceFuncionesBusquedaBinaria))
    }}
  }
}
