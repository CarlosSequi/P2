
// sealed = clase sellada
// trait = interfaz
/**
  * Interfaz generica para la lista
  * @tparam A
  */
sealed trait Lista[+A]

/**
  * Objeto para definir lista vacia
  */
case object Nil extends Lista[Nothing]

/**
  * Clase para definir la lista como compuesta por elemento inicial
  * (cabeza) y resto (cola)
  * @param cabeza
  * @param cola
  * @tparam A
  */
case class Constructor[+A](cabeza : A, cola : Lista[A]) extends Lista[A]

/**
  * Objeto para desarrollar las funciones pedidas
  */
object Lista {

   /**
     * Metodo para permitir crear listas sin usar new
     *
     * @param elementos secuencia de elementos a incluir en la lista
     * @tparam A
     * @return
     */
   def apply[A](elementos: A*): Lista[A] = {
      if (elementos.isEmpty) Nil
      else Constructor(elementos.head, apply(elementos.tail: _*))
   }

   /**
     * funcion para transformar un objeto Lista en un
     * objeto List para poder hacer los test
     * @param lista
     * @tparam A
     * @return
     */

   def toList[A](lista : Lista[A]):List[A] = {
      lista match {
         case Nil => List()
         case Constructor(cabeza, cola) => cabeza::toList(cola)
      }
   }

   /**
     * Obtiene la longitud de una lista
     *
     * @param lista
     * @tparam A
     * @return
     */
   def longitud[A](lista: Lista[A]): Int = {
      // dentro de este match utilizamos los casos que hemos definido arriba, Nil y Constructor
      // es decir, arriba hemos creado dos tipos de estructuras, en caso de que lista tenga la estructura
      // de Nil, devolvera longitud 0, en caso de que coincida con la estructura de Constructor, devolvera
      // su tamaño
      lista match {
         case Nil => 0
         case Constructor(cabeza, cola) => 1 + longitud(cola)
      }
   }

      /**
        * Metodo para sumar los valores de una lista de enteros
        *
        * @param enteros
        * @return
        */
      def sumaEnteros(enteros: Lista[Int]): Double = {
         enteros match {
            case Nil => 0.0
            case Constructor(cabeza, cola) => cabeza.toDouble + sumaEnteros(cola)
         }
      }

      /**
        * Metodo para multiplicar los valores de una lista de enteros
        *
        * @param enteros
        * @return
        */
      def productoEnteros(enteros: Lista[Int]): Double = enteros match {
         case Nil => 1.0
         case Constructor(cabeza, cola) => cabeza.toDouble * productoEnteros(cola)
      }

      /**
        * Metodo para agregar el contenido de dos listas
        *
        * @param lista1
        * @param lista2
        * @tparam A
        * @return
        */
      def concatenar[A](lista1: Lista[A], lista2: Lista[A]): Lista[A] = lista1 match {
         case Nil => lista2
         case Constructor(cabeza, cola) => Constructor(cabeza, concatenar(cola, lista2))
      }

      /**
        * Funcion de utilidad para aplicar una funcion de forma sucesiva a los
        * elementos de la lista
        *
        * @param lista
        * @param neutro
        * @param funcion
        * @tparam A
        * @tparam B
        * @return
        */
      def foldRight[A, B](lista: Lista[A], neutro: B)
                         (funcion: (A, B) => B): B = lista match {
         case Nil => neutro
         case Constructor(cabeza, cola) => funcion(cabeza, foldRight[A, B](cola, neutro)(funcion))
      }

      /**
        * Suma mediante foldRight
        *
        * @param listaEnteros
        * @return
        */
      def sumaFoldRight(listaEnteros: Lista[Int]): Double = {
         def suma(a: Int, b: Int): Int = a + b

         foldRight(listaEnteros, 0)(suma)
      }

      /**
        * Producto mediante foldRight
        *
        * @param listaEnteros
        * @return
        */
      def productoFoldRight(listaEnteros: Lista[Int]): Double = {
         // def producto(a: Int, b: Double): Double = a * b
         // foldRight(listaEnteros, 1.0)(producto)

         // para evitar tener que definir los tipos de datos como
         // hemos hecho en las dos lineas anteriores lo hacemos
         // mejor de la siguiente forma
         foldRight(listaEnteros, 1.0)((a,b) => a*b)
      }

      /**
        * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
        * se devuelve una lista con el nuevo elemento
        *
        * @param lista
        * @param cabezaNueva
        * @tparam A
        * @return
        */
      def asignarCabeza[A](lista: Lista[A], cabezaNueva: A): Lista[A] = lista match {
         case Nil => Lista(cabezaNueva)
         case Constructor(cabeza, cola) => Constructor(cabezaNueva, cola)
      }

      /**
        * Elimina el elemento cabeza de la lista
        *
        * @param lista
        * @tparam A
        * @return
        */
      def tail[A](lista: Lista[A]): Lista[A] = lista match {
         case Nil => lista
         case Constructor(cabeza, cola) => cola
      }

      /**
        * Elimina los n primeros elementos de una lista
        *
        * @param lista lista con la que trabajar
        * @param n     numero de elementos a eliminar
        * @tparam A tipo de datos
        * @return
        */
      def eliminar[A](lista: Lista[A], n: Int): Lista[A] = lista match {
         case Nil => lista
         case Constructor(cabeza, cola) => if (n == 0) lista
         else eliminar(cola, n - 1)
      }

      /**
        * Elimina elementos mientra se cumple la condicion pasada como
        * argumento
        *
        * @param lista    lista con la que trabajar
        * @param criterio predicado a considerar para continuar con el borrado
        * @tparam A tipo de datos a usar
        * @return
        */
      def eliminarMientras[A](lista: Lista[A], criterio: A => Boolean): Lista[A] = {
         lista match {
            case Nil => lista
            case Constructor(cabeza, cola) => if (criterio(cabeza)) eliminarMientras(cola, criterio)
            else lista
         }
      }

      /**
        * Elimina el ultimo elemento de la lista. Aqui no se pueden compartir
        * datos en los objetos y hay que generar una nueva lista copiando
        * datos
        *
        * @param lista lista con la que trabajar
        * @tparam A tipo de datos de la lista
        * @return
        */
      def eliminarUltimo[A](lista: Lista[A]): Lista[A] = lista match {
         case Nil => lista
         case Constructor(cabeza, cola) => if (longitud(lista) == 1) Nil
         else Constructor(cabeza, eliminarUltimo(cola))
      }

      /**
        * foldLeft con recursividad tipo tail
        *
        * @param lista   lista con la que trabajar
        * @param neutro  elemento neutro
        * @param funcion funcion a aplicar
        * @tparam A parametros de tipo de elementos de la lista
        * @tparam B parametro de tipo del elemento neutro
        * @return
        */
      @annotation.tailrec
      def foldLeft[A, B](lista: Lista[A], neutro: B)(funcion: (B, A) => B): B = lista match {
         case Nil => neutro
         case Constructor(cabeza, cola) => foldLeft(cola, funcion(neutro, cabeza))(funcion)


      }

}
