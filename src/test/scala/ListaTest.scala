import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, throws, AnyOperators}
import org.scalacheck.Gen._

object ListaTest extends Properties("ListaTest"){

  // Metodo de generacion de listas de valores enteros
  val secuenciaEnteros = listOf(choose(0,7))

  // LAS SIGUIENTES PROPERTIES SON PARA COMPROBAR QUE LOS METODOS
  // CREADOS EN LA CLASE LISTA FUNCIONAN BIEN

  property("longitud de lista") =
    forAll(secuenciaEnteros) {
      xs => {
        // creamos lista a partir de xs
        val lista : Lista[Int] = Lista(xs : _*)
        val longitudList = xs.length
        val longitudLista = Lista.longitud(lista)
        // con ?= hacemos lo mismo que con == solo que muestra info de depuracion
        // en caso de que no se cumpla la igualdad
        longitudList ?= longitudLista
      }
    }

  property("suma de enteros") =
    forAll(secuenciaEnteros) {
      xs => {
        // objeto a partir de la clase Lista
        val lista : Lista[Int] = Lista(xs : _*)
        val sumaList = xs.map(x => x.toDouble).sum
        val sumaLista = Lista.sumaEnteros(lista)

        // se comprueba la igualdad
        sumaList ?= sumaLista
      }
    }

  property ("multiplicacion de Enteros") =
    forAll(secuenciaEnteros){
      xs => {
        val lista : Lista[Int] = Lista(xs : _*)
        val productoList = xs.map(x => x.toDouble).product
        val productoLista = Lista.productoEnteros(lista)

        //se comprueba la igualdad:
        productoList ?= productoLista
      }
    }

  property ("Concatenacion listas") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)
        // concatenacion de listas
        val concatList = xs:::xs
        val concatLista = Lista.concatenar(lista, lista)

        //se comprueba la igualdad:
        concatList ?= Lista.toList(concatLista)
      }
    }

  property ("suma rightFold") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)
        val sumaList = xs.map(x => x.toDouble).sum
        val sumaLista = Lista.sumaFoldRight(lista)

        //se comprueba la igualdad:
        sumaList ?= sumaLista
      }
    }

  property ("producto rightFold") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)
        val productoList = xs.map(x => x.toDouble).product
        val productoLista = Lista.productoFoldRight(lista)

        //se comprueba la igualdad:
        productoList ?= productoLista
      }
    }

  property ("Cambio de head") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)

        //Cambiamos la cabeza por 12:
        val cabezaNuevaList = xs match{
          case List()   => List(12)
          case a::c => 12::c
        }

        //ahora utilizamos la funcion de Lista:
        val listaNuevaCabeza = Lista.asignarCabeza(lista, 12)

        //se comprueba la igualdad:
        cabezaNuevaList ?= Lista.toList(listaNuevaCabeza)
      }
    }

  property ("Eliminar head") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)

        // eliminamos la cabeza
        val sinCabezaList = xs match{
          case List() => xs
          case a::c => xs.tail
        }

        //ahora utilizamos la funcion de Lista:
        val sinCabezaLista = Lista.tail(lista)

        //se comprueba la igualdad:
        sinCabezaList ?= Lista.toList(sinCabezaLista)
      }
    }

  property ("Eliminar n elementos") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)

        // eliminamos 4 elementos
        val nuevaListSinN = xs match{
          case List()   => xs
          case a::c => xs.drop(4)
        }

        //ahora utilizamos la funcion de Lista:
        val nuevaListaSinN = Lista.eliminar(lista, 4)

        //se comprueba la igualdad:
        nuevaListSinN ?= Lista.toList(nuevaListaSinN)
      }
    }

  property ("eliminar mientras haya numeros divisibles entre 3") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)

        // definimos el predicado
        def divisible3 (x:Int):Boolean = x%3 == 0

        // filtramos por los divisibles entre 3
        val divisiblesEntre3List = xs.dropWhile(divisible3)

        //ahora utilizamos la funcion de Lista:
        val divisiblesEntre3Lista = Lista.eliminarMientras(lista, divisible3)

        //se comprueba la igualdad:
        divisiblesEntre3List ?= Lista.toList(divisiblesEntre3Lista)
      }
    }

  property ("Eliminar ultimo elemento de la lista") =
    forAll(secuenciaEnteros){
      xs => {
        val lista:Lista[Int] = Lista(xs : _*)

        // eliminamos el ultimo elemento
        val sinUltimoList = xs match{
          case List()   => xs
          case a::c => xs.dropRight(1)
        }


        //ahora utilizamos la funcion de Lista:
        val sinUltimoLista = Lista.eliminarUltimo(lista)

        //se comprueba la igualdad:
        sinUltimoList ?= Lista.toList(sinUltimoLista)
      }
    }


}
