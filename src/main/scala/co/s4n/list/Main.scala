package co.s4n.list

import scala.annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A] // h head de la lista, t tail de la lista

object List {
  //acompaña al trait list, es como un factory
  //* es una varianza es una funcion que recibe 0 más argumentos
  //A* es una secuancia de A = Seq[A]
  //el _* convierte una secuencia en su valor de variable
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  def suma(inst:List[Int]): Int = inst match {
    case Nil => 0
    case Const(h, t) => h + suma(t)
  }

  def prod(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Const(h,t) => h * prod(t)
  }

  def length[A](lst:List[A]):Int = lst match {
      case Nil => 0
      case Const(h,t) => 1 + length(t)
  }
//Ejercicio 2, función que remueve el primer elemento de la lista que se le envía
  def tail[A](lst:List[A]): List[A] = lst match {
    case Nil          => Nil
    case Const(h, t)  => t
  }
//Ejercicio 3, función que devuelve el primer elemento de una lista que se le envía
  def head[A](lst:List[A]): A = lst match {
    //case Nil => Nil
    case Const(h,t) => h
  }

  //Ejercicio 4, función que recibe un arreglo de valores bool y retorna true si todos los valores son verdaderos, en otro caso false

  def and(lst:List[Boolean]): Boolean = {
    @tailrec
    def andp(lst:List[Boolean], acum:Boolean): Boolean = lst match {
      case Nil          => acum
      case Const(h, t)  => andp(t, h && acum)
    }
    andp(lst, acum = true)
  }

  //Ejercicio 5, función que recibe un arreglo de valores bool y retorna false si todos los valores son falsos, en otro caso true
  def or(lst:List[Boolean]): Boolean = {
    @tailrec
    def orp(lst:List[Boolean], acum:Boolean): Boolean = lst match {
      case Nil => acum
      case Const(h, t) => orp(t, h || acum)
    }
    orp(lst, acum = false)
  }

  //Ejercicio 6, función que recibe un arreglo de Ints y retorna el valor máximo de la arreglo
  def max(lst:List[Int]): Int = {
    @tailrec
    def maxp(lst:List[Int], acum:Int):Int = lst match {
      case Nil => acum
      case Const(h, t) => if(h > acum) maxp(t, h)
      else maxp(t, acum)
    }
    maxp(lst,0)
  }

  //Ejercicio 7, función que recibe un arreglo de Ints y retorna el valor mínimo del arreglo
  def min(lst:List[Long]): Long = {
    @tailrec
    def minp(lst:List[Long], acum:Long):Long = lst match {
      case Nil => acum
      case Const(h, t) => if(h < acum) minp(t, h)
      else minp(t, acum)
    }
    minp(lst,head(lst))
  }
  //Ejercicio 8, función que recibe un arreglo de Doubles y retorna el min y max de todos los valores en forma de tupla
  def minMax(lst:List[Double]):(Double, Double) = {

    @tailrec
    def min(lst:List[Double], acumMin:Double): Double = lst match {
      case Nil => acumMin
      case Const(h, t) => if(h < acumMin) min(t, h)
      else min(t, acumMin)
    }

    @tailrec
    def max(lst:List[Double], acumMax: Double): Double = lst match {
      case Nil => acumMax
      case Const(h, t) => if(h > acumMax) max(t, h)
      else max(t, acumMax)
    }
    val maxVal:Double = max(lst,0)
    val minVal:Double = min(lst, List.head(lst))
    (minVal, maxVal)
  }

  //Función para añadir un elemento nuevo al proncipio de la lista
  def cons[A](newHead:A, existingTail:List[A]): List[A] = {
    Const(newHead, existingTail)
  }

  //Función para añadir un elemento nuevo la final de una lista
  //Recorre toda la lista haciendo una copia de la lista y añadiendo finalmente el elemento
  def addEnd[A](elem:A, lst:List[A]):List[A] = lst match {
    case Nil        => Const(elem,Nil)
    case Const(h,t) => Const(h, addEnd(elem ,tail(lst)))
  }

  //Función que concatena 2 listas
  def append[A](lst1:List[A], lst2:List[A]):List[A] = (lst1, lst2) match {
    case (Nil, Nil) => Nil
    case (l1, Nil)  => l1
    case (Nil, l2)  => l2
    case (Const(h, t), lst2)   => Const(h, append(t,lst2))
  }

  //Función que elimina n elementos iniciales de una lista, en este caso se podría mejorar haciendo el pattern matching
  //con n en primer lugar
  def drop[A](n:Int, lst:List[A]): List[A] = (lst,n) match {
    case (Nil, n) => Nil
    case (Const(h, t), 0) => Const(h,t)
    case (Const(h, t),n) => drop(n-1, t)
  }

  /*def split[A](n:Int, lst:List[A], tmplst:List[A]):(List[A], List[A]) = {
    def splitAux[A](n:Int, lst:List[A], acum:List[A]):(List[A], List[A]) = n match {
      case 0 =>
    }

    splitAux(n,lst,Nil)
  }*/

  //Función que corta una lista en base a un entero, devuelve una tubla de listas con el resultado del corte
  def split[A](n:Int, lst:List[A]):(List[A], List[A]) = {
    @tailrec
    def splitp[A](n:Int, lst:List[A], tml:List[A]):(List[A], List[A]) = (n, lst) match {
        //Se pasan los valores de la lista original recibida a la lista tml
        case (0, _) => (tml, lst)
        case (n, Const(h, t)) => splitp(n-1, t , addEnd(h,tml))
    }
    splitp(n, lst, Nil)
  }


  //Respuesta ejercicio 1: x + y -> 9
  /*val x = List(4,5,6,7,8) match {
      case Const(x, Const(5, Const(7, _))) => x
      case Nil => 1
      case Const(x, Const(y, Const(6, Const(7, _)))) => x + y
      case Const(h,t) => h + suma(t)
      case _ => 777
      }
 */
}