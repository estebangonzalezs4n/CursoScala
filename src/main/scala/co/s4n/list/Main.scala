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

  def reduce(lst: List[Int], z:Int)(f: (Int, Int) => Int):Int = lst match {
    case Nil => z
    case Const(h, t) => f(h, reduce(t,z)(f))
  }

  def suma(inst:List[Int]): Int = inst match {
    case Nil => 0
    case Const(h, t) => h + suma(t)
  }

  def prod(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Const(h,t) => h * prod(t)
  }

  //Función que retorna la longitud de una lista pasada como parametro
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
  //Ejercicio 8, función que recibe un arreglo de Doubles y retorna el min y max de todos los
  // valores en forma de tupla
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
  @tailrec
  def drop[A](n:Int, lst:List[A]): List[A] = (lst,n) match {
    case (Nil, n) => Nil
    case (Const(h, t), 0) => Const(h,t)
    case (Const(h, t),n) => drop(n-1, t)
  }

  /*@tailrec
  def dropWhile[A](lst:List[A], f:A=>Boolean): List[A] = lst match {
    case Nil                 => Nil
    case Const(h, t) if f(h) => dropWhile(t, f)
    case _                   => lst
    // dropWhile(lst1, (y:Int) => y < 3) se eliminan todos los elementos menores que 3
  }*/

  //Se currifica ya que permite un comportamiento más genérico al poder aplicar inferencia de tipo al segundo parámetro
  @tailrec
  def dropWhile[A](lst:List[A])(f:A=>Boolean): List[A] = lst match {
    case Nil                 => Nil
    case Const(h, t) if f(h) => dropWhile(t)(f)
    case _                   => lst
    // dropWhile(lst1, (y:Int) => y < 3) se eliminan todos los elementos menores que 3
  }

  //Ejercicio (M3) 1, función que toma un entero y una lista y con base a ese entero, retorna una lista de tamaño
  //del entero introducido
  def take[A](n:Int, lst:List[A]): List[A] = {
    @tailrec
    def takep[A](n:Int, lst:List[A], tmp:List[A]): List[A] = (n,lst) match {
      case (0,_) => tmp
      case (n, Nil) => Nil
      case (n, Const(h, t)) => if(n > List.length(lst)) lst
      else takep(n-1, t, addEnd(h,tmp) )
    }
    takep(n, lst, Nil)
  }

  //Ejercicio (M3) 2, función que recibe una lista y retorna la misma lista menos su último elemento
  def init[A](lst:List[A]): List[A] = {
    @tailrec
    def initp[A](lst:List[A], tmp:List[A], n:Int): List[A] = (n, lst) match {
      case (1,_) => tmp
      case (n, Const(h, t)) => initp(t, addEnd(h, tmp), n-1)
    }
    initp(lst, Nil, length(lst))
  }

  //Ejercicio (M3) 3Función que corta una lista en base a un entero, devuelve una tubla de listas con el resultado del corte
  def split[A](n:Int, lst:List[A]):(List[A], List[A]) = {
    @tailrec
    def splitp[A](n:Int, lst:List[A], tml:List[A]):(List[A], List[A]) = (n, lst) match {
      //Se pasan los valores de la lista original recibida a la lista tml
      case (0, _) => (tml, lst)
      case (n, Const(h, t)) => splitp(n-1, t , addEnd(h,tml))
    }
    splitp(n, lst, Nil)
  }

  //Ejercicio (M3) 4, función que toma dos listas de diferentes tipos y retorna una lista de tuplas
  //al juntar los valores de las listas ingresadas
    def zip[A,B](lst:List[A], lst2:List[B]):List[(A,B)] = {

        @tailrec
        def zipAux[A,B](lst:List[A], lst2:List[B], lmp:List[(A,B)]): List[(A,B)] = (lst, lst2) match {
          case (Nil, Nil) => lmp
          case (Const(h ,t) , Nil) => lmp
          case (Nil, Const(h, t)) => lmp
          case (Const(h1, t1), Const(h2, t2)) => zipAux(t1, t2, addEnd((h1, h2), lmp))
        }
    zipAux(lst, lst2, Nil)

  }

  //Ejercicio (M3) 5, función que toma una lista que consiste de tuplas de diferentes tipos de dato y retorna
  //dos listas, cada una compuesta por los elemenos del mismo tipo dato
  def unZip[A,B](lst:List[(A,B)]):(List[A], List[B]) = {
    @tailrec
    def unZipAux[A,B](lst:List[(A,B)], lmp1:List[A], lmp2:List[B]):(List[A], List[B]) = lst match {
      case Nil => (lmp1, lmp2)
      case Const(h , t) => unZipAux(t, addEnd(h._1, lmp1), addEnd(h._2, lmp2))
    }
    unZipAux(lst, Nil, Nil)
  }

  //Ejercicio (M3) 6, función que toma una lista y retorna una versión invertida de la misma
  def reverse[A](lst:List[A]):List[A] = {
    @tailrec
    def reversep[A](lst:List[A], tmp:List[A]):List[A] = lst match {
      case Nil => tmp
      case Const(h,t) => reversep(t, cons(h, tmp))
    }

    reversep(lst, Nil)
  }

  //Ejercicio (M3) 7, función que recibe un elemento y una lista y retorna una lista
  // con los elementos entremezclados
  def interspace[A](elem:A, lst:List[A]): List[A] = {
    @tailrec
    def interspace[A](elem:A, lst:List[A], tmp:List[A]):List[A] = lst match {
      case Nil => tmp
      case Const(h, t) => interspace(elem, t, addEnd(elem, addEnd(h, tmp)))
    }
    interspace(elem, lst, Nil)
  }

  //
  def concat[A](lst:List[List[A]]): List[A] = {
    def concatp[A](lst1:List[A], lst2:List[A]): List[A] = {
      append(lst1, lst2)
    }
    concatp(head(lst), head(tail(lst)))
  }

  /*def sumaR(lst:List[Int]):Int = reduce(lst, 0)((x,y) => x + y)

def prodR(lst:List[Int]): Int = reduce(lst, 1)((x,y) => x * y)*/

  def foldRight[A,B](as:List[A], z:B) (f: (A,B) => B):B = as match {
    case Nil         => z
    case Const(h,t ) => f(h, foldRight(t, z)(f))
  }

  @tailrec
  def foldLeft[A,B](lst:List[A], z:B)(f: (B,A) => B):B = lst match {
    //el truco es usar z como un acumulador
    case Const(h, t) => foldLeft(t, f(z,h))(f)
    case Nil         => z
  }

  def sumaF(lst:List[Int]):Int = foldRight(lst, 0)(_ + _) //optimizando
  def prodF(lst:List[Int]): Int = foldRight(lst, 1)(_ * _ )

  def sumaFL(lst:List[Int]): Int = foldLeft(lst, 0)(_ + _)
  def prodFL(lst:List[Int]): Int = foldLeft(lst, 1)(_ * _)

  def sumarUno(lst:List[Int]): List[Int] = foldRight(lst, Nil:List[Int])((elem, lst) => cons(elem +1, lst))

  //Ejercicio 14, función que retorna la longitud de una lista haciendo uso de foldRight
  def length2[A] (lst:List[A]):Int = foldRight(lst, 0)((x,y) => 1 + y)//usando el foldRight

  //Ejercicio 15, implementación de la función and con foldRight
  def andFR(lst:List[Boolean]): Boolean = foldRight(lst, true)(_ && _)

  //Ejercicio 16, función que recibe una lista y un predicado p, retorna el prefijo más largo posible que
  //satisface p
  def takeWhile[A] (lst:List[A])(p:A => Boolean): List[A] = {
        @tailrec
        def takeWhileP[A](lst:List[A], acum:List[A])(p:A => Boolean):List[A]= lst match {
          case Nil => acum
          case Const(h, t) if p(h) => takeWhileP(t, addEnd(h, acum))(p)
        }
    takeWhileP(lst,Nil)(p)
  }

  //Ejercicio 17
  def filter[A](lst:List[A])(p:A=>Boolean):List[A] = foldRight(lst, Nil:List[A])((h, t) => if (p(h)) Const(h, t) else t)

  //Ejercicio 18
  def unZipFR0[A,B] (lst:List[(A,B)]): (List[A], List[B]) = foldRight(lst,(Nil:List[A], Nil:List[B]))((h, t) => (Const(h._1,t._1), Const(h._2,t._2)))

  //Ejercicio 19
  def lengthFL[A](lst:List[A]):Int = foldLeft(lst, 0)((x, y) => 1 + x)

  //Ejercicio 20
  def andFL(lst:List[Boolean]): Boolean = foldLeft(lst, true)(_&&_)

  //Ejercicio 22
  //def filterFL[A](lst:List[A])(p:A=>Boolean):List[A] = foldLeft(lst,Nil:List[A])((lst, e) => if (p(e)) addEnd(lst, e) else lst)

  //Ejercicio 23
  //def unZipFL[A,B](lst:List[(A,B)]): (List[A], List[B]) = foldLeft(lst, (List[A], List[B]))((lst, elem) => (addEnd(lst, elem._1), addEnd(lst, elem._2)))


  def takeWhileL[A](lst:List[A])(p:A=> Boolean):List[A] = {
    def f(b:(Boolean, List[A]), a:A):(Boolean, List[A]) = b match {
      case (true, lst) => if (p(a)) (true, addEnd(a, lst))
                          else (false,lst)
      case (false, lst) => b
    }
    foldLeft(lst, (true, Nil:List[A]))(f)._2
  }

  //dropWhile con el right

  def map[A,B](lst:List[A])(f:A=>B):List[B] = foldRight(lst,Nil:List[B])((x, y) => Const(f(x), y)) //intentar hacer con foldLeft

  def lstInt2Str(lst:List[Int]):List[String] = lst match {
    case Nil => Nil
    case Const(h, t) => Const(h.toString, lstInt2Str(t))
  }

  def mapGen[A,B](lst:List[A])(f:A=>B): List[B] = lst match {
    case Nil => Nil
    case Const(h, t) => Const(f(h), mapGen(t)(f))
  }

  //aplicando ahora mapGen

  def sumarUnoMap(lst:List[Int]): List[Int] = mapGen(lst)(_+1)
  def int2StringMap(lst:List[Int]): List[String] = mapGen(lst)(_.toString) //_.toString -> se lo aplica al elemento, en este caso sería h

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