package co.s4n.comprension

import scala.annotation.tailrec

object Comprension {

  def map[A, B](lst: List[A])(f: (A) => B): List[B] = for {
    x <- lst
  } yield (f(x))

  def filter[A](lst: List[A])(p: (A) => Boolean): List[A] = for {
    x <- lst
    if p(x)
  } yield x

  def divA(d: Int, n: Int): Boolean = n % d == 0

  def divisoresDe(n: Int): List[Int] = for {
    x <- List.range(1, n)
    if divA(x, n)
  } yield x

  def isPrime(n: Int): Boolean = divisoresDe(n) == List(1, n)

  //1,

//  def myLast[A](lst: List[A]): List[A] = for {
//    x <- (List.range(1, lst.length))
//    y = lst(x)
//    if (x == lst.length - 1)
//
//  } yield y
//
//  def myButLast[A](lst: List[A]): List[A] = for {
//    x <- (List.range(1, lst.length))
//    y = lst(x)
//    if (x == lst.length - 2)
//  } yield y
//
//  def Both[A](lst: List[A]): List[A] = for {
//    x <- lst.drop(lst.length-2)
//  } yield x
//
//  def elementAt[A](elem: Int, lst: List[A]): List[A] = for {
//    x <- (List.range(1,lst.length))
//    y = lst(x)
//    if (x == (elem-1))
//  } yield y

  def myLenght[A](lst:List[A]):Int = (for {
    _ <- lst
  }yield((a:Int) => a + 1)).foldLeft(0)((e,f) => f(e))

  def myLast[A](lst:List[A]):A = (for {
    xi <- lst
  }yield((a:A) => xi)).foldLeft(lst.head)((e,f) => f(e))

  def myHead[A](lst:List[A]):A = (for {
    xi <- lst
  }yield((a:A) => xi)).foldRight(lst.head)((f,e) => f(e))

//  def elementAt[A](lst:List[A], elem:Int):A = (for {
//    xi <- lst
//  }yield ((cont:Int, xi:A) => if (cont == elem) xi else cont +1)).foldLeft(0)((e,f) => f(e,x))

  def elementAt[A](k:Int, lst:List[A]):Option[A] = (
    for {
      xi <- lst
    } yield((t:(Int, Option[A])) => (t._2) match {
      case None => if (t._1 == k) (t._1 ,Some(xi)) else (t._1 + 1, None)
      case Some(x) => (t._1, Some(x))
    })).foldLeft((0, None:Option[A]))((e,f) => f(e))._2

  def copy[A](lst:List[A]):List[A] = (
    for {
      xi <- lst
    }yield((xs:List[A]) => xi :: xs)).foldRight(Nil:List[A])((f,e) => f(e))

  def reverse[A](lst:List[A]):List[A] = (
    for {
      xi <- lst
    }yield(())
  )
}
