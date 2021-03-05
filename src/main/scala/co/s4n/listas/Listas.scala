package co.s4n.listas

import scala.::
import scala.annotation.tailrec

object Listas {

  def subs[A](list: List[A]): List[List[A]] = {
    @tailrec
    def subsAux[A](list: List[A], iterator: Int, aux: List[List[A]]): List[List[A]] = iterator match {
      case -1 => aux
      case n => subsAux(list, iterator - 1, list.combinations(iterator).toList ::: aux)
    }

    subsAux(list, list.length, Nil)
  }

  def permutations[A](list: List[A]): List[List[A]] = list match {
    case Nil => Nil
    case List(x) => List(List(x))
    case _ => list.flatMap(x => permutations(list.filterNot(_ == x)).map(p => x :: p))
  }

  def predAtPos[A](list: List[A], preds: List[(Int, A => Boolean)]): List[List[Boolean]] = {
    @tailrec
    def predAtPosAux[A](list: List[A], preds: List[(Int, A => Boolean)], aux: List[List[Boolean]]): List[List[Boolean]] = list match {
      case Nil => aux
      case list => predAtPosAux(list, preds.tail, List(preds.head._2(list(preds.head._1))).map(List(_)) ::: aux)
    }

    predAtPosAux(list, preds, Nil)

  }

  @tailrec
  def myLast[A](lst:List[A]):A = lst match{
    case head:: Nil => head
    case _ :: tail => myLast(tail)
  }

  @tailrec
  def myButLast[A](lst:List[A]):A = lst match {
    case x :: y :: Nil => x
    case head :: tail => myButLast(tail)

  }

  @tailrec
  def myButLasts[A](lst:List[A]):List[A] = lst match {
    case x :: y :: Nil => List(x, y)
    case head :: tail => myButLasts(tail)
  }

  @tailrec
  def elementAt[A](lst:List[A], pos:Int ):A = (pos, lst) match {
    case (1, head :: tail) => head
    case (n, head :: tail) => elementAt(tail, n -1)
  }

  def myLength[A](lst:List[A]):Int = lst match {
    case _::Nil => 1
    case head :: tail => 1 + myLength(tail)
  }
  def myLengthFL[A](lst:List[A]):Int = lst.foldRight(0)((_,y) => 1 + y)
  def myLengthFR[A](lst:List[A]):Int = lst.foldLeft(0)((x,_) =>1 +x)

  def myReverse[A](lst:List[A]):List[A] = {
    @tailrec
    def myReverseAux[A](lst:List[A], acum:List[A]):List[A] = lst match {
      case Nil => acum
      case head :: tail =>myReverseAux(tail, head::acum)
    }
    myReverseAux(lst,Nil)
  }

  def myHead[A](lst:List[A]):A = lst match {
    case head :: tail => head
  }

  def myInit[A](list:List[A]):List[A] =  {
    @tailrec
    def myInitP[A](list:List[A],aux:List[A]):List[A] = list match {
      case head :: Nil => aux
      case head :: tail => myInitP(tail,aux ::: List(head))
    }
    myInitP(list,Nil:List[A])
  }


  @tailrec
  def isPalindrome[A](lst:List[A]):Boolean = lst match {
    case head :: Nil => true
    case list => true
    case x::xs => x == Listas.myLast(xs) && isPalindrome(Listas.myInit(xs))

  }

  def compress[A](lst:List[A]):List[A] = lst match {
    case Nil => Nil
    case head :: List() => List(head)
    case head :: tail if(head == myHead(tail)) => compress(tail)
    case head :: tail => head :: compress(tail)
//    case head :: tail => if (head == myHead(tail)) compress(tail) else compress(tail.tail)
//    case head :: Nil => List(head)
  }

  def pack[A](lst:List[A]):List[List[A]] = {
    @tailrec
    def packAux[A](lst:List[A], aux1:List[List[A]], aux2:List[A]):List[List[A]] = (lst, aux2) match {
      case (Nil, Nil) => aux1
      case (Nil, tail) => aux1 :+ tail
      case (head :: tail, Nil) => packAux(tail, aux1, aux2:+head)
      case (head :: tail, tail2) => if (myHead(tail2) == head) packAux(tail, aux1, aux2:+head) else packAux(tail,aux1:+aux2, List(head))
    }
    packAux(lst, Nil, Nil)
  }

  //14
  def duplicate[A](lst:List[A]):List[A] = lst.foldLeft(Nil:List[A])((tail, head) => tail :+head:+head)
  //15
  def replicate[A](lst:List[A], n:Int):List[A] = {
    @tailrec
    def replicateAux[A](lst:List[A], num:Int, aux:List[A], count:Int):List[A] = (count, lst) match {
      case (_, Nil) => aux
      case (0, head :: tail) => replicateAux(tail, num, aux, num)
      case (n, head :: tail) => replicateAux(lst,num, aux:+head,count-1)
    }
    replicateAux(lst,n,Nil,n)
  }
  //16
  def dropEveryN[A](lst:List[A], index:Int):List[A] = {
    @tailrec
    def dropEveryNAux[A](lst:List[A], index:Int, aux:List[A], cont:Int):List[A] = (cont, lst) match {
      case (_, Nil) => aux
      case (1, head :: tail) => dropEveryNAux(tail, index, aux, index)
      case (n, head :: tail) => dropEveryNAux(tail, index, aux:+head, cont - 1)

    }
    dropEveryNAux(lst,index,Nil,index)
  }
  //17
  def split[A](lst:List[A], index:Int):(List[A], List[A]) = {
    @tailrec
    def splitAux(lst:List[A], index:Int, res1:List[A]):(List[A], List[A]) = (index, lst) match {
      case (_, Nil) => (Nil, Nil)
      case (0, head :: tail) => (res1, head :: tail)
      case (n,head :: tail) => splitAux(tail, index-1, res1:+head)

    }
    splitAux(lst, index, Nil)
  }
  //18
  def slice[A](lst:List[A], limiteInf:Int, limiteSup:Int):List[A] = {
    @tailrec
    def sliceAux[A](lst:List[A], limiteInf:Int, limiteSup:Int, aux:List[A], cont:Int):List[A] = (cont, lst) match {
      case (_, Nil) => aux
      case (n, head :: tail) if (n>=limiteInf && n<=limiteSup) => sliceAux(tail, limiteInf, limiteSup, aux:+head, cont + 1 )
      case (n, head :: tail ) => sliceAux(tail, limiteInf, limiteSup, aux, cont + 1 )
    }
    sliceAux(lst,limiteInf,limiteSup,Nil,1)
  }

  //19 TODO

  //20
  def removeAt[A](lst:List[A], index:Int):List[A]= (index, lst) match {
    case (_, Nil) => Nil
    case (1, head :: tail) => tail
    case (n, head :: tail) => head :: removeAt(tail, index - 1)
  }
  //21
  def insertAt[A](lst:List[A], index:Int, value:A):List[A] = (index, lst) match {
    case (_, Nil) => List(value)
    case (1, head :: tail) => List(value):::tail
    case (n, head:: tail) => head :: insertAt(lst, index - 1, value)
  }

  def range(limiteInf:Int, limiteSup:Int):List[Int] = {
    def rangeAux(limteInf:Int, limiteSup:Int, cont:Int):List[Int] = cont match {
      //TODO
    }
  }
}

//sealed trait NestedList[+A]
//
//case class Elem[A](elem:A) extends NestedList[A]
//case class Const[+A](lista:List[NestedList[A]]) extends NestedList[A]
//object Elem {
//  def flatten[A](lst:List[NestedList[A]]):List[A] = {
//    //def flattenAux[A] (lst)
//  }
//}
