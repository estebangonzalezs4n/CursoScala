package co.s4n.arboles

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree:Tree[A]):Int = tree match {
      case Leaf(v) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }

  def depht[A](tree:Tree[A]):Int =  tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + math.max(depht(l), depht(r))
  }

  def sum[A](tree:Tree[Int]):Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => sum(l) + sum(r)
  }
}