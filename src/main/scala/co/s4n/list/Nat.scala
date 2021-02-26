package co.s4n.list

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat {

  //Ejercicio 10, toma un número natural y retorna su valor en Int
  def fromNattoInt(n:Nat):Int = n match {
    case Cero => 0
    case Suc(n) => 1 + fromNattoInt(n)
  }

  //Ejercicio 11, toma un número entero positivo y retorna su correspondiente natural
  def fromIntToNat(n:Int):Nat = n match {
    case 0 => Cero
    case n => Suc(fromIntToNat(n-1))
  }
}