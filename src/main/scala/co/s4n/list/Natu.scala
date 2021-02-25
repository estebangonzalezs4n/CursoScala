package co.s4n.list

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat {
  def fromNattoInt(n:Nat):Int = n match {
    case Cero => 0
    case Suc(n) => 1 + fromNattoInt(n)
  }
  def fromIntToNat(n:Int):Nat = n match {
    case 0 => Cero
    case n => fromIntToNat(Suc(n-1))
  }
}