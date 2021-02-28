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

  // Ejercicio (M3) 9, función que recibe dos naturales y los suma
  def addNat(nat1:Nat, nat2:Nat):Nat = {
    val n1:Int = fromNattoInt(nat1)
    val n2:Int = fromNattoInt(nat2)

    fromIntToNat(n1+n2)
  }

  //Ejercicio (M3) 10, función que recibe dos naturales y realiza la multiplicación de estos
  def prodNat(nat1:Nat, nat2:Nat):Nat = {
    val n1:Int = fromNattoInt(nat1)
    val n2:Int = fromNattoInt(nat2)

    fromIntToNat(n1*n2)
  }


}