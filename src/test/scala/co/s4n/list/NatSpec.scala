package co.s4n.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class NatSpec extends AnyFlatSpec with Matchers {
  "Nat se puede contruir" should "como Cero" in {
    val cero = Cero

    cero shouldBe Cero
  }

  "El natural a entero de Suc(Suc(Cero))" should "2" in{
    val dos = Suc(Suc(Cero))

    Nat.fromNattoInt(dos) shouldBe 2
  }

  "El entero a natural de 2" should "2" in {
    val natDos = 2

    Nat.fromIntToNat(natDos) shouldBe Suc(Suc(Cero))

  }
}
