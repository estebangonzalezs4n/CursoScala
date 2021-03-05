package co.s4n.listas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListasSpec extends AnyFlatSpec with Matchers {

  "El último elemento de List(1,2,3,4,5)" should "5" in {
    val l = List(1,2,3,4,5)
    Listas.myLast(List(1,2,3,4,5)) shouldBe 5
  }

  "El penultimo elemento de List(1,2,3,4,5)" should "4" in {
    val l = List(1,2,3,4,5)
    Listas.myButLast(List(1,2,3,4,5)) shouldBe 4
  }

  "El elemento 2 de la lista List(1,2,3,4,5)" should "2" in {
    val l = List(1,2,3,4,5)
    Listas.elementAt(l, 2) shouldBe 2
  }

  "La longitud de una lista List(1,2,3,4,5)" should "5" in {
    val l = List(1,2,3,4,5)
    Listas.myLength(l) shouldBe 5
  }

  "El reverso de una listas List(1,2,3,4,5)" should "List(5,4,3,2,1)" in{
    val l = List(1,2,3,4,5)
    val l2 = List(5,4,3,2,1)

    Listas.myReverse(l) shouldBe l2
  }

  "palindromo de List(1,2,4,8,16,8,4,2,1) " should " true" in {
    val l = List(1,2,4,8,16,8,4,2,1)
    Listas.isPalindrome(l) shouldBe true
  }

  "La compresión de List(1,1,1,2,2,3,4,5,5)" should "List(1,2,3,4,5)" in {
    val l = List(1,1,1,2,2,3,4,5,5)
    Listas.compress(l) shouldBe List(1,2,3,4,5)
  }

  "El replicate de List(1,2,3),2" should "List(1,1,2,2,3,3)" in{
    val l = List(1,2,3)
    val n = 3

    Listas.replicate(l, n) shouldBe List(1,1,1,2,2,2,3,3,3)
  }

  "El drop every 3 de List(1,2,3,4,5,6,7,8,9)" should "List(1,2,4,5,7,8)" in {
    val l = List(1,2,3,4,5,6,7,8,9)
    val n = 3

    Listas.dropEveryN(l,n) shouldBe List(1,2,4,5,7,8)
  }
  "El split en index 3 de  de List(1,2,3,4,5,6,7,8,9)" should "(List(1,2,3),List(4,5,6,7,8,9))" in {
    val l = List(1,2,3,4,5,6,7,8,9)
    val n = 3

    Listas.split(l,n) shouldBe (List(1,2,3),List(4,5,6,7,8,9))
  }

  "El slice de 3 y 6 de List(1,2,3,4,5,6,7,8,9)" should "List(3,4,5,6)" in {
    val l = List(1,2,3,4,5,6,7,8,9)
    val n = 3
    val n2 = 6

    Listas.slice(l, n, n2) shouldBe List(3,4,5,6)
  }

  "El removeAt en 3 de List(1,2,3,4,5,6,7,8,9)" should "List(1,2,4,5,6,7,8,9)" in {
    val l = List(1,2,3,4,5,6,7,8,9)
    val n = 4

    Listas.removeAt(l,n) shouldBe List(1,2,3,5,6,7,8,9)
  }

  "El insertAt en 2 con 9 en List(1,2,3,4,5)" should "List(1,9,2,3,4,5)" in {
    val l = List(1,2,3,4,5)
    val n = 2
    val value = 9

    Listas.insertAt(l,n,value) shouldBe List(1,9,2,3,4,5)
  }
}
