package co.s4n.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MainSpec extends AnyFlatSpec with Matchers {
  "La suma de 1 2 y 3" should "es 6" in {
    val lst = List(1,2,3)

    List.suma(lst) shouldEqual 6
  }

  "La productoria de 7.0 8.0 9.0" should " es 504.0" in {
    val lst2 = List(7.0,8.0,9.0)

    List.prod(lst2) shouldEqual 504.0
  }

  "La longitud de una lista con 1,2,3,4" should  "4" in {
    val lst3 = List(1,2,3,4)

    List.length(lst3) shouldEqual 4
  }

  "La cola de (1,2,3,4,5)" should "es 2,3,4,5" in {
    val lst4 = List(1,2,3,4,5)

    List.tail(lst4) shouldBe List(2,3,4,5)
  }

  "La cabeza de (9,8,7,6)" should "es 9" in {
    val lst5 = List(9,8,7,6)

    List.head(lst5) shouldBe 9
  }

  "El and de (true,true,false)" should "es false" in {
    val lst = List(true,true,false)

    List.and(lst) shouldBe false
  }

  "El and de (true,true,true)" should "es true" in {
    val lst = List(true,true,true)

    List.and(lst) shouldBe true
  }

  "El or de (false,false,false)" should "es false" in {
    val lst = List(false,false,false)

    List.or(lst) shouldBe false
  }

  "El or de (false,false,true)" should "es true" in {
    val lst = List(false,false,true)

    List.or(lst) shouldBe true
  }

  "El or de (true,true,true)" should "es true" in {
    val lst = List(true,true,true)

    List.or(lst) shouldBe true
  }

  "El maximo de (5,3,6,9,0,1)" should "es 9" in {
    val lst = List(5,3,6,9,0,1)

    List.max(lst) shouldBe 9
  }

  "El mínimo de (5L,3L,6L,9L,0L,1L)" should "es 0L" in {
    val lst = List(5L,3L,6L,9L,0L,1L)

    List.min(lst) shouldBe 0L
  }

  "El mínimo y máximo de (5.0,3.5,6.0,9.9,0.1,1.3)" should "son 0.1 y 9.9" in {
    val lst = List(5.0,3.5,6.0,9.9,0.1,1.3)

    List.minMax(lst) shouldBe((0.1, 9.9))
  }

  "El split de (1,2,3,4,5,6,7) con 3" should "es (1,2,3)(4,5,6,7,)" in {
    val lst = List(1,2,3,4,5,6,7)
    val n = 3

    List.split(n,lst) shouldBe(List(1,2,3),List(4,5,6,7))
  }
  "El take de 3 de la lista (1,2,3,4,5,6,7)" should "(1,2,3)" in {
    val n = 3
    val lst = List(1,2,3,4,5,6,7)

    List.take(n, lst) shouldBe List(1,2,3)
  }

  "El init de (1,2,3,4,5,6)" should " es (1,2,3,4,5)" in {
    val lst = List(1,2,3,4,5,6)

    List.init(lst) shouldBe List(1,2,3,4,5)
  }

  "El reverso de List(1,2,3,4,5,6)" should "List(6,5,4,3,2,1)" in {
    val lst = List(1,2,3,4,5,6)

    List.reverse(lst) shouldBe List(6,5,4,3,2,1)
  }

  "El interspace de 1 con List(2,3,4,5,6)" should "List(2,1,3,1,4,1,5,1,6,1)" in {
    val lst = List(2,3,4,5,6)

    List.interspace(1, lst) shouldBe List(2,1,3,1,4,1,5,1,6,1)
  }
  "El zip de List(1,2,3),List(true,false,true,true)" should "es List((1,true),(2,false),(3,true))" in {
    List.zip(List(1,2,3),List(true,false,true,true)) shouldBe List((1,true),(2,false),(3,true))
  }

  "El unZip de List((1,\"a\"),(2,\"b\"),(3,\"b\")" should "es (List(1,2,3),List(\"a\",\"b\",\"c\"))" in {
    List.unZip(List((1,"a"),(2,"b"),(3,"c"))) shouldBe (List(1,2,3),List("a","b","c"))
  }

  "El concat de List(List(1,2,3),List(4,5,6))" should " es List(1,2,3,4,5,6)" in {
    List.concat(List(List(1,2,3),List(4,5,6))) shouldBe List(1,2,3,4,5,6)
  }

  "El and con foldRight de (true,true,false)" should "es false" in {
    val lst = List(true,true,false)

    List.andFR(lst) shouldBe false
  }

  "El sumarUno de List(1,2,3,4,5)" should "es List(2,3,4,5,6)" in {
    val lst = List(1,2,3,4,5)

    List.sumarUno(lst)
  }
  "La longitud de una lista con 1,2,3,4 usando foldLeft" should  "4" in {
    val lst3 = List(1,2,3,4)

    List.lengthFL(lst3) shouldEqual 4
  }
}
