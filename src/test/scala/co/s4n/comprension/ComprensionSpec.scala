package co.s4n.comprension

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ComprensionSpec extends AnyFlatSpec with Matchers {
  "El reverse de List(1,2,3,4)" should "List(4,3,2,1)" in {
    val l = List(1,2,3,4)
    Comprension.reverse(l) shouldBe List(4,3,2,1)
  }
}
