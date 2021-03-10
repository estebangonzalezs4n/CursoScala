package co.s4n.explicacionFlatMapYOption

class FlatOption {


}

object FlatOption {
  def makeInt(s:String):Option[Int] = {
    try {
      Some(s.trim.toInt)
    } catch {
      case e:Exception => None
    }
  }
  val x = makeInt("1")
  val y = makeInt("2")
  val z = makeInt("3")
  //getOrElse permite dar un valor por omisión val sum = x.getOrElse(0) + y.getOrElse(0)
  //pero tiene el problema de que usa los valores por omisión cuando hay error
/*x map { a =>
           y map { b =>
               a + b
      }
      }
val res24: Option[Option[Int]] = Some(Some(3))
 ahora si funciona pero encapsula resultado en resultado

  x flatMap { a =>
     |      y map { b =>
     |          a + b
     | }
     | }

 intentar hacer suma de 3 valores usando flatMaps*/

  x flatMap{ a =>
     y flatMap { b =>
      z map { c =>
        a + b + c
      }
    }
  }

x flatMap { a =>
  y map { b =>
    a + b
  }
}
}
