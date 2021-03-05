//Ejercicio 1
/*trait Felino {
  val sonido:String
  val color:String
}

class Leon(val sonido:String, val color:String, val tamMelena:Int) extends Felino {
}
class Tigre(val sonido:String, val color:String) extends Felino {
}

class Jaguar(val sonido:String, val color:String) extends Felino {
}

class Gato(val sonido:String, val color:String, val comida:String) extends Felino {
}
//Ejercicio 2
trait Forma{
   def getTamano:Double
   def getPerimetro:Double
   def getArea:Double
}

class Circulo(val radio:Double) extends Forma {
  override def getTamano: Double = {
    0
  }

  override def getPerimetro: Double = {
    2 * math.Pi * radio
  }

  override def getArea: Double = {
    math.Pi * math.pow(radio,2)
  }
}

class Rectangulo(val base:Double, val altura:Double) extends Forma {
  override def getTamano: Double = {
    4
  }

  override def getPerimetro: Double = {
    2*(base + altura)
  }

  override def getArea: Double = {
    base * altura
  }
}

class Cuadrado(val lado:Double) extends Forma {
  override def getTamano: Double = {
    4
  }

  override def getPerimetro: Double = {
    4*lado
  }

  override def getArea: Double = {
    lado*lado
  }
}

//ejercicio 3
trait Rectangular extends Forma {
  val longitud: Double
  val altura: Double
}

class Cuadrado2(val longitud: Double, val altura: Double) extends Rectangular{
  override def getTamano: Double = longitud

  override def getPerimetro: Double = 4 * longitud

  override def getArea: Double = longitud * longitud
}
class Rectangulo2(val longitud: Double, val altura: Double) extends Rectangular{

  override def getTamano: Double = longitud

  override def getPerimetro: Double = 2 * (longitud + altura)

  override def getArea: Double = longitud * altura
}
//Ejercicio 4

sealed trait Forma2 {

}

object Draw {
  def apply(forma2: Forma2): Draw = {

  }
}

//Ejercicio 5
class Color(R:Int, G:Int, B:Int){

}*/