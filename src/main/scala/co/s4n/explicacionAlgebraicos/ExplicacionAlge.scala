package co.s4n.explicacionAlgebraicos

sealed trait Meses

case object Enero extends Meses
case object Febrero extends Meses
case object Marzo extends Meses
case object Abril extends Meses
case object Mayo extends Meses
case object Junio extends Meses
case object Julio extends Meses
case object Agosto extends Meses
case object Septiembre extends Meses
case object Octubre extends Meses
case object Noviembre extends Meses
case object Diciembre extends Meses


sealed trait RGB

case object Rojo extends RGB
case object Verde extends RGB
case object Azul extends RGB

case class Persona(name:String, age:Int) //permite generar todos los tipos de combinación de nombre y edad y son inmodificables

sealed trait Shape
case class Circle(radio:Float) extends Shape
case class Rectangle(width:Float, height:Float) extends Shape
case class EquilateralTriangle(arista:Float) extends Shape

//permite definir alternativas, las formas son círculos o son rectángulos
//No las dos a la vez

sealed trait  Expresion

case class Lit(value: Int) extends Expresion
case class Suma(expresionIzq:Expresion, expresionDer:Expresion) extends Expresion
case class Res(expresionIzq:Expresion, expresionDer:Expresion) extends Expresion
case class Div(expresionIzq:Expresion, expresionDer:Expresion) extends Expresion
case class Multi(expresionIzq:Expresion, expresionDer:Expresion) extends Expresion

  object Main {

  def area(shape:Shape):Float = shape match {
    case Circle(radio) => math.Pi.toFloat * radio * radio
    case Rectangle(w,h) => w*h
  }

  def area2(shape:Shape):Float = shape match {
    case Circle(radio) => math.Pi.toFloat * radio * radio
    case Rectangle(w,h) => w*h
    case EquilateralTriangle(a) => ((math.pow(a,2) * math.sqrt(3))/4).toFloat
  }

    Multi(Suma(Lit(3),Lit(4)), Lit(5))

    def eval(exp:Expresion):Int = exp match {
      case Lit(l) => l
      case Suma(l,r) => eval(l) + eval(r)
      case Res(l,r) => eval(l) - eval(r)
      case Multi(l,r) => eval(l) * eval(r)
    }

    // defina la función size que cuenta los números de operadores de una expresion

    def size(expresion: Expresion):Int = expresion match {
      case Lit(_) => 0
      case Suma(l,r) => 1 + size(l) + size(r)
      case Res(l,r) => 1 + size(l) + size(r)
      case Multi(l,r) => 1 + size(l) + size(r)
      case Div(l,r) => 1 + size(l) + size(r)
    }
}

