//Ejercicio 1
object comp {
  def cuadrado(x:Float):Float = {
      x * x
  }
  def cubo(value:Double): Double = {
    value * cuadrado(value.toFloat)
  }
}
//Ejercicio 2
object comp2 {
  def cuadrado(x:Long):Long = {
    x * x
  }
  def cubo(value:Long): Long = {
    value * cuadrado(value)
  }
}
//Ejercicio 3
object prueba {
  def x = {
    print("x")
    1
  }
  val y = {
    print("y")
    x + 2
  }
  def z = {
    print("z")
    x
    x + "c"
  }
}
//prueba.x + prueba.y + prueba.z -> res4: String = 41c

class Gato(
            val Nombre:String,
            val Color:String,
            val Comida:String){

}

/*scala> val gatoIO = new Gato("IO", "Fawn", "Churrus")
val gatoIO: Gato = Gato@5623354e

scala> val gatoMake = new Gato("Make", "Red", "Leche")
val gatoMake: Gato = Gato@2a673ee8

scala> val gatoDocker = new Gato("Docker", "Blue", "Cuido")
val gatoDocker: Gato = Gato@11275783
*/

object ventaDeChurrus {
  def despachar(gato:Gato):Boolean = {
    if(gato.Comida.equalsIgnoreCase("Churrus")) true
    else false
  }
}