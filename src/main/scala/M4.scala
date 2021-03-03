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
class Conductor(val nombre:String, val apellido:String, val totalCarreras:Int, val carrerasTerminadas:Int){

}
class Escuderia(val nombre:String, val conductor: Conductor)

class Contador(val contador:Int){
  def incr(): Contador = {
    new Contador(contador+1)
  }
  def decr(): Contador = {
    new Contador(contador-1)
  }
}

class Counter(val contador:Int){
  def incr(increment:Int = 1):Counter ={
    new Counter(contador + increment)
  }
  def decr(decrement:Int = 1):Counter = {
    new Counter(contador - decrement)
  }
  def ajuste(sumador:Sumador, value:Int):Counter = {
    new Counter(contador + sumador.adicionar(value))
  }
}
class Sumador(monto:Int){
  def adicionar(valor:Int):Int = {
    valor + monto
  }
}
//Ejercicio 11
class Persona(val nombre:String, val apellido:String) {
  def nombreC = s"$nombre $apellido"
}

object Persona{
  def apply(nombre:String): Persona ={
    val partes = nombre.split(" ")
    new Persona(partes(0), partes(1))
  }
}

class Direcetor (val nombre:String, val apellido:String, val nacimiento:Int){
  def nombreC:String = s"$nombre $apellido"
  def copy(nombre:String = this.nombre, apellido:String = this.apellido,
           nacimiento:Int = this.nacimiento):Direcetor = {
    new Direcetor(nombre,apellido, nacimiento)
  }
}
object Direcetor {
  def apply(nombre:String, apellido:String, nacimiento:Int):Direcetor = {
    new Direcetor(nombre,apellido,nacimiento)
  }
  def esMayor(director1:Direcetor, direcetor2: Direcetor):Direcetor = {
    if (director1.nacimiento > direcetor2.nacimiento) director1
    else  direcetor2
  }
}

class Pelicula (val nombre:String, val presentacion: Int, val rangoIMDB: Double, val direcetor: Direcetor){
    def directorEdad:Int = presentacion - direcetor.nacimiento
    def esDirigidaPor(direcetor: Direcetor):Boolean = {
        this.direcetor == direcetor
  }
  def copy(nombre:String = this.nombre, presentacion:Int = this.presentacion,
           rangoIMDB: Double = this.rangoIMDB, direcetor: Direcetor = this.direcetor):Pelicula = {
    new Pelicula(nombre,presentacion,rangoIMDB,direcetor)
  }
}
object Pelicula {
  def apply(nombre: String, presentacion: Int, rangoIMDB: Double, direcetor: Direcetor): Pelicula = {
    new Pelicula(nombre, presentacion, rangoIMDB, direcetor)
  }
  def mejorCalificada(pelicula1:Pelicula, pelicula2: Pelicula):Pelicula = {
    if(pelicula1.rangoIMDB > pelicula2.rangoIMDB) pelicula1
    else pelicula2
  }
  def mayorDirectorEnElTiempo(pelicula1:Pelicula, pelicula2: Pelicula):Direcetor = {
   if(pelicula1.directorEdad > pelicula2.directorEdad) pelicula1.direcetor
    else pelicula2.direcetor
  }
}
