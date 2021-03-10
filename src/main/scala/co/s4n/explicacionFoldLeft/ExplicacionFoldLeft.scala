package co.s4n.explicacionFoldLeft

object ExplicacionFoldLeft {

  val lista = List("a", "b", "c")
  val funcion = ((acumulador:String, elementoActual:String) => acumulador + elementoActual)
  val casoBase = ""

  def aplicarFold():String = {
    lista.foldLeft(casoBase)(funcion)
  }

}
