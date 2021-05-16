package com.controlup.typelevel

object TypeLevelProgramming {
  import scala.reflect.runtime.universe._

  //This method is used only to pretty print the type for our demonstration sicne the types we gonna be using will be quite big.
  def show[T](v:T)(implicit t: TypeTag[T]) = t.toString().replace("com.controlup.typeleve.TypeLevelProgramming.","")

  def main(args: Array[String]): Unit = {
    //no matter what argument we will pass to "show" it will print it's type and will ignore the actual value
    println(show(List(1,2,3))) // pay attention the the entire type signature is carried to the runtime (no raw types)
  }

}
