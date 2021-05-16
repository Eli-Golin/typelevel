package com.controlup.typelevel

object TypeLevelProgramming {
  import scala.reflect.runtime.universe._

  //This method is used only to pretty print the type for our demonstration sicne the types we gonna be using will be quite big.
  def show[T](v:T)(implicit t: TypeTag[T]) = t.toString().replace("com.controlup.typeleve.TypeLevelProgramming.","")

  trait Nat // a general representation for any natural number
  class `0` extends Nat // a type representation for a natural number - 0.

  class Successor[N <:  Nat] extends Nat //rest of the natural numbers will be defined with the type successor

  /* Let's define some type aliases*/
  type `1` = Successor[`0`]
  type `2` = Successor[`1`] // ==> this is basically the Successor[Successor[`0`]]
  type `3` = Successor[`2`]
  type `4` = Successor[`3`]
  type `5` = Successor[`4`]


  //type Animal  = Successor[Int] // won't compile since Int is not extending Nat (remember we are programming with types!!)


  def main(args: Array[String]): Unit = {
    //no matter what argument we will pass to "show" it will print it's type and will ignore the actual value
    println(show(List(1,2,3))) // pay attention the the entire type signature is carried to the runtime (no raw types)
  }

}
