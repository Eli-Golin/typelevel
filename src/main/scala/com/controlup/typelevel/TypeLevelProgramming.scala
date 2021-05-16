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


  //let's define a trait which is called <. The < will take two arguments:
  trait < [A <: Nat, B <: Nat]
  //val comparisson : <[`2` , `4`] = ???
  // in scala we can use the infix type notation which is pretty much equivalent

  val comparisson: `2` < `4` = ???



  object < {
    //what this means is that for any natural number B a compiler can automatically build on demand
    //an instance of `0` < Successor[B] which we we know is always true since 0 is less than any other natural number!
    implicit def ltBasic[B <:Nat]: <[`0` , Successor[B]] = new <[`0`, Successor[B]] {}

    //let's define a method which we can use to fetch the implicit instance that the compiler automatically generates.
    //what this method actually means to compiler is: if for any A,B which extend Nat there's an
    // implicit type instance of <[A,B] in scope, please return it.
    def apply[A <:Nat, B <: Nat](implicit lt: A < B) = lt
  }


  //so let's try to define a comparison:
  val comp0: `0` < `1` = <.apply[`0`,`1`] // notice that the compiler can compile our code, since it generated a value
  //of type `0` < `1`  when calling a method ltBasic!

  //since we have a nice syntactic sugar for apply methods in Scala we can rewrite it as follows:
  val comp1: `0` < `1` = <[`0`,`1`] // this code compiles which means that the truth value of `0` < `1` is true!


  val comp2: `1` < `3` = <[`1`,`3`] // doesn't compile since compiler currently can not validate it
  // has only one implicit method which can validate that 0 is less than any natural,
  // but it is not enough to validate any two naturals.

  def main(args: Array[String]): Unit = {
    //no matter what argument we will pass to "show" it will print it's type and will ignore the actual value
    println(show(List(1,2,3))) // pay attention the the entire type signature is carried to the runtime (no raw types)
  }

}
