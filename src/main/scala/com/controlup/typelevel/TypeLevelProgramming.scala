package com.controlup.typelevel

object TypeLevelProgramming {
  import scala.reflect.runtime.universe._

  //This method is used only to pretty print the type for our demonstration sicne the types we gonna be using will be quite big.
  def show[T](v:T)(implicit t: TypeTag[T]) = t.toString().replace("com.controlup.typelevel.TypeLevelProgramming.","")

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

  //val comparisson: `2` < `4` = ???



  object < {
    //what this means is that for any natural number B a compiler can automatically build on demand
    //an instance of `0` < Successor[B] which we we know is always true since 0 is less than any other natural number!
    implicit def ltBasic[B <:Nat]: <[`0` , Successor[B]] = new <[`0`, Successor[B]] {}


    //let's define another implicit to help the compiler validate `1` < `3`. It will use a mathematical induction method.
    //This method basically means: that for any A,B that extend Nat if you can find an implicit evidence that an instance of
    // A < B exists, then generate a value of Successor[A] < Successor[B]
    implicit def inductive[A <: Nat, B <: Nat](implicit lt: A < B): Successor[A] < Successor[B] = new <[Successor[A],Successor[B]] {}



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


  // Now the compiler can validate our code.
  //What happens here is the following:
  /*
  1. We are calling the <.apply method.
  2. Since we do not pass any argument to that method explicitly, it searches for an implicit instance of `1` < `3` that it could use.
  3. Since the return type of inductive is Successor[A] < Successor[B] (and we know that `1` < `3`  == Successor[0] < Successor[2]) than apply
     implicitly calls to inductive --> inductive[`1` , `3`]
  4. In order for inductive to calculate the return type it needs to find an implicit value of `0` < `2` since if it had those,
     it would be able to calculate the Successor[0] < Successor[2] which is `1` < `3`
  5. The inductive calls implicitly to ltBasic ==> ltBasic[`1`] (since `2` == Successor[`1`])
  6. ltBasic produces implicit <[`0`,Successor[`1`]] == <[`0`,`2`]
   */
  val comp2: `1` < `3` = <[`1`,`3`]

  // this one doesn't compile which means the compiler has validated that it can not create
  // instances of this type and thus this expression is wrong
  val invalidComparison: `3` < `1` = <[`3`,`1`]
  
  def main(args: Array[String]): Unit = {
    //no matter what argument we will pass to "show" it will print it's type and will ignore the actual value
    println(show(comp2)) // pay attention the the entire type signature is carried to the runtime (no raw types)
  }

}
