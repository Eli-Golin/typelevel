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


  trait < [A <: Nat, B <: Nat]
  object < {
    implicit def ltBasic[B <:Nat]: <[`0` , Successor[B]] = new <[`0`, Successor[B]] {}
    def apply[A <:Nat, B <: Nat](implicit lt: A < B) = lt
  }

  trait <= [A <: Nat, B <: Nat]
  object <= {

    //That is true that for any natural number 0 is less or equal to it
    implicit def lteBasic[B <:Nat]: <=[`0` , B] = new <=[`0`,B] {}

    //The enductive method stays the same.
    implicit def inductive[A <: Nat, B <: Nat](implicit lte: A <= B): Successor[A] <= Successor[B] = new <=[Successor[A],Successor[B]] {}


    def apply[A <:Nat, B <: Nat](implicit lte: A <= B) = lte
  }

  /*
  Let's rewind the compiler's steps once more:
  1. we are declaring a variable with explicit type `1` <= `1`
  2. we are calling <=.apply[<=[`1`,`1`]].
  3. since the apply method doesn't get an explicit parameter when called, the compiler tries to search for an implicit parameter in scope.
     It locates an implicit method named inductive that is able to return <=[Successor[`0`],Successor[`0`]], so it calls
     this method without any explicit arguments.
  4. The inductive:Successor[`0`] <= Successor[`0`] will have to find an implicit parameter of type `0` <: `0` in order to be applied,
     so the compiler locates the lteBasic and calls it with `0` instead of the B parameter.
  5. lteBasic[`0`]:<=[`0`,`0`] is able to return the type <=[`0`,`0`] since `0` extends Nat (and thus fulfills the type constraint)
   */
  val validTest: `1` <= `1` = <=[`1`,`1`]
  //val invalitTest: `5` <= `2` = <=[`5`,`2`]// that on the other hand does not compile and thus proves the expression is wrong


  //Now we would like to 'add numbers' as types.
  //The idea is if the compiler can create automatically instances of type + , that should prove that A + B = S truth statement
  // , is true.
  trait +[A <: Nat, B<:Nat, S<:Nat]
  object + {
    //We know that  0 + 0 equals 0. ==> that means the compiler should automatically have to be able to create a +[`0`,`0`,'0`]
    implicit val zero: +[`0`,`0`,`0`] = new +[`0`,`0`,`0`] {}

    //for every A <: Nat && A > 0, we have A + 0 = A and 0 + A = A (commutativity) ==> we will implement those axioms as an implicit methods
    //if we have some A which is a natural number and the compiler can find an
    // implicit evidence that 0 < A, than it is safe to create +[`0`,A,A] instance
    implicit def basicRight[A <: Nat](implicit lt: `0` < A): +[`0`,A,A] = new +[`0`,A,A] {}

    //the other side is also true: A + 0 = A
    implicit def basciLeft[A <: Nat](implicit lt: `0` < A): +[A,`0`,A] = new +[A,`0`,A]{}

    def apply[A <: Nat, B <: Nat, S <: Nat](implicit plus: +[A,B,S]): +[A,B,S] = plus
  }

  //All we defined till far is enough to already prove the following:
  //the bellow two expressions have been validated by teh compiler and thus are proved to be correct.
  val zero: +[`0`,`0`,`0`] = +[`0`,`0`,`0`] //though intellij shows compilation issue - it lies.
  val two: +[`0`,`2`,`2`] = +[`0`,`2`,`2`]

  //val four: +[`1`,`3`,`4`] = +[`1`,`3`,`4`]

  def main(args: Array[String]): Unit = {
    //no matter what argument we will pass to "show" it will print it's type and will ignore the actual value
    println(show(validTest)) // pay attention the the entire type signature is carried to the runtime (no raw types)
  }

}
