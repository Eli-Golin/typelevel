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


  //Let's modify the definition of a + type.
  //instead of the S type argument we gonna add an abstract type member - that would be our result symbolizing type
  trait +[A <: Nat, B<:Nat] {
    type Result <: Nat
  }


  object + {

    //Let's define a type alias as following:
    //So we can see that the type Plus (which is parametrized by A,B,S) is basically defined using the + trait.
    //The trick here to make the compiler automatically match Result type member with the S type argument.
    type Plus[A <: Nat, B <: Nat, S <: Nat] = +[A,B] {type Result = S}

    //We will redefine the zero type using the new Plus type.
    //So because "Plus" type is defined via "+" type, in order to instantiate it we would have to create members of the "+" type.
    implicit val zero: Plus[`0`,`0`,`0`] = new +[`0`,`0`] { type Result = `0`}

    //redefining the new basicRight using the new "Plus" type
    implicit def basicRight[A <: Nat](implicit lt: `0` < A): Plus[`0`,A,A] = new +[`0`,A] {type Result = A}

    //redefining the new basicLeft using the new "Plus" type
    implicit def basciLeft[A <: Nat](implicit lt: `0` < A): Plus[A,`0`,A] = new +[A,`0`]{type Result = A}


    // redefining the inductive step to use the new "Plus" type instead of "+"
    implicit def inductive [A <: Nat, B <: Nat, S <:Nat](implicit plus: Plus[A,B,S]): Plus[Successor[A],Successor[B],Successor[Successor[S]]] =
      new +[Successor[A],Successor[B]] {type Result = Successor[Successor[S]]}

    //we gonna require to have an implicit +[A,B] and we will return that instance
    def apply[A <: Nat, B <: Nat, S <: Nat](implicit plus: +[A,B]): +[A,B] = plus
  }

    //Let's explicitly call the apply method this time without specifying the types! (compiler should be able to infer those)
    //The compiler is been able to validate our result, but we can not really see the result type
    val zero: +[`0`,`0`] = +.apply
    val two: +[`0`,`2`] = +.apply
    val four: +[`1`,`3`] = +.apply


  def main(args: Array[String]): Unit = {
    //see that we don't see the result type.
    println(show(four))
  }

}
