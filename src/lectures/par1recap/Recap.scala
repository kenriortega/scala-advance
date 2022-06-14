package lectures.par1recap

import scala.annotation.tailrec

object Recap extends App {
  val aCondition: Boolean = false
  val aConditionVal = if (aCondition) 42 else 64
  // instruction vs expressions

  // compiler infers types for us
  val aCodeBlock = {
    if (aCondition) 54
    else 56
  }

  // Unit => void (side effect)
  val theUnit = println("Hello , scala")

  // function
  def aFunc(x: Int): Int = x + 1

  @tailrec def factorial(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else factorial(n - 1, n * acc)

  // OOP
  class Animal

  class Dog extends Animal

  val aDog: Animal = new Dog // subtyping polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch!!")
  }

  // anonymous class
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar")
  }

  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog // natural language similitude

  // generic
  abstract class MyList[+A] // variance and variance problems in this course

  // singleton
  object MyList

  // case class
  case class Person(name: String, age: Int)
  // exception and try/catch/finally

  val throwException = throw new RuntimeException
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "I caught an exception"
  } finally {
    println("some logs")
  }
  // packaging and imports

  // functional programing
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }
  val inc2 = (x: Int) => x + 1
  List(1, 2, 3, 4).map(inc2) // HOF
  // map, flatmap,filter

  // for - comprehensions
  val pairs = for {
    num <- List(2, 3, 5) // if condition it`s allowed
    char <- List('a', 'c', 'd')
  } yield num + "-" + char

  // Scala collections : Seq,Arrays,Lists,Vectors,Maps,Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "jess" -> 5555,
  )

  // collections: Options, Try
  val onOption = Some(2)

  // patter matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case _ => x + "th"
  }
  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(name, _) => s"him my name is $name"
  }

  // all pattern matching
}
