package lectures.par1recap

import scala.util.Try

object DarkSugars extends App {

  // syntax sugar #1: methods with single param
  def singleArgMethod(arg: Int): String = s"$arg little ducks"

  val description = singleArgMethod {
    // write some code
    34
  }
  println(description)

  val aTryInstance = Try { // java`s try {...}
    throw new RuntimeException
  }

  List(1, 2, 3, 4).map { x =>
    x + 1
  }

  // syntax sugar #2: single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }
  val aFunkyInstance: Action = (x: Int) => x + 1 // magic

  // example : Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("java style, scala")
  })

  val aSweeterThread = new Thread(() => println("sweet, scala"))

  abstract class AnAbstractType {
    def implemented: Int = 34

    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // syntax sugar #3: the :: and #:: methods are special
  val prependedList = 2 :: List(3, 4)
  // 2.::(List(3,4))
  // List(3,4).::(2)
  // ?!
  // scala spec: last char decides associativity of methods
  1 :: 2 :: 3 :: List(4, 5)
  List(4, 5).::(3).::(2).::(1)

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this // actual implementation here
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4: multi - word method naming
  class Resty(url: String) {
    def `fetch data`(endpoint: String) = println(s"$url$endpoint")

    def `prepare sql`(query: String) = println(s"$query")

    def √≈(data: Int) = println(s"$data")
  }

  val fetchUsers = new Resty("http://localhost:3000")
  fetchUsers `fetch data` "/users"
  fetchUsers `prepare sql` "insert into users value(name,age);"
  fetchUsers.√≈(data = 2)

  // syntax sugar #5: infix types
  class Composite[A, B]

  val composite: Int Composite String = ???

  class -->[A, B]

  val towards: Int --> String = ???

  // syntax sugar #6: update() is very special , much like apply()
  val array = Array(1, 2, 3)
  array(2) = 7 // rewritten to array.update(2,7)
  // used in mutable collection
  // remember apply() & update()

  // syntax sugar #7 setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 // private OO encapsulation

    def member = internalMember // getter

    def member_=(value: Int): Unit =
      internalMember = value // setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // rewritten as aMutableContainer.member_= 42


}
