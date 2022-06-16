package lectures.par2fpa

object CurriesPAF extends App {
  // curries functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int => Int => y => 3+y
  println(add3(5))
  println(superAdder(3)(5)) // curried function

  // scala allow
  // Method! are not instances
  def curriedAdder(x: Int)(y: Int): Int = x + y

  // adding a method into a variable
  val add4: Int => Int = curriedAdder(4)
  // lifting  = ETA - Expansions
  // functions != methods (JVM limitation)

  def inc(x: Int) = x + 1

  List(1, 2, 3).map(inc) // ETA - expansion begin scene

  // Partial function applications
  val add5 = curriedAdder(5) _ // tell to the compiler hey transform this in Int => Int

  // Exercise
  val simpleAddFunc = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y

  val add7 = (x: Int) => simpleAddFunc(7, x)
  val add7_2 = simpleAddFunc.curried(7)

  val add7_3 = curriedAddMethod(7) _ // PAF
  val add7_4 = curriedAddMethod(7)(_) // PAF = alternative syntax
  val add7_6 = simpleAddMethod(7, _: Int) // work all well
  val add7_5 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into a function value
  // y => simpleAddMethod(7,y)

  // underscore are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName = concatenator("Hello, I am ", _: String, " how are you?")
  println(insertName("kali"))
  val fillInTheBlanks = concatenator("hello, ", _: String, _: String)
  println(fillInTheBlanks("k", " scala is awesome"))

  // exercises
  /*
  * 1. Process a list of numbers and return their string representation with different format
  *   use the %4.2f, %8.6f and 14.12f with the curried formatter func
  * */
  println("%8.6f".format(Math.PI))

  def curriedFormatter(s: String)(number: Double): String = s.format(number)

  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)
  val simpleFormat = curriedFormatter("%4.2f") _ // lift
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _

  println(numbers.map(simpleFormat))
  println(numbers.map(seriousFormat))
  println(numbers.map(preciseFormat))

  /*
  *  difference between
  *  - func & methods
  *  - parameters:by-name vs 0-lambda
  * */
  def byName(n: => Int) = n + 1

  def byFunc(f: () => Int) = f() + 1

  def method: Int = 42

  def paramMethod(): Int = 42

  /* call by name & byFunc
  *   - int
  *   - method
  *   - parenMethod
  *   - lambda
  *   - PAF
  * */

  byName(23) // ok
  byName(method) //ok
  byName((() => 42) ()) // ok
  byName(paramMethod())
  //  byName(paramMethod)// no allowed we need to replace by byName(paramMethod())

  //  byFunc(43) not ok
  //  byFunc(method) not ok
  byFunc(paramMethod) // compile does ETA-expansion
  byFunc(() => 46)
  byFunc(paramMethod _)
}
