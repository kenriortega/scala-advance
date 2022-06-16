package lectures.par2fpa

object LazyEval extends App {

  // lazy delay the evaluation of values
  // keyword permit to evaluate the variable only when it is used
  //  lazy val x: Int = throw new RuntimeException
  //  println(x)
  lazy val x: Int = {
    println("Hello")
    43
  }
  println(x)
  println(x)

  // examples of implications
  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "y" else "n")
  // lazyCondition it`s not evaluated

  // in conjunction with call by Name
  def byNameMethod(n: => Int): Int = {
    // call by need technique
    lazy val t = n
    t + t + t + 1
  }

  def retrieveMagicValue = {
    // side effect or a long computation
    println("waiting")
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)

  //  println(lt30)
  //  println(gt20)

  val lt30Lazy = numbers.withFilter(lessThan30)
  val gt20Lazy = lt30.withFilter(greaterThan20)
  //  println
  //  println(gt20Lazy)
  for {
    a <- List(1, 2, 3) if a % 2 == 0 // use lazy vals
  } yield a + 1
  //
  List(1, 2, 3).withFilter(_ % 2 == 0).map(_ + 1)

}
