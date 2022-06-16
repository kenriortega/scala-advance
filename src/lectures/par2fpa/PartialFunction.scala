package lectures.par2fpa

object PartialFunction extends App {

  val aFunc = (x: Int) => x + 1 // Function1[Int,Int] === Int => Int

  val aFussyFunc = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 34
    else throw new FuncNotApplicableEx

  class FuncNotApplicableEx extends Exception

  val aNicerFussyFunc = (x: Int) => x match {
    case 1 => 34
    case 2 => 45
    case 3 => 12
    case _ => 0
  }
  // {1,2,5} => Int

  val aPartialFunc: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 3 => 999
    //    case _ => 0 // use a PF utilities when you need to a void error match
  } // partial function value

  println(aPartialFunc(2))
  //  println(aPartialFunc(33))

  // PF utilities
  println(aPartialFunc.isDefinedAt(67))
  // lift
  val lifted = aPartialFunc.lift
  println(lifted(2))
  println(lifted(65))
  val pfChain = aPartialFunc.orElse[Int, Int] { case 45 => 67 }
  println(pfChain(2))
  println(pfChain(45))

  // PF extend normal functions
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOF accept partial func as well
  val aMappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMappedList)

  /*
  * Note:
  * PF can only have one parameter type
  */

  /*Exercise
  * 1- construct a PF instance yourself (anonymous class)
  * 2- dumb chat bot as a PF
  * */

  val aManualFussyFunc = new PartialFunction[Int, Int] {
    override def apply(x: Int): Int = x match {
      case 1 => 42
      case 2 => 78
      case 5 => 1000
    }

    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 5
  }

  val chatBot: PartialFunction[String, String] = {
    case "hello" => "hi my name is HAL9000"
    case "goodbye" => "Once you start talking to me, there is no return , human!"
    case "call mom" => "unable to fin your phone without your credit card"
  }
  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)
}
