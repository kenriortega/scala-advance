package lectures.par2fpa

object Monads extends App {

  // Monads are a kind of types which have some fundamental ops
  // unit(value: A): MonadTemplate[A] also called pure or apply
  // flatMap[B](f: A => MonadTemplate[B]): MonadTemplate[B] also called bind
  // some example of monads List, Options,Try, Future,Stream,Set
  // Operation must satisfy the monad law:
  // left-identity // unit(x).flatMap(f) == f(x)
  // right-identity // aMonadInstance.flatMap(unit) == aMonadInstance
  // associativity  // m.flatMap(f).flatMap(g) == m.flatMap(x=>f(x).flatMap(g))

  // our own Try monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /*
  * left-identity
  * unit.flatMap(f) =f(x)
  * Attempt(x).flatMap(f) = f(x) // Success case !
  * Success(x).flatMap(f) = f(x) // proved.
  *
  * right-identity
  * attempt.flatMap(unit) = attempt
  * Success(x).flatMap(x=>Attempt(x) = Attempt(x) = Success(x)
  * Fail(_).flatMap(...) = Fail(e)
  *
  * associativity
  *
  * attempt.flatMap(f).flatMap(g) == attempt.flatMap(x => f(x).flatMap(g))
  * Fail(e).flatMap(f).flatMap(g) = Fail(e)
  *
  * Success(v).flatMap(f).flatMap(g) =
  *   f(v).flatMap(g) OR Fail(e)
  *
  * Success(v).flatMap(x=> f(x).flatMap(g)) =
  *   f(v).flatMap(g) OR Fail(e)
  * */

  val attempt = Attempt {
    throw new RuntimeException("My own monad")
  }
  println(attempt)

  // Exercise implement a Lazy[T] monad = computation
  // which will only be executed when it`s needed
  // unit/apply
  // flatMap
  class Lazy[+A](value: => A) {
    //    call by need
    private lazy val internalValue = value

    def use: A = value

    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
  }

  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyEval = Lazy {
    println("Today i don`t feel like doing anything")
    42
  }
  //  println(lazyEval.use)
  val flatMapInstance = lazyEval.flatMap(x => Lazy {
    10 * x
  })
  val flatMapInstance2 = lazyEval.flatMap(x => Lazy {
    10 * x
  })
  flatMapInstance.use
  flatMapInstance2.use



  // Exercise 2
  // Monads = unit + flatMap
  // Monads = unit + map + flatten
  /*
  * Monad[T]{
      def flatMap[B](f: T => Monad[B]):Monad[B] = ...
      * def flatten(m: Monad[Monad[T]]):Monad[T] = m.flatMap((x: Monad[T] => x))
      * def map[B](f: T=>B) : Monad[B] = flatMap(unit(f(x)))
    }
  * */
}
