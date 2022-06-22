package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._
// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturePromise extends App {
  def calculateMeaningOfLife: Int = {
    Thread.sleep(2_000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife
  } // (global) which is passed by the compiler

  println(aFuture.value) // Option[Try[Int]]
  aFuture.onComplete {
    case Success(value) => println(s"the meaning of life is $value")
    case Failure(exception) => println(s"I have failed with $exception")
  } // Some thread
  Thread.sleep(3_000)


  // mini social network
  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile) =
      println(s"${this.name} poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    // "database"
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    // API
    def fetchProfile(id: String): Future[Profile] = Future {
      // fetching from the DB
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }

    // client: mark to poke bill

  }

  val mark: Future[Profile] = SocialNetwork.fetchProfile("fb.id.1-zuck")
  //  mark.onComplete {
  //    case Success(markProfile) =>
  //      val bill: Future[Profile] = SocialNetwork.fetchBestFriend(markProfile)
  //      bill.onComplete {
  //        case Success(billProfile) => markProfile.poke(billProfile)
  //        case Failure(e) => e.printStackTrace()
  //      }
  //    case Failure(e) => e.printStackTrace()
  //  }


  // functional composition of future
  // map , flatMap, filter

  val nameOnTheWall = mark.map(profile => profile.name)
  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zuckBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("z"))

  // for-comprehensions
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchProfile("fb.id.2-bill")
  } mark.poke(bill)
  Thread.sleep(1000)

  // fallbacks
  val aProfileUnknown = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }
  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }
  val fallBackResult = SocialNetwork.fetchProfile("unknown id")
    .fallbackTo(
      SocialNetwork.fetchProfile("fb.id.0-dummy")
    )

  // block a future (could be a promise)
  // online banking app
  case class User(name: String)

  case class Tx(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Tx] = Future {
      // simulate some processes
      Thread.sleep(1_000)
      Tx.apply(user.name, merchantName, amount, "Success")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch the user from DB
      // create a Tx
      // Wait for the Tx to finish
      val txStatusFuture = for {
        user <- fetchUser(username)
        tx <- createTransaction(user, merchantName, cost)
      } yield tx.status

      Await.result(txStatusFuture, 2.seconds) // implicit conversions -> pimp my library
    }
  }

  println(
    BankingApp.purchase(
      "daniel",
      "iphone12",
      "rock the jvm store",
      3000
    )
  )
  // promise
  val promise = Promise[Int]() // controller over a future
  val future = promise.future
  // thread 1 - "consumer"
  future.onComplete {
    case Success(value) => println("[c] i`ve received " + value)
    case Failure(exception) => exception.printStackTrace()
  }

  val producer = new Thread(() => {
    println("[p] creating numbers ...")
    Thread.sleep(1_000)
    // fulfilling the promise
    promise.success(42)
    println("[p] done")
  })
  producer.start()

  Thread.sleep(1_000)

  /*
  * 1- fulfill a future immediately with a value
  * 2- inSequence(fa,fb)
  * 3- first(fa,fb) => new future with the first value of the two futures
  * 4- last(fa,fb) => new future with the last value
  * 5- retryUntil(action: () => Future[T], condition: T => Boolean): Future[T]
  *
  * */

  // 1 fulfill immediately
  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  // 2 in sequence
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] =
    first.flatMap(_ => second)

  // 3 first out of two futures
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]

    def tryCompleteCustom(promise: Promise[A], result: Try[A]) = result match {
      case Success(r) => try {
        promise.success(r)
      } catch {
        case _ =>
      }
      case Failure(t) => try {
        promise.failure(t)
      } catch {
        case _ =>
      }
    }

    fa.onComplete(tryCompleteCustom(promise, _))
    fb.onComplete(promise.tryComplete)

    promise.future
  }

  // 4
  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    // 1 promise which both future will try to complete
    // 2 promise which the last future will complete
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]
    val checkAndComplete = (result: Try[A]) =>
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future
  }

  val fast = Future {
    Thread.sleep(100)
    42
  }
  val slow = Future {
    Thread.sleep(200)
    45
  }
  first(fast, slow).foreach(println)
  last(fast, slow).foreach(println)
  Thread.sleep(1000)

  // retry until
  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
      .filter(condition)
      .recoverWith {
        case _ => retryUntil(action, condition)
      }

  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("Generate " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x < 50).foreach(result => println("settled at " + result))
  Thread.sleep(10_000)
}
