package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {
  // JVM threads
  val runnable = new Runnable {
    override def run(): Unit = println("running in parallel")
  }
  val aThread = new Thread(runnable)

  //  aThread.start() // gives the signal to the JVM to start a JVM thread
  // create a JVM thread => OS thread
  //  runnable.run() // does`t do anything in parallel
  //  aThread.join() // blocks until aThread finishes running

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadBye = new Thread(() => (1 to 5).foreach(_ => println("bye")))

  //  threadHello.start()
  //  threadBye.start()

  // different runs produce different results

  // executors
  //  val pool = Executors.newFixedThreadPool(10)
  //  pool.execute(() => println("something in the thread pool"))
  //  pool.execute(() => {
  //    Thread.sleep(1_000)
  //    println("done after 1 second")
  //  })
  //  pool.execute(() => {
  //    Thread.sleep(1_000)
  //    println("almost done")
  //    Thread.sleep(1_000)
  //    println("done after 2 seconds")
  //  })

  //  pool.shutdown()
  //  println(pool.isShutdown)

  def runInParallel = {
    var x = 0
    val thread1 = new Thread(() => {
      x = 1
    })
    val thread2 = new Thread(() => {
      x = 2
    })
    thread1.start()
    thread2.start()
    println(x)
  }

  //  for (_ <- 1 to 10_000) runInParallel
  // problem 1 race conditions

  // option 2 use @volatile
  class BankAccount(@volatile var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
    //    println("I`ve bought " + thing)
    //    println("my account is now " + account)
  }
  //
  //  for (_ <- 1 to 1000) {
  //    val account = new BankAccount(50_000)
  //    val th1 = new Thread(() => buy(account, "shoes", 3_000))
  //    val th2 = new Thread(() => buy(account, "iphone12", 4_000))
  //    th1.start()
  //    th2.start()
  //    Thread.sleep(100)
  //    if (account.amount != 43_000) println("AHA: " + account.amount)
  //    //    println()
  //  }

  // scala solve the race condition for us
  // option 1 use synchronized()
  def buySafe(account: BankAccount, thing: String, price: Int) =
    account.synchronized {
      // no two threads can evaluate this at the same time
      account.amount -= price
      println("I`ve bought " + thing)
      println("my account is now " + account)
    }

  //  note: more used synchronized

  /*
  * Exercise
  *  1- Construct 50 inception thread
  *   th1 -> th2 -> th3 ....
  *   println("hello from th 3")
  *   in reverser order
  *
  * */
  def inceptionThread(maxThreads: Int, i: Int = 1): Thread =
    new Thread(() => {
      if (i < maxThreads) {
        val newTh = inceptionThread(maxThreads, i + 1)
        newTh.start()
        newTh.join()
      }
      println(s"Hello from th $i")
    })

  inceptionThread(50).start()

  /*
  * Exercise 2
  * */
  var x = 0
  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())
  /*
  * what is the biggest value possible for x? 100
  * what is the smallest value possible for x? 1
  *
  * th1: x = 0
  * th2: x = 0
  *   ...
  * th100: x = 0
  *
  * for all ths: x = 1 & write it back to x
  * */
  threads.foreach(_.join())
  println(x)

  /*
  * 3 sleep fallacy
  * */
  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1_000)
    message = "scala is awesome"
  })
  message = "scala s"
  awesomeThread.start()
  Thread.sleep(2_000)
  awesomeThread.join() // wait for the awesome thread to join
  println(message)
}
