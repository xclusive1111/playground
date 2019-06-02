import Chapter4.{MyMonad, MyReader, MyWriter}
import Chapter4.MyMonadInstance._
import Chapter4.MyMonadSyntax._     // for `flatMap` in order to use for comprehension
import Types.{Box, Db, FullBox}
import org.scalatest.FunSuite

import scala.util.Try

class Chapter4Tests extends FunSuite {

  test("monad instances") {
    val opt1 = MyMonad[Option].flatMap(Option(1))(n => Option(n + 1))
    assert(opt1 === Some(2))

    val opt2 = Option(1).flatMap(n => Option(n + 2))
    assert(opt2 === Some(3))

    val box1 = MyMonad[Box].flatMap(FullBox(1))(n => FullBox(n + 1))
    assert(box1 === FullBox(2))

    val box2: Box[Int] = Box(1).flatMap(n => FullBox(n + 2))
    assert(box2 === FullBox(3))

    val sumBox = sumSquare(Box(1), Box(2))
    assert(sumBox === Box(5))
  }

  test("foldRight stack safe using Eval monad") {
    import cats.Eval

    def foldRight[A, B](xs: List[A])(zero: B)(combineFn: (A, B) => B): B = {

      // Simplified version, stack-unsafe
      def go(as: List[A], acc: B)(f: (A, B) => B): B = as match {
        case head :: tail => f(head, go(tail, acc)(f))
        case Nil          => acc
      }

      // Stack safe
      def fold(as: List[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
        case head :: tail => Eval.defer(f(head, fold(tail, acc)(f)))
        case Nil          => acc
      }

      fold(xs, Eval.now(zero)){ (a, acc) => acc.map(b => combineFn(a, b)) }.value
    }

    val sum = foldRight(List(1, 2, 3, 4))(0)(_ + _)
    assert(sum == 10)
    val bigList = List.fill(500000)(1)
    val sum2 = foldRight(bigList)(0)(_ + _)
    assert(sum2 == 500000)
  }

  test("Writer monad") {
    import Chapter2.MyMonoidInstance._ // for monoid instances, such as Vector and String

    val w1 = MyWriter(Vector("1", "2", "3"), 5)
    val w2 = MyWriter(Vector("a", "b", "c"), 10)
    val w3 = for {
      a <- w1
      b <- w2
    } yield a + b

    val (log, value) = w3.run

    assert(log === Vector("1", "2", "3", "a", "b", "c"))
    assert(value === 15)


    def isBigGang(n: Int): MyWriter[String, Boolean] =
      if (n >= 10) MyWriter(s"$n is a big gang.", true)
      else MyWriter(s"$n is a smallish gang.", false)

    val w = for {
      _ <- isBigGang(10)
      _ <- isBigGang(4)
      _ <- isBigGang(15)
    } yield ()

    val (x, _) = w.run
    assert(x === "10 is a big gang.4 is a smallish gang.15 is a big gang.")
    val (y, _) = w.reset.run
    assert(y === "")
  }

  test("writer monad with factorial") {
    import Chapter2.MyMonoidInstance._ // for monoid instances, such as Vector and String

    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    // A helper function to simulate a computation and make sure it takes a while to run
    def slowly[A](value: => A) =
      try value finally Thread.sleep(100)

    /**
      * Use `println` to record result of each intermediate steps will cause the log interleaved
      * and make it difficult to distinguish which steps come from which computation.
      */
    def factorialPrint(n: Int): Int = {
      val rs = slowly {
        if(n == 0) 1
        else n * factorialPrint(n - 1)
      }
      println(s"fact $n $rs")
      rs
    }

    println("\n\nThe following logs will be interleaved:")
    val result = Await.result(Future.sequence(Vector(
      Future(factorialPrint(10)),
      Future(factorialPrint(10))
    )), 5.seconds)


    /**
      * Writers are useful for logging operations in multi-threaded environments.
      * The test below computes a factorial and log the result of each intermediate steps as it runs.
      */
    def factorial(n: Int): MyWriter[String, Int] =
      for {
        rs  <- if (n == 0) MyWriter("fact 0 1\n", 1)
               else slowly(factorial(n - 1).map(_ * n))
        _   <- MyWriter(s"fact $n $rs\n", ())
      } yield rs

    val results2 = Await.result(Future.sequence(Vector(
      Future(factorial(10)),
      Future(factorial(10))
    )), 5.seconds)

    println("\n\nThe following logs will be distinguishable:")
    results2
      .map(w => w.run._1)
      .foreach(println)
  }

  test("Reader monad") {
    type DbReader[A] = MyReader[Db, A]

    def findUsername(uid: Int): DbReader[Option[String]] =
      MyReader(db => db.usernames.get(uid))

    def checkPassword(uname: String, pwd: String): DbReader[Boolean] =
      MyReader(db => db.passwords.exists(_ == (uname, pwd)))

    def checkLogin(uid: Int, pwd: String): DbReader[Boolean] =
      for {
        unameOpt <- findUsername(uid)
        isValid  <- unameOpt
                      .map(uname => checkPassword(uname, pwd))
                      .getOrElse(false.pure[DbReader])
      } yield isValid

    val users = Map(1 -> "foo", 2 -> "bar", 3 -> "foobar")
    val passwords = Map("foo" -> "abcd", "bar" -> "1234", "foobar" -> "pa55w0rd")

    val db = Db(users, passwords)
    val isValid = checkLogin(1, "abcd").run(db)
    assert(isValid)
    val isInvalid = checkLogin(1, "1234").run(db)
    assert(isInvalid === false)
  }

  def parseInt(str: String): Try[Int] = Try(str.toInt)

  def sumSquare[F[_]: MyMonad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

}
