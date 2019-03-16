import com.me.examples.HigherKindType._
import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

class HigherKindTypeTests extends FunSuite {

  test("echo sync") {
    type Now[String] = String
    implicit object TerminalSync extends Terminal[Now] {
      override def read(): Now[String] = {
        print("Enter something:")
        scala.io.StdIn.readLine()
      }

      override def write(t: String): Now[Unit] = println(t)
    }
    implicit object ExecutionSync extends Execution[Now] {
      override def chain[A, B](c: Now[A])(f: A => Now[B]): Now[B] = f(c)

      override def create[B](b: B): Now[B] = b
    }
    ExecutionSync.chain("abcd")(_.toUpperCase)
  }

  val s: String = ""

  test("echo async") {
    implicit object TerminalAsync extends Terminal[Future] {
      override def read(): Future[String] = Future.successful("")

      override def write(t: String): Future[Unit] = ???
    }

    implicit object ExecutionSync extends Execution[Future] {
      override def chain[A, B](c: Future[A])(f: A => Future[B]): Future[B] = ???

      override def create[B](b: B): Future[B] = ???
    }
    echo2

  }

  test("future for comprehension") {

    def getA: Future[Int] = Future.successful(-10)

    def error(msg: String): Future[Nothing] =
      Future.failed(new RuntimeException(msg))

    val x = for {
      a <- getA
      b <- if (a <= 0) error(s"Must be positive. Got $a")
           else Future.successful(a)
    } yield b * 10

    println(Await.result(x, Duration.Inf))
  }

  test("forComp") {
    for {
      a <- Some(1)
      b <- Try(2)
    } yield a + b
  }

}
