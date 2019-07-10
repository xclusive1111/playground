import org.scalatest.FunSuite
import zio.{DefaultRuntime, IO, Schedule, Task, UIO, ZIO}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class CreatingEffect extends FunSuite {
  val runtime: DefaultRuntime = new DefaultRuntime {}

  test("Creating effects") {
    val int: UIO[Int]   = ZIO.succeed(93)
    val int2: Task[Int] = Task.succeed(10)
    val failed: IO[String, Nothing] = ZIO.fail("Opp! Something's wrong")
    val opt1: IO[Unit, Int] = ZIO.fromOption(Some(10))
    val opt2: IO[Unit, Nothing] = ZIO.fromOption(None)
    val try1: Task[Int] = ZIO.fromTry(Success(123))
    val try2: Task[Nothing] = ZIO.fromTry(Failure(new Exception("Opps!")))
    val try3: Task[Int] = ZIO.fromTry(Try(100 / 0))
    val f1: ZIO[Int, Nothing, Int] = ZIO.fromFunction((i: Int) => i * 2)
    val f2: ZIO[Int, Nothing, Int] = f1.map(_ * 2)
    lazy val future = Future.successful("Hello")
    val zfuture = ZIO.fromFuture { implicit ec => future.map(_ + " World!") }
  }

  test("For comprehension") {
    val int: UIO[Int] = ZIO.succeed(100)
    val int2: Task[Int] = Task.succeed(200)
    val failed: IO[Int, String] = ZIO.fail(123)

    val r: ZIO[Any, Any, String] =
      for {
        a <- int
        b <- int2
        c <- failed
      } yield a + b + c

    val x: ZIO[Any, Nothing, String] = r.fold(_ => "failed", n => s"Success with $n")
    val f1: ZIO[Int, Nothing, Int] = ZIO.fromFunction((n: Int) => n * 1)
    val f2: ZIO[Int, Nothing, Int] = ZIO.fromFunction((n: Int) => n * 2)
    val f3: ZIO[Int, Nothing, Int] = ZIO.fromFunction((n: Int) => n * 3)

    val f: ZIO[Int, Nothing, Int] = for {
      a <- f1
      b <- f2
      c <- f3
    } yield a + b + c

    val getInt: String => Int = str => Integer.parseInt(str)

    val h: IO[Throwable, Int] = ZIO.effect(getInt("5a"))
    val g: ZIO[Int, Throwable, Int] = ZIO.fromFunctionM((i: Int) => h.map(_ * i))

    val out: ZIO[Int, Nothing, Int] = g.catchAll { ex =>
      println(ex.getMessage)
      ZIO.succeed(10)
    }
    println(runtime.unsafeRun(out.provide(10)))
  }
}
