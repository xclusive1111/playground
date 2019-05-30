import Chapter4.MyMonad
import Chapter4.MyMonadInstance._
import Chapter4.MyMonadSyntax._
import Types.{Box, FullBox}
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

  def parseInt(str: String): Try[Int] = Try(str.toInt)

  def sumSquare[F[_]: MyMonad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

}
