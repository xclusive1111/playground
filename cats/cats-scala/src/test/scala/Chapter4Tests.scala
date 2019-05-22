import Chapter4.MyMonad
import Chapter4.MyMonadInstance._
import Chapter4.MyMonadSyntax._
import Types.{Box, EmptyBox, FullBox}
import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class Chapter4Tests extends FunSuite {

  test("monad instances") {
    val opt1 = MyMonad[Option].flatMap(Option(1))(n => Option(n + 1))
    assert(opt1 === Some(2))

    val opt2 = Option(1).bind(n => Option(n + 2))
    assert(opt2 === Some(3))

    val box1 = MyMonad[Box].flatMap(FullBox(1))(n => FullBox(n + 1))
    assert(box1 === FullBox(2))

    val box2: Box[Int] = Box(1).bind(n => FullBox(n + 2))
    assert(box2 === FullBox(3))

    val box3 = MyMonad[Box].pure("123A")
    val box4 = box3.bind(s => toInt[Box](s)) // Need to supply a type hint in order for Scala to infer a type :(
    assert(box4 === EmptyBox)
  }

  def toInt[F[_]: MyMonad](str: String): F[Int] =
    Try(str.toInt) match {
      case Success(n) => MyMonad[F].pure(n)
      case Failure(_) => MyMonad[F].empty
    }

}
