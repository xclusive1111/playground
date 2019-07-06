import Chapter4.MyMonadInstance._
import Chapter4.MyMonadSyntax._
import Chapter5.MyOptionT
import org.scalatest.FunSuite

class Chapter5Tests extends FunSuite {
  test("My monad transformer") {
    type ListOption[A] = MyOptionT[List, A]
    val rs1 = for {
      a <- 5.pure[ListOption]
      b <- 10.pure[ListOption]
    } yield a + b

    assert(rs1.value === List(Some(15)))

    val rs2 = rs1.map(n => n * 2)
    assert(rs2.value === List(Some(30)))

    val rs3 = rs2.fold(5)(_ + 2)
    assert(rs3 === List(32))

    val rs4 = MyOptionT[List, Int](List(None))
    assert(rs4.fold(5)(_ + 10) === List(5))

    val rs5 = rs2.flatMap(_ => MyOptionT[List, String](List(None)))
    assert(rs5.fold(10)(_.toInt) === List(10))
  }
}
