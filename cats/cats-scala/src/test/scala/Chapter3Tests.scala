import Types._
import org.scalatest.FunSuite
import Chapter3.MyFunctorInstances._
import Chapter3.Example._
import Chapter3.MyFunctor
import Chapter3.MyFunctorSyntax._

class Chapter3Tests extends FunSuite {
  test("functor") {
    val b    = Box(123)
    val bOpt = Option(b)

    assert(b.fmap(toDouble) === Box(123.0))
    assert(bOpt.fmap(b => b.fmap(toDouble)) === Some(Box(123.0)))

    val x = MyFunctor[Option].fmap(Some(1))(toDoubleAndPlus2)
    assert(x === Some(2.0))

    def map[F[_], A, B](fa: F[A])(f: A => B)(implicit functor: MyFunctor[F]): F[B] =
      fa.fmap(f)

    assert(map(Option(1))(toDoubleAndPlus2) === Some(2.0))
    assert(map(List(1, 2, 3))(toDoubleAndPlus2) === List(2.0, 4.0, 6.0))
    assert(map(Seq(1, 2, 3))(toDoubleAndPlus2) === Seq(2.0, 4.0, 6.0))

  }

  test("mapping over a binary tree") {
    val t1: Tree[Int] = Branch(Leaf(1), Leaf(2))
    val o1 = t1.fmap(toDoubleAndPlus2)
    assert(o1 === Branch(Leaf(2.0), Leaf(4.0)))

    val t2: Tree[Int] = Branch(Leaf(1), Branch(t1, Leaf(3)))
    val o2 = t2.fmap(toDoubleAndPlus2)
    val expected = Branch(Leaf(2.0), Branch(Branch(Leaf(2.0), Leaf(4.0)), Leaf(6.0)))
    assert(o2 === expected)
  }

  test("showing off with contramap") {
    import PrintableInstances._
    import Printable._
    assert(format(1) === "1")
    assert(format("1") === "1")
    assert(format(true) === "yes")
    assert(format(Box(true)) === "yes")
  }

  test("Transformative thinking with imap") {
    import CodecInstances._
    import Codec._
    assert(encode(123.4) === "123.4")
    assert(decode[Double]("123.4") === 123.4)
    assert(encode(Box(123.4)) === "123.4")
    assert(decode[Box[Double]]("123.4") === Box(123.4))
  }
}
