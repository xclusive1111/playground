import Chapter4.MyMonad
import Chapter6.MySemigroupal
import org.scalatest.FunSuite
import Chapter6.SemigroupalInstances._
import Types.{MyInvalid, MyValid, MyValidated}
import Chapter6.ValidatedSyntax._

class Chapter6Tests extends FunSuite {
  test("Semigroupal instances") {
    val opt = MySemigroupal[Option].product(Some(1), Some(2))
    assert(opt === Some(1, 2))
    val opt2 = MySemigroupal[Option].product(Some(1), None)
    assert(opt2 === None)
  }

  test("Implement product in terms of flatMap") {
    import Chapter4.MyMonadSyntax._
    import Chapter4.MyMonadInstance._
    def product[M[_]: MyMonad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
      for {
        a <- ma
        b <- mb
      } yield (a, b)

    val opt = product(Option(1), Option(2))
    assert(opt === Some(1, 2))
  }

  test("Semigroupal Validation") {
    import Chapter2.MyMonoidInstance.listMonoid
    import Chapter2.MyMonoidInstance.stringMonoid
    type ListErrorsOr[A]  = MyValidated[List[String], A]
    type StringErrorOr[A] = MyValidated[String, A]

    val v1 = 1.valid[String]
    val v2 = 2.valid[String]
    val e1 = "Error 1".invalid[Int]
    val e2 = "Error 2".invalid[Int]
    val el1 = List("Error 1").invalid[Int]
    val el2 = List("Error 2").invalid[Int]
    val vl1 = 2.valid[List[String]]

    val rs1 = MySemigroupal[StringErrorOr].product(v1, v2)
    assert(rs1 === MyValid((1, 2)))

    val rs2 = MySemigroupal[StringErrorOr].product(e1, e2)
    assert(rs2 === MyInvalid("Error 1Error 2"))

    val rs3 = MySemigroupal[ListErrorsOr].product(el1, el2)
    assert(rs3 === MyInvalid(List("Error 1", "Error 2")))

    val rs4 = MySemigroupal[ListErrorsOr].product(el1, vl1)
    assert(rs4 === MyInvalid(List("Error 1")))
  }

}
