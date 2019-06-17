import Chapter4.MyMonad
import Chapter6.MySemigroupal
import org.scalatest.FunSuite
import Chapter6.SemigroupalInstances._
import Types.{MyInvalid, MyValid, MyValidated}
import Chapter6.ValidatedSyntax._

import scala.util.{Failure, Success, Try}

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

  test("Semigroupal syntax") {
    import Chapter2.MyMonoidInstance.stringMonoid
    import Chapter2.MyMonoidInstance.listMonoid

    val v1 = 1.valid[String]
    val v2 = 2.valid[String]
    val v3 = 3.valid[String]

    val rs1 = (v1, v2).tupled
    assert(rs1 === MyValid(1, 2))

    val rs2 = (v1, v2, v3).tupled
    assert(rs2 === MyValid(1, 2, 3))

    val rs3 = (
      List("Error 1").invalid[Int],
      List("Error 2").invalid[Int],
      List("Error 3").invalid[Int],
      List("Error 4").invalid[Int]
    ).tupled
    assert(rs3 === MyInvalid(List("Error 1", "Error 2", "Error 3", "Error 4")))

    val rs4 = (
      3.valid[List[String]],
      List("Error 1").invalid[Int],
      List("Error 2").invalid[Int],
      List("Error 3").invalid[Int]
    ).tupled
    assert(rs4 === MyInvalid(List("Error 1", "Error 2", "Error 3")))
  }

  test("Exercise: Form validation using Validated") {
    import Chapter2.MyMonoidInstance.listMonoid
    final case class User(name: String, age: Int, languages: List[String])
    type Form = Map[String, String]
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = MyValidated[List[String], A]

    def getValue(form: Form, param: String): FailFast[String] =
      form.get(param).toRight(List(s"$param field is not specified"))

    def parseInt(name: String)(str: String): FailFast[Int] =
      Try(str.toInt) match {
        case Success(n) => Right(n)
        case Failure(_) => Left(List(s"Cannot convert $name to an integer"))
      }

    def nonBlank(name: String)(str: String): FailFast[String] =
      if (!str.isEmpty && str.exists(_ != 32)) Right(str)
      else Left(List(s"$name must not be blanked"))

    def nonNegative(name: String)(n: Int): FailFast[Int] =
      if (n > 0) Right(n)
      else Left(List(s"$name must be a positive integer"))

    def readName(form: Form): FailFast[String] =
      getValue(form, "name").flatMap(nonBlank("name"))

    def readAge(form: Form): FailFast[Int] =
      getValue(form, "age")
        .flatMap(parseInt("age"))
        .flatMap(nonNegative("age"))

    def readLanguages(form: Form): FailFast[List[String]] =
      getValue(form, "languages")
        .flatMap(nonBlank("languages"))
        .map(_.split(",").toList)

    def getUser(form: Form): FailSlow[User] =
      (
        readName(form).toValidated,
        readAge(form).toValidated,
        readLanguages(form).toValidated
      ).mapN(User.apply)

    val form = Map[String, String]("name" -> "sondv", "age" -> "26", "languages" -> "en,vi" )
    val user = getUser(form)
    assert(user === MyValid(User("sondv", 26, List("en", "vi"))))

    val form2 = Map[String, String]("name_" -> "sondv", "age" -> "0", "languages" -> "en,vi" )
    val invalid = getUser(form2)
    assert(invalid === MyInvalid(List("name field is not specified", "age must be a positive integer")))
  }

}
