import cats.Show
import org.scalatest.FunSuite

class MeetCatsTests extends FunSuite {

  test("importing default instances") {
    import cats.instances.int._
    import cats.instances.string._
    import cats.syntax.show.toShow
    val showInt:    Show[Int]    = Show.apply[Int]
    val showString: Show[String] = Show.apply[String]
    assert(showString.show("abcd") === "abcd")
    assert(showInt.show(123) === "123")
    assert(123.show === "123")
  }

  test("Cat Show") {
    import ShowInstances._
    val cat = Cat("Bear", 5, "brown")
    val str = ShowCat.format(cat)
    assert(str === "Bear is a 5 year-old brown cat")
  }

  test("Eq") {
    import cats.instances.int._
    import cats.syntax.eq._
    import cats.Eq
    val eqInt = Eq[Int]
    assert(1 =!= 2 )
    assert(eqInt.eqv(123, 123))
    // 1 =!= "1" => won't compile
  }

}
