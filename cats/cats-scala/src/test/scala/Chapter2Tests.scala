import org.scalatest.FunSuite
import Chapter2._
import Types.{Cat, Order}

class Chapter2Tests extends FunSuite {
  import cats.instances.int._
  import cats.instances.boolean._
  import EqExercise._
  import MyMonoidInstance._

  test("associative law") {
    assert(associativeLaw(1, 2, 3))

    val c1 = Cat("Bear", 2, "brown")
    val c2 = Cat("Lue", 1, "black")
    val c3 = Cat("Scaf", 3, "pink")

    assert(associativeLaw(c1, c2, c3))
    assert(identityLaw(c1))

    assert(associativeLaw(true, false, true))
    assert(identityLaw(false))
  }

  test("Super Adder Exercise") {
    def add(ints: List[Int])(implicit m: MyMonoid[Int]): Int =
      ints.foldLeft(m.empty)(m.combine)

    def addOptionInt(list: List[Option[Int]])(implicit m: MyMonoid[Option[Int]]): Option[Int] =
      list.foldLeft(m.empty)(m.combine)

    def addOrders(orders: List[Order])(implicit m: MyMonoid[Order]): Order =
      orders.foldLeft(m.empty)(m.combine)

    def addAnything[A](xs: List[A])(implicit m: MyMonoid[A]): A =
      xs.foldLeft(m.empty)(m.combine)

    val ints    = List(1, 2, 3)
    val intOpts = List(Some(1), Some(2), None, Some(3))
    val orders  = List(Order(1, 2), Order(3, 4))
    val orderOpts  = List(Some(Order(1, 2)), None, Some(Order(3, 4)))

    assert(add(ints) == 6)
    assert(addOptionInt(intOpts) === Some(6))
    assert(addOrders(orders) === Order(4, 6))

    assert(addAnything(ints) === 6)
    assert(addAnything(intOpts) === Some(6))
    assert(addAnything(orders) === Order(4, 6))
    assert(addAnything(orderOpts) === Some(Order(4, 6)))
  }

}
