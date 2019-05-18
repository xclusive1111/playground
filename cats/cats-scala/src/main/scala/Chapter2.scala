import Types.Cat

object Chapter2 {
  import cats._
  import cats.implicits._

  trait MySemigroup[A] {
    def combine(a1: A, a2: A): A
  }

  trait MyMonoid[A] extends MySemigroup[A] {
    def empty: A
  }

  object MyMonoid {
    def apply[A](implicit m: MyMonoid[A]): MyMonoid[A] = m
    def empty[A](implicit m: MyMonoid[A]): A = m.empty
  }

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: MyMonoid[A], eq: Eq[A]): Boolean = {
    m.combine(x, m.combine(y, z)) === m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: MyMonoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  object MyMonoidInstance {
    implicit val intMonoid: MyMonoid[Int] = new MyMonoid[Int] {
      override def combine(a1: Int, a2: Int): Int = a1 + a2

      override def empty: Int = 0
    }

    implicit val catMonoid: MyMonoid[Cat] = new MyMonoid[Cat] {
      override def combine(a1: Cat, a2: Cat): Cat = Cat(a1.name + a2.name, a1.age + a2.age, a1.color + a2.color)

      override def empty: Cat = Cat("", 0, "")
    }

    implicit val boolMonoid: MyMonoid[Boolean] = new MyMonoid[Boolean] {
      override def empty: Boolean = true

      override def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    }

    implicit def setMonoid[A]: MyMonoid[Set[A]] = new MyMonoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
    }

    implicit def optionMonoid[A](implicit m: MyMonoid[A]): MyMonoid[Option[A]] = new MyMonoid[Option[A]] {
      override def empty: Option[A] = Option.empty[A]

      override def combine(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
        case (Some(x), Some(y))      => Some(m.combine(x, y))
        case (left @ Some(_), None)  => left
        case (None, right @ Some(_)) => right
        case _                       => None
      }
    }

    implicit val orderMonoid: MyMonoid[Types.Order] = new MyMonoid[Types.Order] {
      override def empty: Types.Order = Types.Order(0.0, 0.0)

      override def combine(a1: Types.Order, a2: Types.Order): Types.Order =
        Types.Order(a1.totalCost + a2.totalCost, a1.quantity + a2.quantity)
    }
  }

}
