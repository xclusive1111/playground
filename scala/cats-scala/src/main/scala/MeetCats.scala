import java.util.Date

import Types._
import cats.{Eq, Show}


class MeetCats {

}

object MeetCats {
  import cats._
  import cats.implicits._
  /**
    * Define custom Show instances */
  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime}ms since the epoch")

  /**
    * Comparing custom types */
  implicit val dateEq: Eq[Date] =
    Eq.instance[Date] { (d1, d2) =>
      d1.getTime === d2.getTime
    }
}

/**
  * Re-implement the Cat application from the previous section using Show instead of Printable. */
object ShowInstances {
  import cats.instances.int._
  import cats.instances.string._

  implicit val showCat: Show[Cat] = Show.show[Cat] { cat =>
    val ps = implicitly[Show[String]]
    val pi = implicitly[Show[Int]]
    s"${ps.show(cat.name)} is a ${pi.show(cat.age)} year-old ${ps.show(cat.color)} cat"
  }
}

object ShowCat {
  def format(cat: Cat)(implicit s: Show[Cat]): String = s.show(cat)
}

object EqExercise {
  import cats.syntax.eq._
  import cats.instances.string._
  import cats.instances.int._
  import cats.instances.option._

  implicit val eqCat: Eq[Cat] = Eq.instance { (c1, c2) =>
    c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
  }

  def isTwoCatsEqual(c1: Cat, c2: Cat): Boolean = {
    c1 === c2
  }

  def isTwoOptionCatsEqual(c1: Option[Cat], c2: Option[Cat]): Boolean = {
    c1 === c2
  }
}