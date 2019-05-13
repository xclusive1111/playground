import java.util.Date
import cats._
import cats.implicits._


class MeetCats {

}

object MeetCats {
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
  implicit val showCat: Show[Cat] = Show.show[Cat] { cat =>
    val ps = implicitly[Show[String]]
    val pi = implicitly[Show[Int]]
    s"${ps.show(cat.name)} is a ${pi.show(cat.age)} year-old ${ps.show(cat.color)} cat"
  }
}

object ShowCat {
  def format(cat: Cat)(implicit s: Show[Cat]): String = s.show(cat)
}
