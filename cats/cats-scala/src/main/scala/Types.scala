object Types {
  sealed trait Json
  final case class JsonObject(get: Map[String, Json]) extends Json
  final case class JsonString(get: String) extends Json
  final case class JsonInt(get: Int) extends Json
  final case class JsonArray(get: Seq[Json]) extends Json
  case object JsonNull extends Json

  final case class Person(name: String, email: String)
  final case class Cat(name: String, age: Int, color: String)
  final case class Order(totalCost: Double, quantity: Double)

  final case class Box[A](value: A)

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](Value: A) extends Tree[A]
}
