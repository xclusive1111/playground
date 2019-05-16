object Types {
  sealed trait Json
  final case class JsonObject(get: Map[String, Json]) extends Json
  final case class JsonString(get: String) extends Json
  final case class JsonInt(get: Int) extends Json
  final case class JsonArray(get: Seq[Json]) extends Json
  case object JsonNull extends Json

  final case class Person(name: String, email: String)
  final case class Cat(name: String, age: Int, color: String)

}
