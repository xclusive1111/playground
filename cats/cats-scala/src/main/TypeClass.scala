package main

sealed trait Json
final case class JsonObject(get: Map[String, Json]) extends Json
final case class JsonString(get: String) extends Json
final case class JsonInt(get: Int) extends Json
final case class JsonArray(get: Seq[Json]) extends Json
case object JsonNull extends Json

/**
  * A `type class` is an interface or API that represents some functionality we want to implement
  * JsonWriter is a type class in this example
  */
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

/**
  * `Type class instance` is an implementation of a type class.
  * We define instances by creating concrete implementations of the type class
  * and tagging them with the `implicit` keyword
  */
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String): Json = JsonString(value)
  }

  // We can convert the implementation into a single abstract method
  implicit val intWriter: JsonWriter[Int] = (value: Int) => JsonInt(value)

  implicit val personWriter: JsonWriter[Person] = (value: Person) => JsonObject(
    Map("name" -> JsonString(value.name),
        "email" -> JsonString(value.email)
    ))

  /**
    * Another way to create instances is by defining method to construct instances by another type class instances.
    * Be sure to mark the parameter to the method as implicit parameter,
    * otherwise the compiler won't be able to fill in the parameter during implicit resolution.
    * */
  implicit def optionWriter[A](implicit writer : JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
    override def write(opt: Option[A]): Json = opt match {
      case Some(value) => writer.write(value)
      case None        => JsonNull
    }
  }

  implicit def arrayWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Seq[A]] = new JsonWriter[Seq[A]] {
    override def write(xs: Seq[A]): Json = JsonArray(xs.map(value => writer.write(value)))
  }

}

/**
  * `Type class interface` is any functionality we expose to users.
  * Interface is an generic method that accept instances of type class as implicit parameters.
  * There are two common ways of specifying an interface: `Interface Objects` & `Interface Syntax`
  */

/**
  * `Interface Objects`
  * We can create an interface by placing methods in a singleton object
  * */
object Json {
  def toJson[A](value: A)(implicit writer: JsonWriter[A]): Json = writer.write(value)
}

object TestTypeClass {

}


/**
  * `Interface Syntax`
  * By using extension methods, we can extend the existing types.
  * This is referred to as `syntax` for the type class
  */
object JsonWriterSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit writer: JsonWriter[A]): Json = writer.write(value)
  }
}

/**
  * There're roughly 4 ways to package type class instances:
  * 1. by placing them in an object such as JsonWriterInstances
  * 2. by placing them in a trait
  * 3. by placing them in companion object of the type class
  * 4. by placing them in companion object of the parameter type
  *
  * With option 1, we bring implicit into scope by importing them.
  * With option 2, we bring implicit into scope with inheritance.
  * With option 3 and 4, instances are always in implicit scope, regardless of where we try to use them.
  */


/**
  * Exercise: Printable library */

final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val printInt: Printable[Int] = n => n.toString
  implicit val printString: Printable[String] = value => value
  implicit val printCat: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat): String = {
      val ps = implicitly[Printable[String]]
      val pi = implicitly[Printable[Int]]
      s"${ps.format(value.name)} is a ${pi.format(value.age)} year-old ${ps.format(value.color)} cat"
    }
  }
}

/**
  * Define implicit objects */
object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  def print[A](value: A): Unit = println(value)
}

/**
  * Define implicit syntax */
object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = Printable.print(value)
  }
}

