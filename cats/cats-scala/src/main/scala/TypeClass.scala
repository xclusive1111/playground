import Types._

/**
  * A `type class` is an interface or API that represents some functionality we want to implement
  * JsonWriter is a type class in this example
  */
trait JsonWriter[A] {
  def write(value: A): Json
}


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


trait Printable[A] { self =>
  def format(value: A): String

  /**
    * Exercise: Showing off with contramap
    * Contramap prepend an operation to a chain
    * */
  def contramap[B](f: B => A): Printable[B] = new Printable[B] {
    override def format(value: B): String = self.format(f(value))
  }
}

object PrintableInstances {
  implicit val printInt: Printable[Int] = n => n.toString
  implicit val printString: Printable[String] = value => value
  implicit val printBool: Printable[Boolean] = value => if (value) "yes" else "no"
  implicit val printCat: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat): String = {
      val ps = implicitly[Printable[String]]
      val pi = implicitly[Printable[Int]]
      s"${ps.format(value.name)} is a ${pi.format(value.age)} year-old ${ps.format(value.color)} cat"
    }
  }

  implicit def printBox[A](implicit p: Printable[A]): Printable[Box[A]] = new Printable[Box[A]] {
    override def format(box: Box[A]): String = box match {
      case FullBox(value) => p.format(value)
      case EmptyBox => ""
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

/**
  * Invariant functor and the `imap` method
  * Invariant functor represents bidirectional transformations
  * */
trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A

  /**
    * `imap` is informally equivalent to a combination of `map` and `contramap` */
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {

    // contramap: prepend an operation to a chain
    override def encode(value: B): String = self.encode(enc(value))

    // map: append an operation to a chain
    override def decode(value: String): B = dec(self.decode(value))
  }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)
}

object CodecInstances {
  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value

    override def decode(value: String): String = value
  }

  // We can construct many useful codecs for other types by building off
  // of `stringCodec` using `imap`
  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val boolCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    stringCodec.imap(str => FullBox(c.decode(str)), {
      case FullBox(value) => c.encode(value)
      case EmptyBox => ""
    })

  implicit def optionCodec[A](implicit c: Codec[A]): Codec[Option[A]] =
      stringCodec.imap(str => Option(c.decode(str)), {
        case Some(value) => c.encode(value)
        case None => ""
      })
}