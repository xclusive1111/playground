import main._
import org.scalatest.FunSuite

class TypeClassTests extends FunSuite {

  /**
    * To use @Json object, we import any type class instances that are required for the execution */
  test("testInterfaceObject") {
    import main.JsonWriterInstances._
    val json = Json.toJson(Person("john doe", "john.doe@example.com"))
    // Notice that we didn't provide the implicit parameter, the compiler spots that out
    // and tries to fix this by searching for type class instances of the relevant types
    // and inserting them at the call site:
    // Json.toJson(Person("john doe", "john.doe@example.com"))(personWriter)
    assert(json.isInstanceOf[JsonObject])
    val map = json match {
      case JsonObject(value) => value
      case _ => Map[String, Json]()
    }
    assert(map("name") === JsonString("john doe"))
    assert(map("email") === JsonString("john.doe@example.com"))
  }

  /**
    * By importing #toJson method from JsonWriterSyntax,
    * we'll be able to use this method on every object as though the method is defined in the object itself
    */
  test("interface syntax") {
    import main.JsonWriterInstances._
    import main.JsonWriterSyntax._
    Person("john doe", "john.doe@example.com").toJson
  }

  test("implicit method") {
    import main.JsonWriterInstances._
    val writer = implicitly[JsonWriter[Person]]
    writer.write(Person("john doe", "john.doe@example.com"))
  }

  test("test printable cat") {
    import main.PrintableInstances._
    import main.PrintableSyntax._
    val cat = Cat("Bear", 3, "black").format
    assert(cat === "Bear is a 3 year-old black cat")
  }
}
