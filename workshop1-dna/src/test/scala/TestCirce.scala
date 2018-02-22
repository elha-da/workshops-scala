
//import cats.syntax.either._
import io.circe.Decoder.Result
import io.circe.syntax._
import io.circe._
import io.circe.parser.{decode, parse}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.generic.JsonCodec
import io.circe.generic.auto._
import io.circe.optics.JsonPath._
import monocle.Optional


object TestCirce extends App {

  /*
  println(Json.fromFields(List(("key",Json.fromString("value")))).noSpaces)

  val listFromValues: String = Json.fromValues(List(Json.fromFields(List(("x", Json.fromInt(1)))))).noSpaces

  println(listFromValues)

  val jsonFieldsList = Json.fromFields(
                              List(
                                ("name",Json.fromString("sample json")),
                                ("data", Json.fromFields(List(("done",Json.fromBoolean(false)))))
                              )
                        ).noSpaces

  println(jsonFieldsList)

  val jsonArray: Json = Json.fromValues(List(
      Json.fromFields(List(("field1", Json.fromInt(5)))),
      Json.fromFields(List(
        ("field1", Json.fromInt(200)),
        ("field2", Json.fromString("Having circe in Scala Exercises is awesome"))
    ))
  ))

  println(jsonArray)

  def transformJson(jsonArray: Json): Json =
  jsonArray mapArray { oneJson: Vector[Json] =>
      oneJson.init
    }

  println(transformJson(jsonArray).noSpaces)

  */
  // *********************************************************************** //


  val json0: String = """
     {
       "id": "c730433b-082c-4984-9d66-855c243266f0",
       "name": "Foo",
       "counts": [1, 2, 3],
       "values": {
         "bar": true,
         "baz": 100.001,
         "qux": ["a", "b"]
       }
     } """

  val doc: Json = parse(json0).getOrElse(Json.Null)
//    println(doc)
  val cursor: HCursor = doc.hcursor
  val baz: Decoder.Result[Double] = cursor.downField("values").downField("baz").as[Double]
//  println(baz) // res : Right(100.001)

  val intsJson: Json = List(1, 2, 3).asJson
  //  println(intsJson)
  //  println(intsJson.as[List[Int]])

  //  println(
  //    List(1, 2, 3).asJson.as[List[Int]] == decode[List[Int]]("[1, 2, 3]")
  //  )

  val decodeList = decode[Vector[String]]("[\"a\" , \"b\", \"c\"]")
  //  println(decodeList.isLeft)

//  case class Foo(a: Int, b: String, c: Boolean)


//  implicit val fooDecoder: Decoder[Foo] = deriveDecoder[Foo]
//  implicit val fooEncoder: Encoder[Foo] = deriveEncoder[Foo]
  //  implicit val fooDecoder: Decoder[Foo] = deriveDecoder
  //  implicit val fooEncoder: Encoder[Foo] = deriveEncoder

  /*
  @JsonCodec case class Bar(i: Int, s: String)

  val barJson = Bar(1, "baz").asJson.noSpaces
  */ //Error


  case class User(id: Long, firstName: String, lastName: String)
//  println(User(125, "foo", "bar").asJson.hcursor.downField("firstName").as[String])

  case class Person(name: String)
  case class Greeting(salutation: String, person: Person, exclamationMarks: Int)

  val greetingJson = Greeting("Hey", Person("Chris"), 3).asJson
//  println(
//    greetingJson.hcursor.downField("person").downField("name").as[String]
//    +"\n"+
//    greetingJson.hcursor.downField("salutation").as[String]
//    +"\n"+
//    greetingJson.hcursor.downField("exclamationMarks").as[Int]
//  )

  val mapUsers = Map[Int, User](
    123 -> User(125, "foo", "bar"),
    456 -> User(132, "baz", "toto")
  )
  val jsonUsers = mapUsers.asJson
//  println(jsonUsers)
//  println(jsonUsers.as[Map[Int, User]])

  val mapIntUsers = Map[User, Int](
    User(125, "foo", "bar") -> 1203,
    User(132, "baz", "tot") -> 4506
  )
  implicit val usersLastNameKeyEncoder = new KeyEncoder[User] {
    override def apply(foo: User): String = Json.fromFields(List(
                                                              ("id",Json.fromLong(foo.id)),
                                                              ("firstName",Json.fromString(foo.firstName)),
                                                              ("lastName",Json.fromString(foo.lastName))
                                                            )
                                                          ).noSpaces
  }
  val jsonIntUsers = mapIntUsers.asJson
//  println(jsonIntUsers)

//  implicit val usersLastNameKeyDecoder = new KeyDecoder[User] {
//    override def apply(key: String): Option[User] = Some(User(
//      parse(key).getOrElse(Json.Null).hcursor.downField("id").as[Int].toSeq(0),
//      parse(key).getOrElse(Json.Null).hcursor.downField("firstName").as[String].toOption.getOrElse("undefined"), //.toSeq(0),
//      parse(key).getOrElse(Json.Null).hcursor.downField("lastName").as[String].toSeq(0)
//    ))
//  }
  implicit val usersLastNameKeyDecoder = new KeyDecoder[User] {
      override def apply(key: String): Option[User] = Some(User(
        root.id.long.getOption(parse(key).getOrElse(Json.Null)).getOrElse(0),
        root.firstName.string.getOption(parse(key).getOrElse(Json.Null)).getOrElse("undefined"),
        root.lastName.string.getOption(parse(key).getOrElse(Json.Null)).getOrElse("undefined")
      ))
    }
//  println(jsonIntUsers.as[Map[User, Int]]) //.toSeq(0))

  //  implicit val decodeUsers: Decoder[User] = new Decoder[User] {
  //    final def apply(c: HCursor): Decoder.Result[User] =
  //      for {
  //        id <- c.downField("id").as[Int]
  //        fName <- c.downField("firstName").as[String]
  //        lName <- c.downField("lastName").as[String]
  //      } yield {
  //        new User(id, fName, lName)
  //      }
  //  }


  class Thing(val foo: String, val bar: Int)
  // defined class Thing

  implicit val encodeThing: Encoder[Thing] = new Encoder[Thing] {
    final def apply(a: Thing): Json = Json.obj(
      ("foo", Json.fromString(a.foo)),
      ("bar", Json.fromInt(a.bar))
    )
  }
//  println(encodeThing)
  // encodeThing: io.circe.Encoder[Thing] = $anon$7@44a3ec6b

  implicit val decodeThing: Decoder[Thing] = new Decoder[Thing] {
    final def apply(c: HCursor): Decoder.Result[Thing] =
      for {
        foo <- c.downField("foo").as[String]
        bar <- c.downField("bar").as[Int]
      } yield {
        new Thing(foo, bar)
      }
  }
//  println(decodeThing)
  // decodeThing: io.circe.Decoder[Thing] = $anon$7@44a3ec6b


  case class Foo(value: String)

  implicit val fooKeyEncoder = new KeyEncoder[Foo] {
    override def apply(foo: Foo): String = foo.value
  }

  val mapFoo = Map[Foo, Int](
    Foo("hello") -> 123,
    Foo("world") -> 456
  )

  val jsonFoo = mapFoo.asJson
//  println(jsonFoo)

  implicit val fooKeyDecoder = new KeyDecoder[Foo] {
    override def apply(key: String): Option[Foo] = Some(Foo(key))
  }

//  println(jsonFoo.as[Map[Foo, Int]])
  // res : Decoder.Result[Map[Foo,Int]] = Right(Map(Foo(hello) -> 123, Foo(world) -> 456))


  val jsonOrder: Json = parse("""
      {
        "order": {
          "customer": {
             "name": "Custy McCustomer",
             "contactDetails": {
                "address": "1 Fake Street, London, England",
                "phone": "0123-456-789"
             }
          },
          "items": [{
              "id": 123,
              "description": "banana",
              "quantity": 1
            }, {
              "id": 456,
              "description": "apple",
              "quantity": 2
          }],
          "total": 123.45
        }
      }
    """).getOrElse(Json.Null)

//  println(jsonOrder)

  val _address: Optional[Json, String] = root.order.customer.contactDetails.address.string
  val address: Option[String] = _address.getOption(jsonOrder)
  //  println(address) //Some(1 Fake Street, London, England)

  val addressModify = root.order.customer.contactDetails.address.string.modify(_ => s"22 Chez moi à la Maison")(jsonOrder)
//  println(addressModify)

  val items: Vector[Json] = jsonOrder.hcursor
      .downField("order")
      .downField("items")
      .focus
      .flatMap(_.asArray)
      .getOrElse(Vector.empty)

  println(items)

  val descriptions: Vector[String] = items.flatMap(_.hcursor.get[String]("description").toOption)

//  println(descriptions)

//  val doubleQuantities: Json => Json = root.order.items.each.quantity.int.modify(_ * 2)
  val doubleQuantities: Json => Json = root.order.items.each.quantity.int.modify(_ * 2)

//  println(doubleQuantities(jsonOrder))
}
