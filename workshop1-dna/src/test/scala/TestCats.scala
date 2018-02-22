

import cats.Functor
import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.{Monoid, Semigroup}




object TestCats extends App {

  println(Semigroup[Int].combine(Semigroup[Int].combine(2, 1),  3))
  println(Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)))
  println(Semigroup[Option[Int]].combine(Option(1), Option(2)))

  println(
    Semigroup[Int ⇒ Int]
      .combine({ (x: Int) ⇒
        x + 1
      }, { (x: Int) ⇒
        x * 10
      })
      .apply(6)
  )

//  val mapComb: Map[String, Map[String, Int]] = Map("foo" -> Map("bar" -> 5)).combine(Map("foo" -> Map("bar" -> 6), "baz" -> Map()))
  val mapComb = Map("foo" -> Map("bar" -> 5)) |+| Map("foo" -> Map("bar" -> 6), "baz" -> Map())
  println(mapComb)

  val list = List(1, 2, 3, 4, 5)
  val (left, right) = list.splitAt(3)
  println((left, right))

  val l = List(1, 2, 3, 4, 5)
  println(l.foldMap(identity))

  val listNem = List(NonEmptyList(1, List(2, 3)), NonEmptyList("b", List(5, 6)))
  val liftedNEL = listNem.map(nel => Option(nel))
  println(listNem +" | "+ liftedNEL)

  println(Monoid.combineAll(liftedNEL))

  println(Monoid.combineAll(List(List(1, 2, "a"), List(4, 5, 6))))

  println(Option(1).map(_ + 1))

  println(Functor[List].map(List("qwer", "adsfg"))(_.length))

  println(Functor[Option].map(Option(""))(_.length))
  println(Functor[Option].map(None: Option[String])(_.length))

  val sourcePr = List("Cats", "is", "awesome")
  val product = Functor[List].fproduct(sourcePr)(_.length).toMap
  println(product)

  val listOpt = Functor[List] compose Functor[Option]
  println(listOpt.map(List(None, Some(0)))(_ + 1))
}
