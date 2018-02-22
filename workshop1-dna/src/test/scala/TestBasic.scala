import models._
import scala.io.Source

import scala.annotation.tailrec

object TestBasic extends App {

  val list = List(1, 2)

  list match {
    case one :: two :: Nil =>
      println(two)
    case _ =>
      println("WTF")
  }

  case class Person(name: String, age: Int, married: Option[Person] = None)

  val elie = Person("Toi", 32, Some(Person("adel", 26)))

  elie match {
    case Person(n, a, Some(Person(fn, _, _))) if fn == "ade" => println(s"$n a $a ans")
    case _ => println("Not exist")
  }


  println(List(6, 2, 4).forall( i => i % 2 == 0))
  println(List(4, 3, 4).forall( i => i % 2 == 0))
  println(List(3, 2).sum)


  val fruits = Seq("apple", "banana", "orange")
  println(fruits)
  val fruitsUpper = fruits map (_.toUpperCase)
  println(fruitsUpper)
  println(fruits flatMap (_.toUpperCase))



/*  for {
    s <- Seq ("A", "B", "C")
  } {
    val rst: Seq[String] = s match {
      case "A" => Seq("E")
      case "B" => Seq("D")
      case _ => None
    }
    println(rst)
  }*/


  sealed trait Base
  case object A extends Base
  case object C extends Base
  case object B extends Base

  val s = Seq(A, B, C)
  val rst: Seq[Base] = s flatMap(_ match {
    case A => Some(C)
    case B => Some(C)
    case _ => None
  })
  println(rst)

  val s2 = Seq("apple", "oranges", "apple", "banana", "apple", "oranges", "oranges")
//  val resultCount = s2.groupBy(l => l).map(t => (t._1, t._2.length))
  val resultCount = s.groupBy(l => l).map(t => (t._1, t._2.length))
  println(resultCount)


  val capital1 = Map("US" -> "Washington", "France" -> "Paris")
  val capital2 = capital1 ++ Map("Japan" -> "Tokio")

  println(capital2("France")+ " ; "+ capital2)


  val seq1 = Seq("a", "b", "c", "d", "e", "b", "c", "f", "j")
  val subSeq1_no = Seq("b", "c", "e")
  val subSeq1_ye = Seq("b", "c", "f")

  println("contain : " + subSeq1_no.forall( c => seq1.contains(c)) )
  println("contain seq: " + seq1.contains(subSeq1_ye))
  println("contain seq with Slice : " + seq1.containsSlice(subSeq1_ye))
  println("contain seq: " , seq1.tails exists (_.startsWith(subSeq1_ye)))
  println("Not contain seq: " , seq1.tails exists (_.startsWith(subSeq1_no)))

  val seq2 = Seq(A, G, T, C, G, T) //Seq(A, T, C, G)
  val subSeq2_no = Seq(T, C, T) //Seq(C, G)
  val subSeq2_ye = Seq(C, G)
  val subSeq2_empty = Seq()

  println("contain DNA : " + subSeq2_no.forall(seq2.contains))

  println("contain DNA seq: " , seq2.tails exists (_.startsWith(subSeq2_ye)))
  println("No contain DNA seq: " , seq2.tails exists (_.startsWith(subSeq2_no)))
  println("contain DNA seq empty: " , seq2.tails exists (_.startsWith(subSeq2_empty)))

  println("======================")

  //  println(seq1(2))
  println(seq1.length)
  val rstInsrtingSeq = {
    //    seq1.splitAt(2)._1 ++ subSeq2_ye ++ seq1.splitAt(2)._2
    seq1.splitAt(0)._1 ++ List("Z") ++ seq1.splitAt(0)._2
  }
  println(rstInsrtingSeq)
  println(rstInsrtingSeq.length)

  val myObject: Map[Any, Any] = Map("prop1" -> 1, 250 -> "string", A -> Seq(1, 2, 3))

  for ((key, value) <- myObject) {
    println(key +" : "+ value)
  }

  myObject map(c => println(c._1 +" : "+ c._2))

  println("======================")

  val dna1 = Seq(A, T, C, G, T, G)
  val dna2 = Seq(C, T, G, A)
  val N = math.min(dna1.size, dna2.size)
  println(N)

//  println(A eq A)
//  println(A != T)

  //  var nbr = (dna2 zip dna1) map {c => if (c._1 eq(c._2)) 258}
  //  println(nbr)

  val nbr1 = {
    for {
        c <- (dna2 zip dna1)
        if (c._1 != c._2)
        x: Int = 1
    } yield x
  }.sum

  println(nbr1)

  println("======================")

  (dna2 zip dna1).zipWithIndex foreach {case(c,i) => println(c , i)}

  val nbr2 = for {
    ((a, b), c) <- (dna2 zip dna1).zipWithIndex
    if (a != b)
  } yield c

  println(nbr2)

  private val str = """
    |foot
    |ball
    """
  println(str +" ;\n"+ str.trim +" ;\n"+ str.stripMargin)

  private val translationTableSource = """
     |FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG
     |TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG
     |TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG
     |TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG
   """.trim.stripMargin

  def parse(tab: Array[String]) : Map[String,Char] = {
    def loop(acu: Map[String, Char], i: Int): Map[String, Char] = {
      if(tab(0).size == i)
        acu
      else {
        val one = tab(1)(i)
        val two = tab(2)(i)
        val three = tab(3)(i)
        val aa = tab(0)(i)
        loop(acu + (s"$one$two$three" -> aa ), i+1)
      }
    }
    loop(Map.empty, 0)
  }

  val translationTableSourceParsed = parse(translationTableSource.split("\n"))
  println("translationTableSourceParsed : " + translationTableSourceParsed)

//  translationTableSourceParsed map { a => println(a) }

//  val egDnaFrame = Seq(A,G,G,T,G,A,C,A,C,C,G,C,A,A,G,C,C,T,T,A,T,A,T,T,A,G,C)
  val egDnaFrame = "AGGTGACACCGCAAGCCTTATATTAGC".toList
//  println(egDnaFrame)

//  val egDnaFrameSlided = egDnaFrame.sliding(3, 3).toList
/*
  val egDnaFrameSlided = egDnaFrame.grouped(3).toList
  println("egDnaFrameSlided : " + egDnaFrameSlided)
  egDnaFrameSlided map { e => println(e)}
*/

/*
  val result = egDnaFrame.grouped(3).toList map { e => translationTableSourceParsed(e mkString)}
  println((result mkString))
*/

/*
  def doTranslate(dna: Seq[Char]): Seq[String] = {
    val rst = dna.grouped(3).toList map { e => translationTableSourceParsed.getOrElse(e mkString, "")}
    Seq(rst. mkString)
  }
//  val result = doTranslate(egDnaFrame)  ++ doTranslate(egDnaFrame.drop(1)) ++ doTranslate(egDnaFrame.drop(2))
val result = doTranslate(List())  ++ doTranslate(List().drop(1)) ++ doTranslate(List().drop(2))
*/

  def translate(dna: Seq[Char]): Seq[String] = {
    def doTranslate(acu: Seq[String] = Seq(), i: Int = 0, nbrFrames: Int): Seq[String] = {
      val dnaTmp = dna.drop(i)
      if (dnaTmp.size < 3 || i == nbrFrames ) {
        acu
      }
      else {
        val rst = dnaTmp.grouped(3).toList map {
          e => translationTableSourceParsed.getOrElse(e mkString, "")
        }
        doTranslate(acu ++ Seq(rst mkString), i+1, nbrFrames)
      }
    }
    doTranslate(nbrFrames = 3)
  }

  val result = translate(egDnaFrame)

  println("result : " + result)

//  println(Seq(A, C).groupBy(identity).mapValues(_.size))

//  val dnaTest = Seq(A,T,T,T,T,A,A,C,C,C,C,G,C,G)
  val dnaTest = Seq('A','T','T','T','T','A','A','C','C','C','C','G','C','G')
  "AACCCTTGGCCCGGTTTTTCCCC".split(raw"(?<=(\w))(?!\1)") map(e => println(e))

  //println((dnaTest mkString).groupBy)

//  def longestSequences(dna: Seq[Char]): Map[Char, Int] = {
  def longestSequences(dna: Seq[Char]) = {
    def loop(sub: Seq[Char], i: Int, accu: Seq[Char] = Seq()): List[Seq[Char]] = {
      if (i == 0) {
        List(accu ++ sub)
      }
      else if (i == dna.size) {
        List(accu)
      }
      else if (sub(i) == dna(i+1)) {
        accu.:+(sub(i))
        loop(dna.slice(i+1, i+2), i + 1)
      }
      else {
        accu :: loop(dna.slice(i, i + 1), i + 1)
      }
    }

    if (dna.size < 1)
      Map.empty
    else
      loop(dna.take(1), 0)
/*        .groupBy(d => d(0))
        .mapValues(l => l.maxBy(d => d.size).size)*/
  }
  println(longestSequences(dnaTest))


  def longestSequences2(dna: Seq[Char]) = {
    val dnaMapped = dna.zip(dna.drop(1)).zipWithIndex.filter { case ((a, b), i) => (a != b) }
    def dnaSplited(accu: Seq[Seq[Char]] = Seq(), dna2: Seq[Char], i: Int = 0): Seq[Seq[Char]] = {
      if (i == 0) {
        dnaSplited(accu ++ Seq(dna2.splitAt(1)._1), dna2.drop(1), i + 1)
      }
      else if (dna2.size != 1 && i < dnaMapped.size) {
        val index = (dnaMapped(i)._2)-(dnaMapped(i-1)._2)
        dnaSplited(accu ++ Seq(dna2.splitAt(index)._1), dna2.drop(index), i+1)
      }
      else {
        if (dna2.size != 0)
          accu ++ Seq(dna2)
        else
          accu
      }
    }
    if (dna.size >= 1)
      dnaSplited(dna2 = dna)
        .groupBy(d => d(0))
        .mapValues(l => l.maxBy(b => b.size).size)
    else
      Map.empty
  }

  println("ici 001 : "+ longestSequences2(dnaTest))
  val test = longestSequences2(Seq('A'))
  println("ici là : "+ test)

  println((Seq(A) ++ Seq(A)).size)


  println("\n ====================== \n")
  // Curryfication
  def multiply(x: Int, y: Int): Int = x*y
//  def multiplyCurrified (x: Int): Int = multiply(x, 2)
  def multiplyCurrified (x: Int): Int = multiply(x, 2)
//  def multiplyCurrified = (multiply _).curried(2)

  def modN(n: Int)(x: Int) = ((x % n) == 0)

  val nums = List(1, 2, 3, 4, 5, 6, 7, 8)
//  println(filter(nums, modN(2)))
//  println(filter(nums, modN(3)))

  def fibonacci(list : List[Int], size : Int): List[Int]= (list, size) match {
    case (_, 0) => List()
    case (_, 1) => List(0)
    case (_, 2) => List(1) ++ fibonacci(list, size-1)
    case (_, _) => {
      if (list.size == size)
        list
      else if (list.size == 0)
        fibonacci(List(1, 0), size)
      else
        fibonacci(List(list.head + list.tail.head) ++ list, size)
    }
  }

  println("fibonacci(List(), 5) " + fibonacci(List(), 5))

  println("======================")


  val fruit1 = "apples" :: ("oranges" :: ("pears" :: ("banana2" :: Nil)))
  val fruit2 = "mangoes" :: ("banana" :: Nil)

  println("fruit1         : " + fruit1)
  println("fruit1.head    : " + fruit1.head)
  println("fruit1.tail    : " + fruit1.tail)
  println("fruit1.take(2) : " + fruit1.take(2))
  println("fruit2 : " + fruit2.take(2))

  // use two or more lists with ::: operator
  println( "fruit1 ::: fruit2           : " + (fruit1 ::: fruit2) )
  println( "fruit1 ++ fruit2            : " + (fruit1 ++ fruit2) )

  // pass two or more lists as arguments
  println( "List.concat(fruit1, fruit2) : " + (List.concat(fruit1, fruit2))  )

  // use two lists with Set.:::() method
  println( "fruit1.:::(fruit2)          : " + (fruit1.:::(fruit2)) )

  println("============ ****************** ==========")

  val array = Array('a', "b", 3)
  val list2 = List(1, 2, 3)
  val map = Map("one" -> 1, "two" -> 2, "three" -> 3)
  val set = Set(1, 2, 3)

  //Concatenating
  val arrayConcat = array ++ Array("d", "e", 'f') .:+ (7) .:+ ("c")
  val listConcat = list2 ++ List(4, 5, 6) .:+ (8)
  val mapConcat = map ++ Map("four" -> 4, "five" -> 5, "six" -> 6) ++ Map("c" -> 1)

  //Legacy ::: oprator in Lists only
  val listConcat2 = list2 ::: List(4, 5, 6) .:+ (9)
  val listConcat3 = list2 :: List(4, 5, 6) .:+ (9)
  val listConcat4 = list2 +: List(4, 5, 6) .:+ (9)
  val listConcat5 = list2 :+ List(4, 5, 6) .:+ (9)
  val listConcat6 = (list2 +: List(4, 5, 6)) .:+ (9)

  println(arrayConcat mkString)
  println(arrayConcat.mkString(";"))

  println(listConcat) //.mkString(","))
  println(listConcat2) //.mkString(","))
  println(listConcat3) //.mkString(","))
  println(listConcat4) //.mkString(","))
  println(listConcat5) //.mkString(","))
  println(listConcat6) //.mkString(","))
  println(mapConcat.mkString(","))

  implicit val n = "John"
  implicit val a = 20

  def greet(greeting:String)(implicit name:String, age:Int) = {
    println(s"$greeting $name, you are $age")
  }

  greet("Hello")

}

