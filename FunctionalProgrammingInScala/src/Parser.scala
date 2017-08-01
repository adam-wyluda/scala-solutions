package fpinscala.parser

import fpinscala.errors._
import fpinscala.propertytesting.proptests._

import Option._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  implicit def forString(s: String): Parser[String] = string(s)
  implicit def forRegex(r: Regex): Parser[String] = regex(r)
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def string(s: String): Parser[String]
  def regex(r: Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
  def succeed[A](a: A): Parser[A]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ +: _) or succeed(Nil)
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ +: _)
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))(_ +: _)

  def separatedBy[A, B](p: Parser[A], s: Parser[B]): Parser[List[A]] =
    (p ** (s.slice ** p).many).map {
      case head ** rest => head +: rest.map(_._2)
    }

  def enclosedBy[A, S, E](p: Parser[A])(s: Parser[S], e: Parser[E]): Parser[A] =
    (s ** p ** e).map {
      case _ ** a ** _ => a
    }

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  trait Errors {
    def label[A](msg: String)(parser: Parser[A]): Parser[A]
    def label[A](msg: String => String)(parser: Parser[A]): Parser[A]
    def located[A](msg: (Int, Int) => String)(parser: Parser[A]): Parser[A]
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]) = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]) = self.or(p, p2)
    def map[B](f: A => B) = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    def many = self.many(p)
    def slice = self.slice(p)
    def product[B](p2: Parser[B]) = self.product(p, p2)
    def **[B](p2: Parser[B]) = self.product(p, p2)
    def separatedBy[B](p2: Parser[B]) = self.separatedBy(p, p2)
    def enclosedBy[S, E](s: Parser[S], e: Parser[E]): Parser[A] = self.enclosedBy(p)(s, e)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    def succeedLaw(in: Gen[String]): Prop =
      forAll(in ** in) { case s1 ** s2 => run(succeed(s1))(s2) == Right(s1) }

    def productLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      equal(p1 ** p2, p2 ** p1 map { case (a, b) => (b, a) })(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.sizedString) {
        case (input, msg) =>
          run(label(msg)(p))(input) match {
            case Left(e) => errorMessage(e) == msg
            case _ => true
          }
      }
  }

  object Test {
    val numA: Parser[Int] = char('a').many.map(_.size)
    val x1 = run(("a" | "b").many.slice)("aaba")
    val x2 = char('a').many.slice.map(_.size)

    def digitParser(c: Char) =
      for {
        digitChar <- "[0-9]".r
        digit = digitChar.toInt
        list <- listOfN(digit, c.toString)
      } yield (digitChar + list.mkString)
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col =
    input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

  lazy val fromOffset = input.substring(offset)

  def toError(msg: String) = ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)
}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String) = copy(stack = (loc, msg) +: stack)

  def label[A](s: String) = ParseError(latestLoc.map(loc => (loc, s)).toList)

  def latestLoc: Option[Location] = latest.map(_._1)

  def latest: Option[(Location, String)] = stack.lastOption.fromStdOpt
}

object TheParserResult {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] =
      this match {
        case Failure(e, isCommitted) => Failure(f(e), isCommitted)
        case _ => this
      }

    def uncommit: Result[A] =
      this match {
        case Failure(e, true) => Failure(e, false)
        case _ => this
      }

    def addCommit(isCommitted: Boolean): Result[A] =
      this match {
        case Failure(e, c) => Failure(e, c || isCommitted)
        case _ => this
      }

    def advanceSuccess(n: Int): Result[A] =
      this match {
        case Success(a, m) => Success(a, n + m)
        case _ => this
      }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
}

object TheParser extends Parsers[TheParserResult.Parser] {
  import TheParserResult._

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    val loc = Location(input, 0)
    val result = p(loc)

    result match {
      case Success(get, _) => Right(get)
      case Failure(get, _) => Left(get)
    }
  }

  def string(s: String): Parser[String] = { location =>
    if (location.fromOffset.startsWith(s)) {
      Success(s, s.length)
    } else {
      Failure(location.toError(s"Expected: $s"), true)
    }
  }

  def regex(r: Regex): Parser[String] =  { location =>
    val matcher = r.pattern.matcher(location.fromOffset)
    if (matcher.matches()) {
      val group = matcher.group()
      Success(group, group.length)
    } else {
      Failure(location.toError(s"Expected pattern: ${r.regex}"), true)
    }
  }

  def slice[A](p: Parser[A]): Parser[String] = { location =>
    p(location) match {
      case Success(_, charsConsumed) => {
        val str = location.fromOffset.substring(0, charsConsumed)
        Success(str, charsConsumed)
      }
      case fail @ Failure(_, _) => fail
    }
  }

  def succeed[A](a: A): Parser[A] = { location =>
    Success(a, 0)
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.label(msg))

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.push(loc, msg))

  def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
    loc => x(loc) match {
      case Failure(e, false) => y(loc)
      case r => r
    }

  def attempt[A](p: Parser[A]): Parser[A] =
    loc => p(loc).uncommit

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    loc => p(loc) match {
      case Success(a, n) =>
        f(a)(loc.advanceBy(n))
          .addCommit(n != 0)
          .advanceSuccess(n)
      case e @ Failure(_, _) => e
    }

  def errorLocation(e: ParseError): Location = ???

  def errorMessage(e: ParseError): String = ???
}

object Parsing extends App {
  sealed trait JSON
  object JSON {
    case class JNull() extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }

  def jsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    import JSON._

    object JsonParser {
      val whitespace = (" " | "\n").many.slice
      def spaced(c: Char) = (whitespace ** char(c) ** whitespace).slice

      val name = regex("[a-zA-Z][a-zA-Z0-9]*".r)

      val jNull: Parser[JNull] = string("null").map(_ => JNull())
      val jNumber: Parser[JNumber] = regex("[0-9]+".r).map(str => JNumber(str.toDouble))
      val jString: Parser[JString] = regex("\"[a-zA-Z0-9 ]*\"".r).map(JString.apply)
      val jBool: Parser[JBool] = ("true" | "false").map(str => JBool(str.toBoolean))

      val jList: Parser[List[JSON]] = jAny.separatedBy(spaced(','))
      val jArray: Parser[JArray] = jList.enclosedBy(spaced('['), spaced(']')).map(list => JArray(list.toVector))

      val jPair: Parser[(String, JSON)] =
        (name ** spaced(':') ** jAny).map {
          case name ** _ ** obj => (name, obj)
        }
      val jObject: Parser[JObject] = jPair.separatedBy(spaced(',')).map(list => JObject(list.toMap))

      val jAny: Parser[JSON] = jNull | jNumber | jString | jBool | jArray | jObject
    }

    JsonParser.jAny
  }

  val parser = jsonParser(TheParser)
  val result = TheParser.run(parser)(
    """
      |{
      |"test": 10
      |}
    """.stripMargin
  )

  println(result)
}
