package fpinscala.propertytesting

import fpinscala.pure.{RNG, SimpleRNG, State}
import fpinscala.datastructures.streams._

import fpinscala.parallel.Par
import java.util.concurrent.{ExecutorService, Executors}

package object proptests {
  import Prop._

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop =
      Prop { (s, n, rng) =>
        val r1 = run(s, n, rng)
        val r2 = p.run(s, n, rng)
        (r1, r2) match {
          case (r @ Falsified(_, _), _) => r
          case (_, r @ Falsified(_, _)) => r
          case _ => Passed
        }
      }

    def ||(p: Prop): Prop =
      Prop { (s, n, rng) =>
        val r1 = run(s, n, rng)
        val r2 = run(s, n, rng)
        (r1, r2) match {
          case (r @ Passed, _) => r
          case (_, r @ Passed) => r
          case (Falsified(f1, sc1), Falsified(f2, sc2)) =>
            Falsified(s"$f1, $f2", sc1 + sc2)
        }
      }
  }

  object Prop {
    type MaxSize = Int
    type TestCases = Int

    type SuccessCount = Int
    type FailedCase = String

    def run(
      p: Prop,
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = SimpleRNG(123L)
    ): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
        case Passed => println(s"+ OK, passed $testCases tests.")
        case Proved => println(s"+ OK, proved property.")
      }
  }

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified = false
  }

  case object Proved extends Result {
    override def isFalsified = false
  }

  case class Falsified(
    failure: FailedCase,
    successCount: SuccessCount) extends Result {
    override def isFalsified = true
  }

  case class Gen[+A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(f(_).sample))

    def map[B](f: A => B): Gen[B] =
      Gen(sample.map(f))

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(sample.map2(g.sample)(f))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap { i =>
        Gen(State.sequence(List.fill(i)(sample)))
      }

    def unsized: SGen[A] = SGen(_ => this)

    def **[B](g: Gen[B]): Gen[(A, B)] = map2(g)(Tuple2.apply)
  }

  case class SGen[+A](forSize: Int => Gen[A]) {
    def flatMap[B](f: A => SGen[B]): SGen[B] =
      SGen(size => forSize(size).flatMap(f(_).forSize(size)))

    def map[B](f: A => B): SGen[B] =
      SGen(size => forSize(size).map(f))

    def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] =
      SGen(size => forSize(size).map2(g.forSize(size))(f))

    def **[B](g: SGen[B]): SGen[(A, B)] = map2(g)(Tuple2.apply)
  }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  object Gen {
    import SimpleRNG._

    private def genState[A](run: RNG => (A, RNG)): Gen[A] = Gen(State(run))

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      genState {
        map(nonNegativeLessThan(stopExclusive - start))(_ + start)
      }

    def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
      genState {
        val range = choose(start, stopExclusive).sample.run
        map2(range, range)(Tuple2.apply)
      }

    def unit[A](a: => A): Gen[A] =
      genState {
        SimpleRNG.unit[A](a)
      }

    val boolean: Gen[Boolean] =
      genState {
        map(int)(_ % 2 == 0)
      }

    val double: Gen[Double] = genState(SimpleRNG.double)

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      genState {
        sequence(List.fill(n)(g.sample.run(_)))
      }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      for {
        a1 <- g1
        a2 <- g2
        rand <- boolean
        selected = if (rand) a1 else a2
      } yield selected

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val (gen1, p1) = g1
      val (gen2, p2) = g2
      val sum = p1 + p2
      for {
        a1 <- gen1
        a2 <- gen2
        randToOne <- double
        rand = randToOne * sum
        selected = if (rand <= p1) a1 else a2
      } yield selected
    }

    def genOpt[A](g: Gen[A]): Gen[Option[A]] =
      genState {
        map(g.sample.run)(Some.apply)
      }

    def genUnopt[A](gOpt: Gen[Option[A]], default: Gen[A]): Gen[A] =
      genState {
        map2(gOpt.sample.run, default.sample.run)(_ getOrElse _)
      }

    private val stringArray =
      "abcdefghijklmnopqrstvwxyzABCDEFGHIJKLMNOPQRSTVWXYZ0123456789"

    val char: Gen[Char] =
      genState {
        map2(
          SimpleRNG.unit(stringArray),
          choose(0, stringArray.length).sample.run
        ) {
          _ charAt _
        }
      }

    def string(length: Int): Gen[String] =
      genState {
        map(
          listOfN(length, char).sample.run
        ) (
          _.map(_.toString).reduce(_ + _)
        )
      }

    def dynString(length: Gen[Int]): Gen[String] =
      length.flatMap(string)

    val chooseSize: SGen[Int] = SGen(Gen.unit(_))
    val chooseToSize: SGen[Int] = SGen(i => Gen.choose(0, 1 max i))

    val sizedString = SGen(i => string(i))

    def listOf[A](gen: Gen[A]): SGen[List[A]] = SGen(listOfN(_, gen))
    def listOf1[A](gen: Gen[A]): SGen[List[A]] = SGen(listOf(gen).forSize compose (_ max 1))

    def deepPar[A](gen: Gen[A]): SGen[Par[A]] =
      SGen { depth =>
        gen.map { a =>
          (0 until depth).foldLeft(Par.unit(a)) { (p, _) =>
            Par.fork(p)
          }
        }
      }
  }

  case class Exhaustive[+A](iterable: Iterable[A]) {
    def flatMap[B](f: A => Iterable[B]) = iterable.flatMap(f)
    def map[B](f: A => B) = iterable.map(f)
  }

  object Exhaustive {
    val boolean = Exhaustive(List(false, true))
    val byte = Exhaustive(Range(Byte.MinValue, Byte.MaxValue).map(_.toByte))
    val short = Exhaustive(Range(Short.MinValue, Short.MaxValue).map(_.toShort))
    val int = Exhaustive(Range(Int.MinValue, Int.MaxValue))
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props
            .map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) })
            .toList
            .reduce(_ && _)
        prop.run(max, n, rng)
    }

  def forAll[A](g: Gen[A])(f: A => Boolean): Prop =
    Prop {
      (_, n, rng) =>
        randomStream(g)(rng)
          .zipWith(Stream.from(0))
          .take(n)
          .map((checkFor(f) _).tupled)
          .find(_.isFalsified)
          .getOrElse(Passed)
    }

  def forAll[A](ex: Exhaustive[A], continue: Boolean = false)(f: A => Boolean): Prop =
    Prop {
      (_, _, _) =>
        ex.iterable
          .zipWithIndex
          .map((checkFor(f) _).tupled)
          .find(_.isFalsified)
          .getOrElse(Passed)
    }

  private def checkFor[A](f: A => Boolean)(a: A, i: Int): Result =
    try {
      if (f(a)) Passed else Falsified(a.toString, i)
    } catch {
      case e: Exception => Falsified(buildMsg(a, e), i)
    }

  def check(p: => Boolean): Prop =
    Prop {
      (_, _, _) =>
        if (p) Proved else Falsified("()", 0)
    }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s}\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val S =
    Gen.weighted(
      Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .0,
      Gen.unit(Executors.newCachedThreadPool) -> .25
    )

  val SG = SGen(_ => S)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

  def forAllPar[A](g: SGen[Par[A]])(f: A => Par[Boolean]): Prop =
    forAll(SG ** g) { case s ** ga => Par.flatMap(ga)(f)(s).get }

//  def treeGen: SGen[Tree]
}

object PropertyTesting extends App {
  import proptests._
  import Gen._

  def printline = println("\n-------------------\n")

  val listGen = listOfN(20, choose(5, 20))
  println(listGen.sample.run(SimpleRNG(1337)))

//  val stringGen = string(8)
  val stringGen = dynString(choose(10, 30).map(_ - 9))
  println(stringGen.sample.run(SimpleRNG(42)))
  println(stringGen.listOfN(choose(5, 20)).sample.run(SimpleRNG(42)))

  val unionGen = union(
    choose(1, 10),
    choose(50, 100)
  ).listOfN(unit(20))
  println(unionGen.sample.run(SimpleRNG(-1)))

  val weightedGen = weighted(
    (choose(1, 10), 5.0),
    (choose(50, 100), 1.5)
  ).listOfN(unit(50))
  println(weightedGen.sample.run(SimpleRNG(37)))

  printline

  val prop =
    forAll(unit(5))(_ == 5) &&
      forAll(choose(0, 100).listOfN(unit(10)))(_.forall(_ != 67))
  println(prop.run(100, 1500, SimpleRNG(10)))

  val sGen = listOf(choose(5, 15))
  println(sGen.forSize(10).sample.run(SimpleRNG(42)))
  println(sGen.map(_.sum).forSize(10).sample.run(SimpleRNG(42)))

  val sProp = forAll(sGen)(list => if (list.size < 15) list.sum == list.reverse.sum else false)
  println(sProp.run(50, 300, SimpleRNG(10)))

  val smallInt = Gen.choose(-10, 10)
  val smallList = listOf1(smallInt)
  val maxProp = forAll(smallList) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

//  println(maxProp.run(10, 500, SimpleRNG(20)))
  Prop.run(maxProp)

  val sortedProp =
    forAll(smallList)(ns => ns.sorted == ns.reverse.sorted) &&
      forAll(smallList)(ns => ns.sorted.sum == ns.sum) &&
      forAll(smallList)(ns => ns.sorted.head == ns.min) &&
      forAll(smallList)(ns => ns.sorted.last == ns.max) &&
      forAll(smallList)(ns => ns.sorted.size == ns.size) &&
      forAll(smallList)(ns => ns.sorted.forall(ns.contains)) &&
      forAll(smallList) { ns =>
        val (result, _) =
          ns.sorted.foldLeft((true, ns.min)) {
            case ((result, previous), next) => (result && previous <= next, next)
          }
        result
      }
  Prop.run(sortedProp)

  val absProp = forAll(Exhaustive.byte) { b => Math.abs(b * b) == Math.abs(b) * Math.abs(b) }
  Prop.run(absProp)

  printline

  val ES: ExecutorService = Executors.newCachedThreadPool()

  val parProp =
    check {
      val p = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    }
  Prop.run(parProp)

  val parProp2 =
    check(
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(ES).get
    )
  Prop.run(parProp2)

  val parProp3 =
    checkPar(
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    )
  Prop.run(parProp3)

  val pint = Gen.choose(0, 10).map(Par.unit)
  val p4 = forAllPar(pint)((n: Par[Int]) => equal(Par.map(n)(identity), n))
  Prop.run(p4)

  val p5 = forAllPar(pint)((n: Par[Int]) => equal(Par.fork(n), n))
  Prop.run(p5)

  val deepInt = deepPar(Gen.choose(0, 10))
  val deepP = forAllPar(deepInt)(n => equal(Par.map(Par.unit(n))(identity), Par.unit(n)))
  Prop.run(deepP)

  printline

  {
    println("Testing our data structures")

    import fpinscala.datastructures.List
    import List._

    val listGen = listOf1(Gen.choose(0, 1000)).map(List.apply)
    val withSize = listGen ** chooseToSize

    val takeProp =
      forAll(withSize) { case (list, size) => length(take(list, size)) == size } &&
        forAll(withSize) { case (list, size) => startsWith(list, take(list, size)) }
    Prop.run(takeProp)

    val dropProp =
      forAll(withSize) { case (list, size) => length(drop(list, size)) == length(list) - size }
    Prop.run(dropProp)
  }

  println("Finish")
}
