package fpinscala.monoids

import fpinscala.datastructures.{Tree, Leaf, Branch}
import fpinscala.propertytesting.proptests._
import fpinscala.pure.SimpleRNG
import fpinscala.parallel._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 concat a2
    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    def zero: (A) => A = identity
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen) { a =>
      m.op(a, m.zero) == m.op(m.zero, a)
    } &&
    forAll(gen ** gen ** gen) { case ((a1, a2), a3) =>
      m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)
    }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.size == 1) f(v.head)
    else {
      val (s1, s2) = v.splitAt(v.size / 2)
      val r1 = foldMapV(s1, m)(f)
      val r2 = foldMapV(s2, m)(f)
      m.op(r1, r2)
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val pm = par(m)
    if (v.isEmpty) pm.zero
    else if (v.size == 1) Par.unit(f(v.head))
    else {
      val (s1, s2) = v.splitAt(v.size / 2)
      val r1 = Par.fork(parFoldMap(s1, m)(f))
      val r2 = Par.fork(parFoldMap(s2, m)(f))
      pm.op(r1, r2)
    }
  }

  val orderingMonoid: Monoid[(Int, Boolean)] = new Monoid[(Int, Boolean)] {
    def op(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) =
      (a1, a2) match {
        case ((i, l), (j, r)) => (i max j, l && r && i <= j)
      }

    def zero: (Int, Boolean) = (Int.MinValue, true)
  }

  def isOrdered(list: List[Int]): Boolean = foldMap(list, orderingMonoid)((_, true))._2

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a: (A, B), b: (A, B)): (A, B) = (A.op(a._1, b._1), B.op(a._2, b._2))
    def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(
          k,
          V.op(
            a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)
          )
        )
      }

    def zero: Map[K, V] = Map[K, V]()
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))
    def zero: (A) => B = _ => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

object Parsing {
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  private val threshold = 32

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Stub(lChars), Stub(rChars)) => Stub(lChars + rChars)
        case (Stub(lChars), Part(lStub, words, rStub)) => Part(lChars + lStub, words, rStub)
        case (Part(lStub, words, rStub), Stub(lChars)) => Part(lStub, words, rStub + lChars)
        case (Part(lStubL, wordsL, rStubL), Part(lStubR, wordsR, rStubR)) => {
          val words = wordsL + wordsR + (if (rStubL.isEmpty && lStubR.isEmpty) 0 else 1)
          Part(lStubL, words, rStubR)
        }
      }

    def zero: WC = Stub("")
  }

  def wordCount(string: String): Int = {
    ???
//    if (string.isEmpty) 0
//    else if (string.size < threshold)
//    else {
//      val (first, second) = string.splitAt(string.size / 2)
//      val firstWord = firstWord(first)
//      val lastWord = lastWord(second)
//
//    }
  }

  def firstWord(string: String): String = {
    val index = string.indexOf(' ')
    if (index == -1) ""
    else string.substring(0, index)
  }

  def lastWord(string: String): String = {
    val index = string.lastIndexOf(' ')
    if (index == -1) ""
    else string.substring(index + 1)
  }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](f: F[A]): List[A] = foldRight(f)(List.empty[A])(_ +: _)
  def toListByMonoid[A](f: F[A]): List[A] = foldMap(f)(a => List(a))(Monoid.listMonoid)
}

object ListFoldable extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]) = as.map(f).foldLeft(mb.zero)(mb.op)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]) = as.map(f).foldLeft(mb.zero)(mb.op)
}

object StreamFoldable extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]) = as.map(f).foldLeft(mb.zero)(mb.op)
}

object TreeFoldable extends Foldable[Tree] {
  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = Tree.fold(as)(z)((a, b) => f(b, a))
  def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = Tree.fold(as)(z)(f)
  def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]) = foldLeft(Tree.map(as)(f))(mb.zero)(mb.op)
}

object OptionFoldable extends Foldable[Option] {
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]) = as.map(f).foldLeft(mb.zero)(mb.op)
}

object Monoids extends App {
  import Monoid._

  def run(prop: Prop) = prop.run(10, 10, SimpleRNG(23))

  println(run(monoidLaws(stringMonoid, Gen.string(10))))
  println(run(monoidLaws(listMonoid[Int], Gen.listOfN(10, Gen.choose(0, 1000)))))
  println(run(monoidLaws(intAddition, Gen.choose(0, 1000))))
  println(run(monoidLaws(intMultiplication, Gen.choose(0, 1000))))
  println(run(monoidLaws(booleanOr, Gen.boolean)))
  println(run(monoidLaws(booleanAnd, Gen.boolean)))

  val optionGen: Gen[Option[Int]] = Gen.boolean.map2(Gen.choose(0, 2000)) {
    case (isSome, value) => if (isSome) Some(value) else None
  }
  println(run(monoidLaws(optionMonoid[Int], optionGen)))

  val funGen: Gen[Int => Int] = Gen.choose(0, 1000).map(a => (x: Int) => x + a)
  println(run(monoidLaws(endoMonoid[Int], funGen)))

  val ordered = List(1, 3, 4, 5, 10)
  val unordered = List(1, 5, 3, 7, 8)

  println(isOrdered(ordered))
  println(isOrdered(unordered))

  val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
  val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
  val m2 = Map("o1" -> Map("i2" -> 1), "o2" -> Map("i1" -> 7))
  val m3 = M.op(m1, m2)
  println(m3)

  println(bag(Vector("a", "rose", "is", "a", "rose")))

  val m = productMonoid(intAddition, intAddition)
  val p = ListFoldable.foldMap(List(1, 2, 3, 4))(a => (1, a))(m)
  val mean = p._2.toDouble / p._1
  println(mean)
}
