package fpinscala.datastructures.streams

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

//  def headOption: Option[A] =
//    this match {
//      case Empty => None
//      case Cons(h, _) => Some(h())
//    }

  def headOption: Option[A] = foldRight(Option.empty[A])((a, b) => Some(a))

//  def toList: List[A] =
//    this match {
//      case Empty => Nil
//      case Cons(h, t) => h() +: t().toList
//    }

  def toList: List[A] = foldRight(List.empty[A])(_ +: _)

  def take(n: Int): Stream[A] =
    if (n == 0) Empty
    else
      this match {
        case Empty => Empty
        case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else
      this match {
        case Empty => Empty
        case Cons(h, t) => t().drop(n - 1)
      }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

  def existsFold(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](a: => B): Stream[B] =
    foldRight(cons(a, Empty))((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (a, b) =>
      f(a).foldRight(b)((c, d) => cons(c, d))
    }

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (n, Cons(h, t)) if n > 0 => Some(h(), (n - 1, t()))
      case _ => None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B](other: Stream[B]): Stream[(A, B)] =
    unfold((this, other)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some((ah(), bh()), (at(), bt()))
      case _ => None
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, other)) {
      case (Cons(ah, at), Cons(bh, bt)) =>
        Some((Some(ah()), Some(bh())), (at(), bt()))
      case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), Empty))
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (Empty, bt()))
      case _ => None
    }

  def startsWith[A](other: Stream[A]): Boolean =
    (this zipAll other).foldRight(true) { (pair, p) =>
      pair match {
        case (Some(a), Some(b)) => a == b && p
        case (Some(a), None) => true
        case _ => false
      }
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case state @ Cons(_, t) => Some(state, t())
      case Empty => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
  // TODO Too hard for me now :(
//    this match {
//      case Cons(h, t) => {
//        f(h(), scanRight(z)(f))
//        cons(z, Empty)
//      }
//      case Empty => cons(z, Empty)
//    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, Stream(as.tail: _*))

  val ones: Stream[Int] = Stream.constant(1)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def stream(a: Int, b: Int): Stream[Int] = cons(a, stream(b, a + b))
    stream(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }

  val onesUnfold = Stream.unfold(1)(Some(_, 1))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(Some(_, a))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

  val fibsUnfold: Stream[Int] = unfold((0, 1)) {
    case (a, b) => Some(a, (b, a + b))
  }
}

object Streams {

  def main(args: Array[String]) {
    import Stream._

    val stream = Stream(1, 2, 3, 4, 5)
    println(stream.headOption)
    println(empty.headOption)
    println(stream.toList)
    println()

    println("take/drop")
    println(stream.take(3).toList)
    println(stream.take(7).toList)
    println(stream.drop(3).toList)
    println(stream.drop(7).toList)
    println(stream.takeWhile(_ < 5).toList)
    println(stream.takeWhile(_ < 7).toList)
    println(stream.takeWhileFold(_ < 5).toList)
    println(stream.takeWhileFold(_ < 7).toList)
    println()

    println("headOption")
    println(stream.headOption)
    println(empty[Int].headOption)
    println()

    println("map/filter/append/flatMap")
    val mapped = stream.map("*" * _)
    println(mapped.append("LOL").toList)
    println(mapped.toList)
    println(mapped.filter(_.size % 2 == 0).toList)
    println()

    println("flatMap")
    println(stream.flatMap(stream.take(_)).toList)
    println()

    println("infinite")
    println(ones.take(5).toList)
    println(ones.forAll(_ < 1))
    println(constant(5).take(5).toList)
    println(from(13).take(10).toList)
    println(fibs.take(10).toList)
    println()

    println("unfold")
    println(onesUnfold.take(5).toList)
    println(onesUnfold.forAll(_ < 1))
    println(constantUnfold(5).take(5).toList)
    println(fromUnfold(13).take(10).toList)
    println(fibsUnfold.take(10).toList)
    println(
      fibsUnfold
        .mapUnfold(_ + 10)
        .zipWith(fromUnfold(27))
        .zipAll(fromUnfold(-5).take(6))
        .takeUnfold(10)
        .toList)
    println()

    println("startsWith")
    println(fibsUnfold startsWith Stream(0, 1, 1, 2, 3, 5))
    println(constant(0) startsWith fibsUnfold)
    println()

    println("tails/hasSubsequence")
    println(stream.tails map (_ toList) toList)
    println(fibsUnfold.tails.take(5) map (_ take (5) toList) toList)
    println(fibsUnfold hasSubsequence stream.take(3))
    println(fibsUnfold.take(150) hasSubsequence stream.take(4))
    println(fibsUnfold hasSubsequence Stream(21, 34, 55, 89, 144))
    println()

    println("scanRight")

    println()
  }
}
