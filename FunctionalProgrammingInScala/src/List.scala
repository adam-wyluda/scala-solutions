package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  @tailrec
  def sum(ints: List[Int], init: Int = 0): Int =
    ints match {
      case Nil => init
      case Cons(x, xs) => sum(xs, init = x + init)
    }

  @tailrec
  def product(ds: List[Double], init: Double = 1): Double =
    ds match {
      case Nil => init
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => product(xs, x * init)
    }

  def firstTwo[A](l: List[A]): Option[(A, A)] =
    l match {
      case Cons(a, Cons(b, _)) => Some((a, b))
      case _ => None
    }

  def head(list: List[_]) =
    list match {
      case Cons(head, _) => head
    }

  def tail(list: List[_]) =
    list match {
      case Cons(_, tail) => tail
    }

  def setHead[A](newHead: A, list: List[A]) =
    list match {
      case Cons(_, tail) => Cons(newHead, tail)
    }

  def drop[A](list: List[A], n: Int): List[A] =
    list match {
      case Cons(_, tail) if n > 0 => drop(tail, n - 1)
      case Cons(_, tail) => tail
    }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] =
    list match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case Cons(_, tail) => tail
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](list: List[A]): List[A] =
    list match {
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs)(z)(f))
    }

  @tailrec
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs)(f(z, x))(f)
    }

  def foldLeftByRight[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldRight(as)(identity[B] _)((a, b) => (x) => f(b(x), a))(z)

  def foldRightByLeft[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)(identity[B] _)((b, a) => (x) => b(f(a, x)))(z)

  def reverse[A](list: List[A]): List[A] =
    foldLeft(list)(Nil: List[A])((b, a) => Cons(a, b))

  def length[A](as: List[A]): Int = foldRight(as)(0)((a, b) => b + 1)

  def string(list: List[_]): String =
    list match {
      case Nil => ""
      case Cons(h, Nil) => h.toString
      case Cons(h, t) => h.toString + ", " + string(t)
    }

  def appendByFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRightByLeft(a1)(a2)((a, b) => Cons(a, b))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRightByLeft(l)(Nil: List[A])(appendByFold)

  def inc(l: List[Int]) =
    foldRightByLeft(l)(Nil: List[Int])((a, b) => Cons(a + 1, b))

  def toStr(l: List[Double]) =
    foldRightByLeft(l)(Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRightByLeft(as)(Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(as)(Nil: List[A])((b, a) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def filterMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def add(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add(t1, t2))
    case (Nil, Nil) => Nil
  }

  def zipWith[A, B](a: List[A], b: List[B]): List[(A, B)] = (a, b) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zipWith(t1, t2))
    case (Nil, Nil) => Nil
  }

  @tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(a, ta), Cons(b, tb)) if a == b => startsWith(ta, tb)
    case (_, Nil) => true
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    startsWith(sup, sub) || (sup match {
          case Cons(_, tail) => hasSubsequence(tail, sub)
          case _ => false
        })

  def insert[A](list: List[A], el: A, pos: Int): List[A] =
    if (pos == 0) Cons(el, list)
    else
      list match {
        case Cons(head, tail) => Cons(head, insert(tail, el, pos - 1))
        case _ =>
          throw new IllegalArgumentException(
              "Can't insert element at given position")
      }

  def delete[A](list: List[A], pos: Int): List[A] =
    list match {
      case Cons(head, tail) if (pos == 0) => tail
      case Cons(head, tail) => Cons(head, delete(tail, pos - 1))
      case _ =>
        throw new IllegalArgumentException(
            "Can't delete element at given position")
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object ListMain extends App {
  import List._

  val l = List(1, 2, 3)
  println(sum(l))

  val d = List(1.5, 3.5, 3.0)
  println(product(d))

  println(firstTwo(d))
  val e = List(5)
  println(firstTwo(e))

  println(string(init(d)))
  println(length(d))

  println(foldLeftByRight(l)(0)(_ + _))
  println(foldRightByLeft(l)(0)(_ + _))
  println(string(reverse(l)))

  println(string(appendByFold(l, d)))

  println("ll ------")
  val ll = List(l, d)
  println(string(flatten(ll)))

  println(string(inc(l)))
  println(toStr(d))

  println(string(map(l)(x => 0.5 * x * x)))
  println(string(filter(l)(_ % 2 == 0)))

  println(string(flatMap(l)(i => List(i, i))))
  println(string(filterMap(l)(_ % 2 == 0)))

  println("add ------")
  val l2 = List(4, 5, 6)
  println(string(add(l, l2)))
  println(string(zipWith(l, l2)))

  println("subsequence -----")
  val sup = List(1, 2, 3, 4)
  val sup2 = Nil
  val sub = List(2, 3)
  val sub2 = Nil
  val sub3 = List(2, 4)
  println(hasSubsequence(sup, sub))
  println(hasSubsequence(sup, sub2))
  println(hasSubsequence(sup, sub3))
  println(hasSubsequence(sup2, sub))

  val ins = insert(l, 100, 3)
  println(string(ins))
  val del = delete(ins, 1)
  println(string(del))
}
