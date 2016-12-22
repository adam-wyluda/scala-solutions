import scala.util.Random

object Lists {
  // P01 (*) Find the last element of a list.
  def last[A](list: List[A]): A =
    list match {
      case a +: Nil => a
      case h +: t => last(t)
      case Nil => throw new Exception("Can't find last element for an empty list")
    }

  // P02 (*) Find the last but one element of a list.
  def penultimate[A](list: List[A]): A =
    list match {
      case a +: b +: Nil => a
      case h +: t => penultimate(t)
      case Nil => throw new Exception("Can't find penultimate element o")
    }

  // P03 (*) Find the Kth element of a list.
  def nth[A](n: Int, list: List[A]): A =
    list match {
      case h +: t if n == 0 => h
      case h +: t => nth(n - 1, t)
      case Nil => throw new Exception("Couldn't find nth element")
    }

  // P04 (*) Find the number of elements of a list.
  def length(list: List[_]): Int =
    list match {
      case h +: t => 1 + length(t)
      case Nil => 0
    }

  // P05 (*) Reverse a list.
  def reverse[A](list: List[A], acc: List[A] = Nil): List[A] =
    list match {
      case h +: t => reverse(t, h +: acc)
      case Nil => acc
    }

  // P06 (*) Find out whether a list is a palindrome.
  def isPalindrome(list: List[_]): Boolean =
    list == reverse(list)

  // P07 (**) Flatten a nested list structure.
  def flatten[A](list: List[_]): List[A] =
    list match {
      case h +: t =>
        h match {
          case l: List[_] => flatten(l) ++ flatten(t)
          case a: A => a +: flatten(t)
        }
      case Nil => Nil
    }

  // P08 (**) Eliminate consecutive duplicates of list elements.
  def compress[A](list: List[A]): List[A] =
    list match {
      case a +: b +: t if (a == b) => compress(a +: t)
      case a +: t => a +: compress(t)
      case Nil => Nil
    }

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  def pack[A](list: List[A]): List[List[A]] =
    list match {
      case h +: t =>
        pack(t) match {
          case (`h` +: ct) +: p => (h +: h +: ct) +: p
          case p => List(h) +: p
        }
      case Nil => Nil
    }

  // P10 (*) Run-length encoding of a list.
  def encode[A](list: List[A]): List[(Int, A)] =
    pack(list) map {
      case l@(h +: _) => (length(l), h)
    }

  // P11 (*) Modified run-length encoding.
  def encodeModified[A](list: List[A]): List[_] =
    encode(list) map {
      case (1, a) => a
      case (n, a) => (n, a)
    }

  def repeat[A](n: Int, a: A): List[A] =
    n match {
      case 0 => Nil
      case n => a +: repeat(n - 1, a)
    }

  // P12 (**) Decode a run-length encoded list.
  def decode[A](list: List[(Int, A)]): List[A] =
    flatten(list map (repeat[A] _ tupled))

  // P13 (**) Run-length encoding of a list (direct solution).
  def encodeDirect[A](list: List[A]): List[(Int, A)] =
    list match {
      case h +: t =>
        encodeDirect(t) match {
          case (n, `h`) +: p => (n + 1, h) +: p
          case p => (1, h) +: p
        }
      case Nil => Nil
    }

  // P14 (*) Duplicate the elements of a list.
  def duplicate[A](list: List[A]): List[A] =
    list match {
      case h +: t => h +: h +: duplicate(t)
      case Nil => Nil
    }

  // P15 (**) Duplicate the elements of a list a given number of times.
  def duplicateN[A](n: Int, list: List[A]): List[A] =
    flatten(list map (repeat(n, _)))

  // P16 (**) Drop every Nth element from a list.
  def drop[A](n: Int, list: List[A], acc: Int = 1): List[A] =
    (acc, list) match {
      case (`n`, h +: t) => drop(n, t)
      case (_, h +: t) => h +: drop(n, t, acc + 1)
      case (_, Nil) => Nil
    }

  // P17 (*) Split a list into two parts.
  def split[A](n: Int, list: List[A]): (List[A], List[A]) =
    (n, list) match {
      case (0, list) => (Nil, list)
      case (n, h +: t) =>
        split(n - 1, t) match {
          case (l, r) => (h +: l, r)
        }
      case (n, Nil) => throw new Exception("N exceeds list length")
    }

  // P18 (**) Extract a slice from a list.
  def slice[A](i: Int, k: Int, list: List[A]): List[A] =
    split(i, list) match {
      case (_, list) =>
        split(k - i, list) match {
          case (result, _) => result
        }
    }

  // P19 (**) Rotate a list N places to the left.
  def rotate[A](n: Int, list: List[A], back: List[A] = Nil): List[A] =
    if (n < 0) rotate(n + length(list), list)
    else
      split(n, list) match {
        case (l, r) => r ++ l
      }

  // P20 (*) Remove the Kth element from a list.
  def removeAt[A](n: Int, list: List[A]): (List[A], A) =
    (n, list) match {
      case (0, h +: t) => (t, h)
      case (_, h +: t) =>
        removeAt(n - 1, t) match {
          case (list, s) => (h +: list, s)
        }
      case (_, Nil) => throw new Exception("N exceeds list length")
    }

  // P21 (*) Insert an element at a given position into a list.
  def insertAt[A](s: A, n: Int, list: List[A]): List[A] =
    (n, list) match {
      case (0, l) => s +: l
      case (_, h +: t) => h +: insertAt(s, n - 1, t)
      case (_, Nil) => throw new Exception("N exceeds list length")
    }

  // P22 (*) Create a list containing all integers within a given range.
  def range(from: Int, to: Int): List[Int] =
    if (from <= to) from +: range(from + 1, to)
    else Nil

  // P23 (**) Extract a given number of randomly selected elements from a list.
  def randomSelect[A](rng: Random)(n: Int, list: List[A]): List[A] =
    if (n == 0) Nil
    else
      removeAt(rng.nextInt(length(list)), list) match {
        case (rest, el) => el +: randomSelect(rng)(n - 1, rest)
      }

  // P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  def lotto(rng: Random)(n: Int, m: Int): List[Int] =
    randomSelect(rng)(n, range(1, m))

  // P25 (*) Generate a random permutation of the elements of a list.
  def randomPermute[A](rng: Random)(list: List[A]): List[A] =
    randomSelect(rng)(length(list), list)

  // P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  def combinations[A](list: List[A], k: Int, n: Int): List[List[A]] =
    (list, k, n) match {
      case (_, k, n) if n < k => Nil
      case (l, _, `k`) => List(l)
      case (l, 1, n) => l map (List(_))
      case (h +: t, k, n) =>
        combinations(t, k, n - 1) ++ combinations(t, k - 1, n - 1).map(h +: _)
      case (Nil, _, _) => Nil
    }

  def combinations[A](list: List[A], k: Int): List[List[A]] =
    combinations(list, k, length(list))

  def separate[A](indices: List[Int], list: List[A]): (List[A], List[A]) =
    indices match {
      case h +: t =>
        removeAt(h, list) match {
          case (rest, a) =>
            separate(t.map(_ - 1), rest) match {
              case (sep, rest) => (a +: sep, rest)
            }
        }
      case Nil => (Nil, list)
    }


  // P27 a) (**) Group the elements of a set into disjoint subsets.
  def group3[A](list: List[A]): List[List[List[A]]] =
    for {
      c <- combinations(range(0, length(list) - 1), 2)
      (first, restFirst) = separate(c, list)
      c2 <- combinations(range(0, length(restFirst) - 1), 3)
      (second, restSecond) = separate(c2, restFirst)
      third <- combinations(restSecond, length(restSecond))
    } yield List(first, second, third)

  // P27 b) (**) Group the elements of a set into disjoint subsets.
  def group[A](groups: List[Int], list: List[A]): List[List[List[A]]] =
    (groups, length(list)) match {
      case (h +: Nil, len) => List(combinations(list, h, len))
      case (h +: t, len) =>
        for {
          c <- combinations(range(0, len - 1), h)
          (g, rest) = separate(c, list)
          u <- for (l <- group(t, rest)) yield g +: l
        } yield u
      case (Nil, 0) => Nil
    }

  // P28 a) (**) Sorting a list of lists according to length of sublists.
  def lsort[A](list: List[List[A]]) = list sortBy length

  // P28 b) (**) Sorting a list of lists according to length of sublists.
  def lsortFreq[A](list: List[List[A]]): List[List[A]] = {
    val freqs = list groupBy length mapValues length
    list.sortBy(l => freqs(length(l)))
  }
}

object ListsTest extends App {

  import Lists._

  val list = List(1, 1, 2, 3, 5, 8)

  assert(last(list) == 8)
  assert(penultimate(list) == 5)
  assert(nth(2, list) == 2)
  assert(length(list) == 6)
  assert(reverse(list) == list.reverse)
  assert(isPalindrome(List(1, 2, 3, 2, 1)))
  assert(isPalindrome(List(1, 2, 3, 3, 2, 1)))
  assert(!isPalindrome(List(1, 2, 3)))
  assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == list)
  assert(
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      == List('a, 'b, 'c, 'a, 'd, 'e))
  assert(
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  assert(
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  assert(
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      == List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  assert(
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
      == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  assert(
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  assert(duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  assert(
    duplicateN(3, List('a, 'b, 'c, 'c, 'd))
      == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  assert(
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  assert(
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
  assert(
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  assert(
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd), 'b))
  assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
  assert(range(4, 9) == List(4, 5, 6, 7, 8, 9))

  def rng = new Random(101)

  assert(randomSelect(rng)(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)) == List('b, 'g, 'f))
  assert(lotto(rng)(6, 49) == List(2, 12, 10, 40, 29, 13))
  assert(randomPermute(rng)(List('a, 'b, 'c, 'd, 'e, 'f)) == List('e, 'a, 'c, 'd, 'b, 'f))
  assert(
    combinations(range(1, 5), 3, 5)
      ==
      List(
        List(3, 4, 5), List(2, 4, 5), List(2, 3, 4), List(2, 3, 5), List(1, 4, 5),
        List(1, 3, 4), List(1, 3, 5), List(1, 2, 3), List(1, 2, 4), List(1, 2, 5)
      )
  )
  assert(separate(List(0, 3, 5), range(0, 10)) == (List(0, 3, 5), List(1, 2, 4, 6, 7, 8, 9, 10)))
  assert(
    group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).take(5)
      ==
      List(
        List(List("Hugo", "Ida"), List("Evi", "Flip", "Gary"), List("Aldo", "Beat", "Carla", "David")),
        List(List("Hugo", "Ida"), List("David", "Flip", "Gary"), List("Aldo", "Beat", "Carla", "Evi")),
        List(List("Hugo", "Ida"), List("David", "Evi", "Flip"), List("Aldo", "Beat", "Carla", "Gary")),
        List(List("Hugo", "Ida"), List("David", "Evi", "Gary"), List("Aldo", "Beat", "Carla", "Flip")),
        List(List("Hugo", "Ida"), List("Carla", "Flip", "Gary"), List("Aldo", "Beat", "David", "Evi"))
      )
  )
  assert(
    group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).take(5)
      ==
      List(
        List(List("Hugo", "Ida"), List("Flip", "Gary"), List("Aldo", "Beat", "Carla", "David", "Evi")),
        List(List("Hugo", "Ida"), List("Evi", "Flip"), List("Aldo", "Beat", "Carla", "David", "Gary")),
        List(List("Hugo", "Ida"), List("Evi", "Gary"), List("Aldo", "Beat", "Carla", "David", "Flip")),
        List(List("Hugo", "Ida"), List("David", "Evi"), List("Aldo", "Beat", "Carla", "Flip", "Gary")),
        List(List("Hugo", "Ida"), List("David", "Flip"), List("Aldo", "Beat", "Carla", "Evi", "Gary"))
      )
  )
  assert(lsort(
    List(
      List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e),
      List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    ==
    List(
      List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c),
      List('f, 'g, 'h), List('i, 'j, 'k, 'l))
  )
  assert(lsortFreq(
    List(
      List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e),
      List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    ==
    List(
      List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h),
      List('d, 'e), List('d, 'e), List('m, 'n))
  )

  println("Finished")
}
