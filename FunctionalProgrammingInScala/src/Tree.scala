package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/**
  * @author Adam Wyłuda
  */
object Tree {
  def size(tree: Tree[_]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(x) => x
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  def depth(tree: Tree[_]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left) max depth(right)
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](tree: Tree[A])(z: B)(f: (B, A) => B): B =
    tree match {
      case Leaf(x) => f(z, x)
      case Branch(left, right) => {
        val foldLeft = fold(left)(z)(f)
        fold(right)(foldLeft)(f)
      }
    }

  def reduce[A](tree: Tree[A])(f: (A, A) => A): A =
    tree match {
      case Leaf(x) => x
      case Branch(left, right) => f(reduce(left)(f), reduce(right)(f))
    }

  def fromList[A](list: List[A]): Tree[A] =
    list match {
      case Cons(head, Nil) => Leaf(head)
      case Cons(head, tail) => Branch(Leaf(head), fromList(tail))
    }

  def toStr(tree: Tree[_]): String =
    tree match {
      case Leaf(x) => s"Leaf($x)"
      case Branch(left, right) => s"Branch:"
    }

  def print(tree: Tree[_], indent: Int = 0, indentChars: Int = 2): Unit = {
    println(" " * indentChars * indent + toStr(tree))
    tree match {
      case Branch(left, right) => {
        print(left, indent + 1)
        print(right, indent + 1)
      }
      case _ => ()
    }
  }
}

object TreeMain extends App {

  import Tree._

  val tree =
    Branch(
      Branch(
        Leaf(9),
        Branch(
          Leaf(2),
          Leaf(7)
        )
      ),
      Branch(
        Leaf(0),
        Leaf(5)
      )
    )

  print(tree)
  println(size(tree))
  println(maximum(tree))
  println(depth(tree))
  print(map(tree)(_ * 2))

  def fsum(x: Int, y: Int) = x + y

  val sum = fold(tree)(0)(fsum)
  println(s"sum = $sum")
  val sumR = reduce(tree)(fsum)
  println(s"sumR = $sumR")

  val list = List(2, 3, 5, 7)
  val fl = fromList(list)
  print(fl)

  assert(reduce(fl)(fsum) == List.sum(list))
}
