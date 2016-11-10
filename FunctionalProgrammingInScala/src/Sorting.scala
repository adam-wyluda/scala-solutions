package sorting

import fpinscala.datastructures._
import List._
import Tree._

/**
  * @author Adam Wyłuda
  */
object Sorting {
  def sort(list: List[Double]): List[Double] =
    list match {
      case Nil => Nil
      case Cons(head, Nil) => list
      case Cons(head, tail) => {
        val sorted = sort(tail)
        sorted match {
          case Cons(sortedHead, sortedTail) => {
            if (head < sortedHead) Cons(head, sorted)
            else {
              val newList = Cons(sortedHead, Cons(head, sortedTail))
              sort(newList)
            }
          }
          // In that case this assertion might be as well true
          case Nil => assert(true == false); Nil
        }
      }
    }

//  def sortedTree[A: Ordering](list: List[A]): Tree[A] =
//    list match {
//      case Cons()
//    }
//
//  def appendSorted[A: Ordering](tree: Tree[A], el: A): Tree[A] =
//    tree match {
//      case Leaf(x) if (implicitly[Ordering[A]].lt(el, x)) => Branch(Leaf(el), Leaf(x))
//      case Leaf(x) => Branch(Leaf(x), Leaf(el))
//      case Branch(left, right)
//    }
}

object SortingMain extends App {

  import Sorting._

  val list = List(5.0, -3.5, -1.5, 2.3, 4.3, -6.7, 10.0, -8.0, 0.0, 2.0, 0.0, 3.1, 5.2, -1.3, 2.0)
  println(string(list))

//  val stree = sortedTree(list)
//  print(stree)

  val sorted = Sorting.sort(list)
  println(string(sorted))
}
