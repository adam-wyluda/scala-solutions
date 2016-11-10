package main

import scala.annotation.tailrec

/**
  * @author Adam Wyłuda
  */
object Main extends App {

  def fibIter(n: Int): Int = {
    var a, b = 1
    1 until n foreach { _ =>
      var c = b
      b = a + b
      a = c
    }
    b
  }

  def fib(n: Int): Int = {
    @tailrec
    def fib(a: Int, b: Int, n: Int, sum: Int): Int = if (n > 1) fib(b, a + b, n - 1, sum + a) else sum
    fib(1, 1, n, 1)
  }

  def iterate[U](msg: String, start: Int, end: Int, fun: Int => U): Unit = {
    start until end foreach (i => println(s"$msg: $i => ${fun(i)}"))
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  iterate("fibIter", 0, 10, fibIter)
  iterate("fib", 0, 10, fib)

  def add(a: Int, b: Int) = a + b
  def inc = partial1(1, add)

  iterate("inc", 0, 5, inc)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => (b => f(a, b))

  def curr = curry(add)
  def incurr = curr(1)

  iterate("incurr", 0, 5, incurr)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def uncurr = uncurry(curr)

  iterate("uncurr", 0, 5, i => uncurr(i, i))

  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))
}
