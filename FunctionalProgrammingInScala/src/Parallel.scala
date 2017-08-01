package fpinscala

import java.util.concurrent.Executors

package object parallel {

  import java.util.concurrent.{CountDownLatch, Executors, Callable, ExecutorService, Future, TimeUnit}
  import java.util.concurrent.atomic.AtomicReference

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean) = false
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      es => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      es => (lazyUnit(ps.map(_.apply(es)).map(_.get)))(es)

    def sortPar(parList: Par[List[Int]]) = map2(parList, unit(()))((a, _) => a.sorted)

    def sortParMap(parList: Par[List[Int]]) = map(parList)(_.sorted)

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
      fork(sequence(ps.map(asyncF(f))))

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
      fork {
        val seq: Par[List[A]] = sequence(as.map(lazyUnit(_)))
        map(seq)(_.filter(f))
      }

    def parFold[A](z: A, seq: IndexedSeq[A])(f: (A, A) => A): Par[A] =
      if (seq.size <= 1) Par.unit(seq.headOption getOrElse z)
      else {
        val (l, r) = seq.splitAt(seq.length / 2)
        map2(fork(parFold(z, l)(f)), fork(parFold(z, r)(f)))(f)
      }

    def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
      map2(map2(pa, pb)(Tuple2.apply), pc) { case ((a, b), c) => f(a, b, c) }

    def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] =
      map2(map2(pa, pb)(Tuple2.apply), map2(pc, pd)(Tuple2.apply)) { case ((a, b), (c, d)) => f(a, b, c, d) }

    def map5[A, B, C, D, E, F](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      map3(map2(pa, pb)(Tuple2.apply), map2(pc, pd)(Tuple2.apply), pe) {
        case ((a, b), (c, d), e) => f(a, b, c, d, e)
      }

    def equal[A](es: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(es).get == p2(es).get

//    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
//      es =>
//        if (run(es)(cond).get) t(es)
//        else f(es)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => choices(run(es)(n).get)(es)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => choices(run(es)(key).get)(es)

    def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      es => choices(run(es)(pa).get)(es)

    def choiceNew[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      flatMap(cond)(if (_) t else f)

    def choiceNNew[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(n)(choices.apply)

    def join[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(identity)

    def joinFlatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      join(map(pa)(choices))

    def map2fm[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
      flatMap(pa) { a =>
        map(pb)(b => f(a, b))
      }

    // join(fork(fork(x))) == fork(x)
    // join(fork(y)) == y
    // join(unit(x)) == x
  }

  object NewPar {
    sealed trait Future[A] {
      private[parallel] def apply(k: A => Unit): Unit
    }

    type Par[A] = ExecutorService => Future[A]

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown }
      latch.await
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })
  }

}


object ParallelMain extends App {

  import parallel._
  import Par._

  //  def sum(ints: IndexedSeq[Int]): Int =
  //    if (ints.size <= 1)
  //      ints.headOption getOrElse 0
  //    else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      val sumL: Par[Int] = Par.unit(sum(l))
  //      val sumR: Par[Int] = Par.unit(sum(r))
  //      Par.get(sumL) + Par.get(sumR)
  //    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def max(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size == 0) Par.unit(Int.MinValue)
    else if (ints.size == 1) Par.unit(ints.head)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(max(l)), Par.fork(max(r)))(_ max _)
    }

  def sumParFold(ints: IndexedSeq[Int]) = parFold(0, ints)(_ + _)
  def maxParFold(ints: IndexedSeq[Int]) = parFold(Int.MinValue, ints)(_ max _)

  val fut = max(Vector(1, 75, 32, 20, 50))(Executors.newCachedThreadPool())
  println(fut.get)

  //    def wordsByParagraph(pars: List[String]): Par[Int] = {
  //      val splits = parMap(pars)(_.split(" ").size)
  //    }

  /**
    * Theorem: map(map(y)(g))(f) == map(y)(f compose g)
    *
    * map(y)(id) == y
    * subs: y = map(a)(f)
    * map(map(a)(f))(id) == map(a)(f)
    * subs: a = map(b)(g compose h)
    * map(map(map(b)(g compose h))(id) == map(map(map(b)(
    */
}
