package fpinscala.monads

import fpinscala.parallel.Par
import fpinscala.parser.TheParser
import fpinscala.parser.TheParserResult.Parser
import fpinscala.propertytesting.proptests.Gen
import fpinscala.pure.State

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left.apply)
      case Right(fb) => map(fb)(Right.apply)
    }
}

object Functor {
  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B) = fa map f
  }
}

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma match {
      case Nil => unit(Nil)
      case h +: t => map2(h, sequence(t))(_ +: _)
    }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))

  def replicateM[A](n: Int, m: F[A]): F[List[A]] =
    map(m)(List.fill(n)(_))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)(Tuple2.apply)

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    map(traverse(ms)(f)) { bList =>
      ms.zip(bList).flatMap {
        case (a, true) => List(a)
        case (a, false) => Nil
      }
    }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapC[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose[F[A], A, B](identity, f)(fa)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  def flatMapJ[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  def composeJ[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A) = Gen.unit(a)
    def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]) = fa.flatMap(f)
  }

  val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.lazyUnit(a)
    def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]) = Par.flatMap(fa)(f)
  }

  val parserMonad = new Monad[Parser] {
    def unit[A](a: => A) = TheParser.succeed(a)
    def flatMap[A, B](fa: Parser[A])(f: (A) => Parser[B]) = TheParser.flatMap(fa)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]) = fa.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]) = fa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A, B](fa: List[A])(f: (A) => List[B]) = fa.flatMap(f)
  }

  def stateMonad[S] = new Monad[({ type L[X] = State[S, X] }) # L] {
    def unit[A](a: => A) = State.unit(a)
    def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]) = fa.flatMap(f)
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] }) # f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](fa: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] =
      Reader(r => f(fa.run(r)).run(r))
  }
}

object Monads extends App {
  import Monad._

  println(listMonad.filterM(List(1, 2, 3))(a => Nil))
  println(listMonad.filterM(List(1, 2, 3))(a => List(true)))
  println(listMonad.filterM(List(1, 2, 3))(a => List(false)))
  println(listMonad.filterM(List(1, 2, 3))(a => List(true, true)))
  println(listMonad.filterM(List(1, 2, 3))(a => List(true, false)))
  println(listMonad.filterM(List(1, 2, 3))(a => List(false, true)))
  println(listMonad.filterM(List(1, 2, 3))(a => List(false, false)))

  println(optionMonad.filterM(List(1, 2, 3))(a => None))
  println(optionMonad.filterM(List(1, 2, 3))(a => Some(false)))
  println(optionMonad.filterM(List(1, 2, 3))(a => Some(true)))

  import State._

  val id =
    for {
      a <- Id("Hello, ")
      b <- Id("monad!")
    } yield a + b
  println(id.value)

  val add = State.modify((_: Int) + 10).flatMap(_ => State.get)
  val multiply = State.modify((_: Int) * 2).flatMap(_ => State.get)
  val result = stateMonad.map2(add, multiply)(_ + _)
  println(result.run(10))

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) =>
      for {
        xs <- acc
        n <- get
        _ <- set(n + 1)
      } yield (n, a) +: xs).run(0)._1.reverse
}
