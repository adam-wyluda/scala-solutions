package fpinscala.pure

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class PseudoRNG(value: Int) extends RNG {
  def nextInt = (value, this)
}

object SimpleRNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (Int.MinValue, rng2) => (0, rng2)
      case (i, rng2) => (math.abs(i), rng2)
    }

  def double(rng: RNG): (Double, RNG) =
    nonNegativeInt(rng) match {
      case (i, rng2) => (i.toDouble / Int.MaxValue, rng2)
    }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count > 0) {
      val (i, rng2) = rng.nextInt
      val (list, rngLast) = ints(count - 1)(rng2)
      (i +: list, rngLast)
    } else (Nil, rng)

  def intsTail(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(c: Int, r: RNG, list: List[Int]): (List[Int], RNG) =
      if (c > 0) {
        val (i, r2) = r.nextInt
        loop(c - 1, r2, i +: list)
      } else (list, r)
    loop(count, rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)(rng)

  def intDoubleMap(rng: RNG): ((Int, Double), RNG) =
    map2(_.nextInt, double)(Tuple2.apply)(rng)

  def doubleIntMap(rng: RNG): ((Double, Int), RNG) =
    map2(double, _.nextInt)(Tuple2.apply)(rng)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)(Tuple2.apply)

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case h +: t => map2(h, sequence(t))(_ +: _)(_)
      case Nil => unit(Nil)
    }

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapFlat[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2Flat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      (f(a), s2)
    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s =>
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    }
}

object State {
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)
  val nonNegativeInt = int map math.abs
  val double = nonNegativeInt map (_.toDouble / Int.MaxValue)

  def id[S] = modify(identity[S])

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] =
    list match {
      case h +: t => h.map2(sequence(t))(_ +: _)
      case Nil => unit(Nil)
    }

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def insert = copy(coins = coins + 1)
  def unlock = copy(locked = false)
  def dispense = copy(candies = candies - 1, locked = true)
  def toPair = (coins, candies)
}

object Machine {
  import State._

  type MachineState = State[Machine, (Int, Int)]

  def simulateMachine(inputs: List[Input]): MachineState =
    sequence(inputs map input2State).map(_.last)

  def insert = modify[Machine](_.insert)
  def unlock = modify[Machine](_.unlock)
  def dispense = modify[Machine](_.dispense)

  def input2State(input: Input) =
    for {
      machine <- get[Machine]
      _ <- interpret(input, machine)
      nextMachine <- get
    } yield (nextMachine.toPair)

  def interpret(input: Input, machine: Machine) =
    (input, machine) match {
      case (Coin, Machine(true, _, _)) =>
        for {
          _ <- insert
          _ <- unlock
        } yield ()
      case (Coin, Machine(false, _, _)) =>
        insert
      case (Turn, Machine(true, _, _)) =>
        id[Machine]
      case (Turn, Machine(false, candies, _)) if candies > 0 =>
        dispense
      case (Turn, Machine(false, _, _)) =>
        id[Machine]
    }
}

object Pure extends App {

  {
    import SimpleRNG._

    def ntimes[T](n: Int)(rng: RNG)(f: Rand[T]): Unit =
      if (n > 0) ntimes(n - 1)(f(rng)._2)(f)
    def print3[T](rng: RNG)(msg: String)(f: Rand[T]): Unit = {
      println(msg)
      ntimes(3)(rng) { next =>
        val result = f(next)
        println(result)
        result
      }
      println
    }

    val rng = SimpleRNG(42)
    print3(rng)("nextInt")(_.nextInt)
    print3(rng)("nonNegativeInt")(nonNegativeInt)
    assert(nonNegativeInt(PseudoRNG(-42))._1 == 42)
    assert(nonNegativeInt(PseudoRNG(Int.MinValue))._1 == 0)
    print3(rng)("double")(double)
    print3(rng)("intDouble")(intDouble)
    print3(rng)("doubleInt")(doubleInt)
    print3(rng)("double3")(double3)
    print3(rng)("ints")(ints(5))
    print3(rng)("intsTail")(intsTail(5))

    val u5 = unit(5)
    println(u5(rng))
    println

    print3(rng)("nonNegativeEven")(nonNegativeEven)
    print3(rng)("doubleMap")(doubleMap)
    print3(rng)("intDoubleMap")(intDoubleMap)
    print3(rng)("doubleIntMap")(doubleIntMap)
    print3(rng)("intsSequence")(intsSequence(5))
    print3(rng)("rollDie")(rollDie)
  }

  println("\n-------------------\n")

  {
    import State._

    val rand: Rand[(Int, Int, Int, Double)] =
      for {
        s <- get
        x <- int
        _ <- set(PseudoRNG(123): RNG)
        y <- int
        _ <- set(s)
        z <- int
        d <- double
      } yield (x, y, z, d)

    val rng = SimpleRNG(42)
    println(double.run(rng))
    println(rand.run(rng))
  }

  println("\n-------------------\n")

  {
    import State._

    val machine = Machine(true, 5, 10)
    val inputs = List.fill(4)(List(Coin, Turn)).flatten
    val state = Machine.simulateMachine(inputs)
    println(state.run(machine))
  }
}
