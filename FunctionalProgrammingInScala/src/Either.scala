package fpinscala.errors

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  def fold[R](l: E => R, r: A => R): R

  def toOption: Option[A]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  def map[B](f: (Nothing) => B): Either[E, B] = this
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing,
                                                B) => C): Either[EE, C] = this
  def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this
  def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  def fold[R](l: E => R, r: Nothing => R): R = l(value)

  def toOption = None
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))
  def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A,
                                                      B) => C): Either[EE, C] =
    flatMap(a => b map (f(a, _)))
  def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] =
    f(value)
  def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
  def fold[R](l: Nothing => R, r: A => R): R = r(value)

  def toOption = Some(value)
}

object Either {
  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def traverse[E, A, B](as: List[A])(
      f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft(Right(Nil): Either[E, List[B]]) { (acc, el) =>
      acc.map2(f(el))(_ :+ _)
    }

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(identity)
}

object Eithers extends App {

  import Either._

  println(safeDiv(10, 4))
  println(safeDiv(3, 0))

  def insuranceRateQuote(age: Int, tickets: Int): Either[Exception, Int] =
    safeDiv(age + tickets, tickets)

  def nonNegative(numb: Int) =
    if (numb >= 0) Right(numb)
    else Left(new IllegalArgumentException(s"$numb is non negative!"))

  def parseInsuranceRateQuote(age: String,
                              tickets: String): Either[Exception, Int] =
    for {
      a <- Try { age.toInt }
      t <- Try { tickets.toInt }
      _ <- nonNegative(a)
      _ <- nonNegative(t)
      q <- insuranceRateQuote(a, t)
    } yield q

  println(parseInsuranceRateQuote("123", "456"))
  println(parseInsuranceRateQuote("123a", "456"))
  println(parseInsuranceRateQuote("123", "456c"))
  println(parseInsuranceRateQuote("123", "0"))
  println(parseInsuranceRateQuote("123", "-20"))
  println(parseInsuranceRateQuote("-123", "-20"))
}
