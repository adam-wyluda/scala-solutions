package fpinscala.errors

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  def foreach[U](f: A => U): Unit
}

case class Some[+A](get: A) extends Option[A] {
  def map[B](f: (A) => B): Option[B] = Some(f(get))
  def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)
  def filter(f: (A) => Boolean): Option[A] = if (f(get)) Some(get) else None
  def getOrElse[B >: A](default: => B): B = get
  def orElse[B >: A](ob: => Option[B]): Option[B] = this
  def foreach[U](f: (A) => U): Unit = f(get)
}

case object None extends Option[Nothing] {
  def map[B](f: (Nothing) => B): Option[B] = None
  def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None
  def filter(f: (Nothing) => Boolean): Option[Nothing] = None
  def getOrElse[B >: Nothing](default: => B): B = default
  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  def foreach[U](f: (Nothing) => U): Unit = ()
}

object Option {
  def call[A, R](args: Seq[A])(funs: Seq[(String, A => R)])(
      msg: (A, R) => String): Unit =
    for {
      (name, fun) <- funs
      e <- Seq(
              Seq(Right(s"------- $name -------")),
              args.map(Left(_)),
              Seq(Right(""))
          ).flatten
      txt = e.fold(arg => msg(arg, fun(arg)), identity)
    } println(txt)

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def varianceFor(xs: Seq[Double]): Option[Double] =
    for {
      m <- mean(xs)
      d = for (x <- xs) yield math.pow(x - m, 2)
      r <- mean(d)
    } yield r

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2FlatMap[A, B, R](aOpt: Option[A], bOpt: Option[B])(
      f: (A, B) => R): Option[R] =
    aOpt flatMap (a => bOpt map (b => f(a, b)))

  def map2[A, B, R](aOpt: Option[A], bOpt: Option[B])(f: (A,
                                                          B) => R): Option[R] =
    for {
      a <- aOpt
      b <- bOpt
    } yield f(a, b)

  def map3[A, B, C, R](aOpt: Option[A], bOpt: Option[B], cOpt: Option[C])(
      f: (A, B, C) => R): Option[R] =
    for {
      a <- aOpt
      b <- bOpt
      c <- cOpt
    } yield f(a, b, c)

  def sequenceOld[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft(Some(Nil): Option[List[A]])(map2(_, _)(_ :+ _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft(Some(Nil): Option[List[B]])((acc, el) =>
          map2(acc, f(el))(_ :+ _))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}

object Options extends App {

  import Option._

  val a = Seq(1.5, 3.5, 10.0)
  val b = Seq(0.0, 1.0)
  val c = Seq()
  val d = Seq(7.0)

  call(Seq(a, b, c, d))(
      Seq(
          ("mean", mean _),
          ("variance", variance _),
          ("varianceFor", varianceFor _)
      ))(
      (seq, res) => s"[${seq.mkString(", ")}] => ${res}"
  )

  case class Employee(name: String,
                      departament: String,
                      manager: Option[Employee])

  def empName(empOpt: Option[Employee]) = empOpt map (_.name) getOrElse "none"

  def lookupByName(name: String): Option[Employee] =
    name match {
      case "Joe" =>
        Some(
            Employee(name,
                     "Marketing",
                     Some(Employee("Frank", "Marketing", None))))
      case "Joan" => Some(Employee(name, "Sales", None))
      case _ => None
    }

  def getName(employee: Employee) = employee.name
  val liftedName = lift(getName)

  def departament(name: String) = lookupByName(name).map(_.departament)
  def manager(name: String) = lookupByName(name).flatMap(_.manager).map(_.name)
  def depDef(name: String) = departament(name) orElse Some("Default dep.")
  def byLiftedName(name: String) = liftedName(lookupByName(name))

  call(Seq("Joe", "Joan", "Alice"))(
      Seq(
          ("departament", departament _),
          ("manager", manager _),
          ("depDef", depDef _),
          ("byLiftedName", byLiftedName _)
      ))(
      (name, res) => s"$name => $res"
  )

  val optAbs = lift(math.abs(_: Double))
  println(optAbs(Some(3.5)))
  println(optAbs(None))

  println("\nINSURANCES")

  def insuranceRateQuote(age: Int, numberOfTickets: Int): Double =
    age * 3 + numberOfTickets * 10

  def parseInsuranceRateQuote(age: String,
                              numberOfTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def forInsuranceRateQuote(age: String,
                            numberOfTickets: String): Option[Double] =
    for {
      age <- Try(age.toInt)
      tickets <- Try(numberOfTickets.toInt)
    } yield insuranceRateQuote(age, tickets)

  println(parseInsuranceRateQuote("123", "32"))
  println(parseInsuranceRateQuote("123a", "32"))
  println(parseInsuranceRateQuote("123", "3b2"))
  println(parseInsuranceRateQuote("##", "z"))

  println(forInsuranceRateQuote("123", "32"))
  println(forInsuranceRateQuote("123a", "32"))
  println(forInsuranceRateQuote("123", "3b2"))
  println(forInsuranceRateQuote("##", "z"))

  println("\nSequence")
  val opts = List(Some(1), Some(2), Some(3))
  val opts2 = List(Some(1), None, Some(5))
  println(sequence(opts))
  println(sequence(opts2))

  def parseInts(list: List[String]) = traverse(list)(str => Try(str toInt))

  println(parseInts(List("123", "346")))
  println(parseInts(List("123", "z346")))
}
