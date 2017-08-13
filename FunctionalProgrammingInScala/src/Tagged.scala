package tagged

object Tags {
  type Tagged[A] = AnyRef { type T = A }
  type @@[A, B] = A with Tagged[B]

  object Tag {
    @inline def apply[A, T](a: A): A @@ T = a.asInstanceOf[A @@ T]
  }

  sealed trait Multiplication

  def Multiplication[A](a: A): A @@ Multiplication = Tag[A, Multiplication](a)
}
