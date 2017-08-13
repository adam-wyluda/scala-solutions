package category

import fpinscala.monoids.Monoid

trait Category[F[_, _]] {
  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]
  def identity[A]: F[A, A]
}

object FunctionCategory extends Category[Function1] {
  def compose[A, B, C](f: B => C, g: A => B): A => C = f compose g
  def identity[A] = a => a
}

//object ObjectCategory extends Category[<:<] {
//  def compose[A, B, C](f: B <:< C, g: A <:< B): A <:< C = implicitly[A <:< C]
//  def identity[A] = implicitly[A <:< A]
//}

object Category {
  def monoidCategory[M](m: Monoid[M]) =
    new Category[({type L[A, B] = M})#L] {
      def compose[A, B, C](f: M, g: M) = m.op(f, g)
      def identity[A] = m.zero
    }
}
