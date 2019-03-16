package com.me.examples.control

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => fmap(fa)(f)

  def as[A, B](fa: F[A], f: => B): F[B] =
    fmap(fa)(_ => f)

  def void[A](fa: F[A]): F[Unit] =
    as(fa, ())
}

trait FunctorLaws {
  def identity[F[_], A](fa: F[A])(implicit F: Functor[F]) =
    F.fmap(fa)(a => a) == fa

  // If f :: A => B and g :: B => C then h :: A => C == g . f
  def composition[F[_], A, B, C](fa: F[A], f: A => B, g: B => C)(implicit F: Functor[F]) =
    F.fmap(F.fmap(fa)(f))(g) == F.fmap(fa)(f andThen g)
}

object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

}