import Chapter3.MyFunctor
import Chapter4.MyMonad

/** Monad transformer */
object Chapter5 {
  import cats.Monad
  import cats.data.OptionT
  import cats.instances.list._
  import cats.syntax.applicative._

  type ListOption[A] = OptionT[List, A] // Equivalent to List[Option[A]]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]


  val result3: ListOption[Int] =
    for {
      r1 <- result1
      r2 <- result2
    } yield r1 + r2

  final case class MyOptionT[F[_], A](value: F[Option[A]]) {
    def map[B](f: A => B)(implicit F: MyFunctor[F]): MyOptionT[F, B] =
      MyOptionT(F.fmap(value)(opt => opt.map(f)))

    def flatMap[B](f: A => MyOptionT[F, B])(implicit M: MyMonad[F]): MyOptionT[F, B] = {
      flatMapF(a => f(a).value)
    }

    def flatMapF[B](f: A => F[Option[B]])(implicit M: MyMonad[F]): MyOptionT[F, B] = {
      val empty = M.pure[Option[B]](None)
      MyOptionT(M.flatMap(value)(opt => opt.fold(empty)(f)))
    }

    def fold[B](default: B)(f: A => B)(implicit F: MyFunctor[F]): F[B] =
      F.fmap(value)(opt => opt.fold(default)(f))
  }

}
