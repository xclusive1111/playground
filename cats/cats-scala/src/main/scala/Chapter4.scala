import Chapter3.MyFunctor
import Types.{Box, FullBox, EmptyBox}

object Chapter4 {

  object Example {
    import cats.Monad
    import cats.instances.list._
    import cats.instances.option._

    val opt1: Option[Int] = Monad[Option].pure(3) // Some(3)
    val opt2: Option[Int] = Monad[Option].flatMap(opt1)(a => Some(a + 5)) // Some(8)
    val opt3: Option[Int] = Monad[Option].map(opt2)(a => 2 * a) // Some(16)
    val list1: List[Int] = Monad[List].pure(3)
    val list2: List[Int] = Monad[List].flatMap(list1)(a => List(a * 3, a))
  }

  /**
    * Define a type class */
  trait MyMonad[F[_]] extends MyFunctor[F] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def fmap[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))

    def empty[A]: F[A]
  }

  /**
    * Define interface using implicit object
    */
  object MyMonad {
    def apply[F[_]](implicit m: MyMonad[F]): MyMonad[F] = m
    def pure[F[_], A](value: A)(implicit m: MyMonad[F]): F[A] = m.pure(value)
    def flatMap[F[_], A, B](fa: F[A])(f: A => F[B])(implicit m: MyMonad[F]): F[B] = m.flatMap(fa)(f)
    def map[F[_], A, B](fa: F[A])(f: A => B)(implicit m: MyMonad[F]): F[B] = m.fmap(fa)(f)
    def empty[F[_], A](implicit m: MyMonad[F]): F[A] = m.empty
  }

  /**
    * Define interface using extension methods
    */
  object MyMonadSyntax {
    implicit class MyMonadOps[F[_], A](fa: F[A]) {
      def bind[B](fab: A => F[B])(implicit M: MyMonad[F]): F[B] = M.flatMap(fa)(fab)
    }

    implicit class Ops[F[_]: MyMonad, A](value: A) {
      def pure(): F[A] = MyMonad[F].pure(value)
    }
  }

  object MyMonadInstance {
    implicit val optionMonad: MyMonad[Option] = new MyMonad[Option] {
      override def pure[A](value: A): Option[A] = Option(value)

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

      override def empty[A]: Option[A] = None
    }

    implicit val listMonad: MyMonad[List] = new MyMonad[List] {
      override def pure[A](value: A): List[A] = List(value)

      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

      override def empty[A]: List[A] = List.empty[A]
    }

    implicit val boxMonad: MyMonad[Box] = new MyMonad[Box] {
      override def pure[A](value: A): Box[A] = if (value == null) EmptyBox else FullBox(value)

      override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = fa match {
        case EmptyBox => EmptyBox
        case FullBox(value) => f(value)
      }

      override def empty[A]: Box[A] = EmptyBox
    }
  }

}
