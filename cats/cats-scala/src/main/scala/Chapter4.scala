import Chapter2.MyMonoid
import Chapter3.MyFunctor
import Types.{Box, EmptyBox, FullBox, Id}

import scala.util.{Success, Try}

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
  }

  /**
    * Define interface using implicit object
    */
  object MyMonad {
    def apply[F[_]](implicit m: MyMonad[F]): MyMonad[F] = m
    def pure[F[_], A](value: A)(implicit m: MyMonad[F]): F[A] = m.pure(value)
    def flatMap[F[_], A, B](fa: F[A])(f: A => F[B])(implicit m: MyMonad[F]): F[B] = m.flatMap(fa)(f)
    def map[F[_], A, B](fa: F[A])(f: A => B)(implicit m: MyMonad[F]): F[B] = m.fmap(fa)(f)
  }

  /**
    * Define interface using extension methods
    */
  object MyMonadSyntax {
    implicit class MyMonadOps[F[_], A](fa: F[A]) {
      def flatMap[B](fab: A => F[B])(implicit M: MyMonad[F]): F[B] = M.flatMap(fa)(fab)
      def map[B](fab: A => B)(implicit M: MyMonad[F]): F[B] = M.fmap(fa)(fab)
    }

    implicit class Ops[A](value: A) {
      def pure[F[_]: MyMonad]: F[A] = MyMonad[F].pure(value)
    }
  }

  object MyMonadInstance {
    implicit val optionMonad: MyMonad[Option] = new MyMonad[Option] {
      override def pure[A](value: A): Option[A] = Option(value)

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    implicit val listMonad: MyMonad[List] = new MyMonad[List] {
      override def pure[A](value: A): List[A] = List(value)

      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }

    implicit val boxMonad: MyMonad[Box] = new MyMonad[Box] {
      override def pure[A](value: A): Box[A] = if (value == null) EmptyBox else FullBox(value)

      override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] = fa match {
        case EmptyBox => EmptyBox
        case FullBox(value) => f(value)
      }
    }

    implicit val idMonad: MyMonad[Id] = new MyMonad[Id] {
      override def pure[A](value: A): Id[A] = value

      override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    }

    implicit val tryMonad: MyMonad[Try] = new MyMonad[Try] {
      override def pure[A](value: A): Try[A] = Success(value)

      override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
    }

    implicit def eitherMonad[E]: MyMonad[Either[E, ?]] = new MyMonad[Either[E, ?]] {
      override def pure[A](value: A): Either[E, A] = Right(value)

      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
    }

    implicit def writerMonad[W: MyMonoid]: MyMonad[MyWriter[W, ?]] = new MyMonad[MyWriter[W, ?]] {
      override def pure[A](value: A): MyWriter[W, A] = MyWriter(MyMonoid[W].empty, value)

      override def flatMap[A, B](fa: MyWriter[W, A])(f: A => MyWriter[W, B]): MyWriter[W, B] = fa.bind(f)
    }

    implicit def readerMonad[DEP]: MyMonad[MyReader[DEP, ?]] = new MyMonad[MyReader[DEP, ?]] {
      override def pure[A](value: A): MyReader[DEP, A] = MyReader(_ => value)

      override def flatMap[A, B](fa: MyReader[DEP, A])(f: A => MyReader[DEP, B]): MyReader[DEP, B] = fa.bind(f)
    }
  }

  /**
    * Define a Writer monad.
    * A Writer is a data structure that allows to carry a log along with a computation.
    * A Writer can be used to record messages, errors or additional data about a computation,
    * and extract a log alongside the final result.
    *
    * @tparam W type of a `log`
    * @tparam A type of a value
    */
  final case class MyWriter[W, A](written: W, value: A) {
    def run: (W, A) = (written, value)

    def fmap[B](fn: A => B): MyWriter[W, B] =
      MyWriter(written, fn(value))

    // Use `bind` intentionally instead of `flatMap`, just for fun :)
    def bind[B](fn: A => MyWriter[W, B])(implicit monoid: MyMonoid[W]): MyWriter[W, B] =
      fn(value) match {
        case MyWriter(xs, b) => MyWriter(MyMonoid[W].combine(written, xs), b)
      }

    def reset(implicit monoid: MyMonoid[W]): MyWriter[W, A] =
      MyWriter(MyMonoid[W].empty, value)
  }


  /**
    * Define a Reader monad.
    * A Reader is a data structure that wrap up a function of one argument
    * and allows to sequence operations that depend on some input.
    *
    * One common use case for Reader is dependency injection. If we have a number
    * of operations that all depend on some external configuration, we can chain
    * them together using a Reader to produce one large operation that accept the
    * configuration as a parameter and runs our program in the order specified.
    *
    * @param run A function to be wrapped.
    * @tparam A type of the input.
    * @tparam B type of the output.
    */
  final case class MyReader[A, B](run: A => B) {
    /**
      * The `map` method simply extends the computation in the Reader by passing
      * the result of previous computation through a specified function.
      */
    def map[C](f: B => C): MyReader[A, C] =
      MyReader(f.compose(run))

    /**
      * The `flatMap` method allows to combine Readers that depend on the same input type.
      */
    def bind[C](f: B => MyReader[A, C]): MyReader[A, C] =
      MyReader(a => f(run(a)).run(a))

  }
}
