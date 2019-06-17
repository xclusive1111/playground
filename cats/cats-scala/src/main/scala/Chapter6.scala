import Chapter2.MyMonoid
import Types.{MyInvalid, MyValid, MyValidated}

/**
  * Semigroupal & Applicative
  */
object Chapter6 {

  /**
    * Semigroupal allows to combine two independent contexts of the same kind.
    * Unlike Functor & Monad, which make the assumption that each computation is dependent on the previous one.
    * By using Semigroupal, two contexts can be computed independently
    * and combined in either order before passing to #product
    *
    * Note: Semigroup allows to join two values, Semigroupal allows to join two contexts.
    *
    * @tparam F Type of the context
    */
  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  /**
    * Define interface using implicit object */
  object MySemigroupal {
    def apply[F[_]](implicit S: MySemigroupal[F]): MySemigroupal[F] = S
    def product[F[_], A, B](fa: F[A], fb: F[B])(implicit S: MySemigroupal[F]): F[(A, B)] = S.product(fa, fb)
  }

  /**
    * Define instances */
  object SemigroupalInstances {
    implicit val optionSemigroupal: MySemigroupal[Option] = new MySemigroupal[Option] {
      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = (fa, fb) match {
        case (Some(a), Some(b)) => Some(a, b)
        case _ => None
      }
    }

    implicit def eitherSemigroupal[E](implicit M: MyMonoid[E]): MySemigroupal[Either[E, ?]] = new MySemigroupal[Either[E, ?]] {
      override def product[A, B](fa: Either[E, A], fb: Either[E, B]): Either[E, (A, B)] = (fa, fb) match {
        case (Right(v1), Right(v2)) => Right((v1, v2))
        case (Right(v1), Left(e1))  => Left(e1)
        case (Left(e1), Right(v1))  => Left(e1)
        case (Left(e1), Left(e2))   => Left(M.combine(e1, e2))
      }
    }

    implicit def validatedSemigroupal[E](implicit M: MyMonoid[E]): MySemigroupal[MyValidated[E, ?]] = new MySemigroupal[MyValidated[E, ?]] {
      override def product[A, B](fa: MyValidated[E, A], fb: MyValidated[E, B]): MyValidated[E, (A, B)] = (fa, fb) match {
        case (MyInvalid(e1), MyInvalid(e2)) => MyInvalid(M.combine(e1, e2))
        case (MyValid(a1), MyValid(a2))     => MyValid((a1, a2))
        case (MyInvalid(e1), MyValid(a1))   => MyInvalid(e1)
        case (MyValid(a1), MyInvalid(e1))   => MyInvalid(e1)
      }
    }
  }

  object ValidatedSyntax {
    implicit class ValidatedOps[A](value: A) {
      def valid[X]: MyValidated[X, A] = MyValid(value)
      def invalid[X]: MyValidated[A, X] = MyInvalid(value)
    }

    def tupled2[E, A, B](t2: (MyValidated[E, A], MyValidated[E, B]))
                        (implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, (A, B)] =
      S.product(t2._1, t2._2)

    def map2[E, A, B, C](t2: (MyValidated[E, A], MyValidated[E, B]))
                        (f: (A, B) => C)
                        (implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, C] =
      S.product(t2._1, t2._2) match {
        case MyValid(value) => MyValid(f(value._1, value._2))
        case MyInvalid(err) => MyInvalid(err)
      }


    def tupled3[E, A, B, C](t3: (MyValidated[E, A], MyValidated[E, B], MyValidated[E, C]))
                           (implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, (A, B, C)] =
      t3 match {
        case (fa, fb, fc) =>
          val fab = tupled2(fa, fb)
          tupled2(fab, fc) match {
            case MyValid(((a, b), c)) => MyValid((a, b, c))
            case MyInvalid(e)         => MyInvalid(e)
          }
      }

    def map3[E, A, B, C, D](t3: (MyValidated[E, A], MyValidated[E, B], MyValidated[E, C]))
                           (f: (A, B, C) => D)
                           (implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, D] =
      tupled3(t3) match {
        case MyValid(value) => MyValid(f(value._1, value._2, value._3))
        case MyInvalid(err) => MyInvalid(err)
      }


    def tupled4[E, A, B, C, D](t4: (MyValidated[E, A], MyValidated[E, B], MyValidated[E, C], MyValidated[E, D]))
                              (implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, (A, B, C, D)] =
      t4 match {
        case (fa, fb, fc, fd) =>
          val fabc = tupled3(fa, fb, fc)
          tupled2(fabc, fd) match {
            case MyValid(((a, b, c), d)) => MyValid((a, b, c, d))
            case MyInvalid(e) => MyInvalid(e)
          }
      }

    def map4[E, A, B, C, D, X](t4: (MyValidated[E, A], MyValidated[E, B], MyValidated[E, C], MyValidated[E, D]))
                              (f: (A, B, C, D) => X)
                              (implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, X] =
      tupled4(t4) match {
        case MyValid(value) => MyValid(f(value._1, value._2, value._3, value._4))
        case MyInvalid(err) => MyInvalid(err)
      }

    implicit class Tuple2Ops[E, A, B](t2: (MyValidated[E, A], MyValidated[E, B])) {
      def tupled(implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, (A, B)] = tupled2(t2)
      def mapN[C](f: (A, B) => C)(implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, C] = map2(t2)(f)
    }

    implicit class Tuple3Ops[E, A, B, C](t3: (MyValidated[E, A], MyValidated[E, B], MyValidated[E, C])) {
      def tupled(implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, (A, B, C)] = tupled3(t3)
      def mapN[D](f: (A, B, C) => D)(implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, D] = map3(t3)(f)
    }

    implicit class Tuple4Ops[E, A, B, C, D](t4: (MyValidated[E, A], MyValidated[E, B], MyValidated[E, C], MyValidated[E, D])) {
      def tupled(implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, (A, B, C, D)] = tupled4(t4)
      def mapN[X](f: (A, B, C, D) => X)(implicit S: MySemigroupal[MyValidated[E, ?]]): MyValidated[E, X] = map4(t4)(f)
    }

    implicit class ValidatedFromEither[E, A](either: Either[E, A]) {
      def toValidated: MyValidated[E, A] = either match {
        case Left(e)  => MyInvalid(e)
        case Right(v) => MyValid(v)
      }
    }
  }

}
