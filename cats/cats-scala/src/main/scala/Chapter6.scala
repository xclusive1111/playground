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
  }

}
