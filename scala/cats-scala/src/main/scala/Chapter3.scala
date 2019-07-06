import Types._
import cats.Functor
import cats.instances.function._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._

import scala.concurrent.{ExecutionContext, Future}

object Chapter3 {

  object Example {
    val toDouble: Int => Double = _.toDouble
    val plus2: Double => Double = _ * 2

    val toDoubleAndPlus2: Int => Double = toDouble map plus2 // composition using map, just like Functor
    val _toDoubleAndPlus2: Int => Double = plus2.compose(toDouble) // built-in way to compose

    val shouldBeTrue: Boolean = toDoubleAndPlus2(1) == _toDoubleAndPlus2(1)

    val list1: List[Int]    = List(1, 2, 3)
    val list2: List[Double] = Functor[List].map(list1)(toDoubleAndPlus2)

    val opt1: Option[Int]    = Option(123)
    val opt2: Option[Double] = Functor[Option].map(opt1)(toDouble)


    // Functor also provides lift method
    val liftedFunc: Option[Int] => Option[Double] = Functor[Option].lift(toDoubleAndPlus2)
    liftedFunc(Option(1)) // Option[Double] = Some(2.0)
  }

  /**
    * Define type class */
  trait MyFunctor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  /**
    * Define interface using implicit object
    */
  object MyFunctor {
    def apply[F[_]](implicit functor: MyFunctor[F]): MyFunctor[F] = functor
    def fmap[F[_], A, B](fa: F[A])(f: A => B)(implicit functor: MyFunctor[F]): F[B] = functor.fmap(fa)(f)
  }

  /**
    * Define interface using extension methods
    */
  object MyFunctorSyntax {
    implicit class MyFunctorOps[F[_]: MyFunctor, A](fa: F[A]) {
      def fmap[B](fab: A => B): F[B] = MyFunctor[F].fmap(fa)(fab)
    }
  }

  /**
    * Define interface using extension methods */
  object MyFunctorSyntax2 {
    implicit class MyFunctorOps[F[_], A](source: F[A]) {
      def fmap[B](f: A => B)(implicit functor: MyFunctor[F]): F[B] = functor.fmap(source)(f)
    }
  }

  /**
    * Define instances */
  object MyFunctorInstances {
    implicit val boxFunctor: MyFunctor[Box] = new MyFunctor[Box] {
      override def fmap[A, B](fa: Box[A])(f: A => B): Box[B] = fa match {
        case FullBox(value) => FullBox(f(value))
        case EmptyBox => EmptyBox
      }
    }

    implicit val optionFunctor: MyFunctor[Option] = new MyFunctor[Option] {
      override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    }

    implicit val listFunctor: MyFunctor[List] = new MyFunctor[List] {
      override def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

    implicit val seqFunctor: MyFunctor[Seq] = new MyFunctor[Seq] {
      override def fmap[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
    }

    implicit def futureFunctor(implicit ex: ExecutionContext): MyFunctor[Future] = new MyFunctor[Future] {
      override def fmap[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    }

    implicit val treeFunctor: MyFunctor[Tree] = new MyFunctor[Tree] {
      override def fmap[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(left, right) => Branch(fmap(left)(f), fmap(right)(f))
        case Leaf(value)         => Leaf(f(value))
      }
    }
  }



}
