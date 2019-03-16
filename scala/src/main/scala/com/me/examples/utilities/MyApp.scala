package com.me.examples.utilities

import org.apache.hadoop.hbase.HBaseConfiguration
import org.apache.hadoop.hbase.client.{Connection, ConnectionFactory}

import scala.language.higherKinds
import scala.util.Try

object MyApp {

  trait App[F[_]] {
    def finish[A](a: A): F[A]
    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]
    def map[A, B](fa: F[A], fab: A => B): F[B]
  }

  object App {
    def apply[F[_]](implicit F: App[F]): App[F] = F
  }

  implicit class AppSyntax[F[_]: App, A](fa: F[A]) {
    def map[B](f: A => B): F[B] = App[F].map(fa, f)
    def flatMap[B](afb: A => F[B]): F[B] = App[F].chain(fa, afb)
  }

  trait HBaseConnection[F[_]] {
    def getConnection(zkNode: String, zkPort: String, zkQuorum: String): F[Connection]
  }

  object HBaseConnection {
    def apply[F[_]](implicit F: HBaseConnection[F]): HBaseConnection[F] = F
  }

  final case class Export(conf: Map[String, String]) {

  }

  object Export {
    def apply(conf: Map[String, String]): Export = new Export(conf)

    val ExportApp = new App[Try] {

      override def finish[A](a: A): Try[A] = ???

      override def chain[A, B](fa: Try[A], afb: A => Try[B]): Try[B] = ???

      override def map[A, B](fa: Try[A], fab: A => B): Try[B] = ???
    }
  }

  implicit val TryConnection = new HBaseConnection[Try[Connection]] {
    override def getConnection(zkNode: String, zkPort: String, zkQuorum: String): Try[Connection] = Try {
      val conf = HBaseConfiguration.create()
      ConnectionFactory.createConnection(conf)
    }
  }

}
