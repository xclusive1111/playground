import org.scalatest.FunSuite
import zio.{DefaultRuntime, Task, ZIO}

final case class UserProfile(id: Int, name: String)

trait UserRepo {
  def lookup(id: Int): Task[UserProfile]
  def update(id: Int, profile: UserProfile): Task[Unit]
}

object UserRepoLive extends UserRepo {
  override def lookup(id: Int): Task[UserProfile] =
    Task(UserProfile(1, "sondv"))

  override def update(id: Int, profile: UserProfile): Task[Unit] =
    Task.effect { println("Updated!") }
}

object UserRepoTest extends UserRepo {
  private var map: Map[Int, UserProfile] = Map(1 -> UserProfile(1, "Foo"))

  override def lookup(id: Int): Task[UserProfile] =
    Task(map(id))

  override def update(id: Int, profile: UserProfile): Task[Unit] =
    Task.effect { map = map + (id -> profile) }
}

object UserService {
  def lookup(id: Int): ZIO[UserRepo, Throwable, UserProfile] =
    ZIO.accessM(_.lookup(id))

  def update(id: Int, profile: UserProfile): ZIO[UserRepo, Throwable, Unit] =
    ZIO.accessM(_.update(id, profile))
}

class TestingEffects extends FunSuite {
  val runtime: DefaultRuntime = new DefaultRuntime {}

  test("lookup") {
    println("Live: " + runtime.unsafeRun(UserService.lookup(1).provide(UserRepoLive)))
    println("Test: " + runtime.unsafeRun(UserService.lookup(1).provide(UserRepoTest)))
  }

}
