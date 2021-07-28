import cats.effect.IO
import cats.effect.concurrent.Ref

val ref: IO[Ref[IO, Int]] = Ref.of[IO, Int](9)

case class R(ref: Ref[IO, Int]) {
  def update(i: Int) = {
    ref.set(i)
    ref.get
  }
  def get() = {
    ref.get
  }
}

ref.map(x=> R(x)).map(_.update(7)).map(_.map(x => println(x))).unsafeRunSync()