import cats.effect.IO
import cats.effect.concurrent.Ref

val ref: IO[Ref[IO, Int]] = Ref.of[IO, Int](9)

case class R(ref: Ref[IO, Int]) {
  def update(i: Int): IO[Int] =
    ref.set(i).flatMap(_ =>ref.get)

  def get(): IO[Int] =
    ref.get
}

val r: IO[R] = for {
  r <- ref
} yield R(r)

val r1: IO[Int] = r.flatMap(x => x.get())

r1.unsafeRunSync()

val r2: IO[Int] = r.flatMap(x => x.update(4))

r2.unsafeRunSync()