package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

import scala.concurrent.{ExecutionContext, Future}

object IOParallelism extends IOApp.Simple {

  given ExecutionContext = ExecutionContext.global

  // IOs are usually sequential
  val aniIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")
  val io: IO[Future[Int]] = IO(Future(42))

  val composedIO: IO[String] = for {
    ani    <- aniIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  val composedIO_v2: IO[String] = aniIO.flatMap(ani => kamranIO.map(kamran => s"$ani and $kamran love Rock the JVM"))

  // debug extension method
  import com.rockthejvm.utils.*
  // mapN extension method
  import cats.syntax.apply.*

  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang: IO[String] = IO.delay("Scala")
  val goalInLife: IO[String] = (meaningOfLife.debug, favLang.debug)
    .mapN((num, string) => s"my goal in life is $num and $string")

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)

  import cats.effect.implicits.*
  val goalInLifeParallel: IO.Par[String] = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")

  // turn back to sequential
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand:
  import cats.syntax.parallel.*
  val goalInLife_v3: IO[String] = (meaningOfLife.debug, favLang.debug).parMapN((num, string) => s"my goal in life is $num and $string")

  // regarding failure:
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))

  // compose success + failure
  val parallelWithFailure = (meaningOfLife.debug, aFailure.debug).parMapN((num, string) => s"$num $string")

  // compose failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  // the first effect to fail gives the failure of the result
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = {
    //    composedIO.map(println)
    composedIO_v2.debug.void
    //    twoFailures.debug.void

    (meaningOfLife, goalInLife_v3).parMapN((num, str) => s"$num $str").debug.void
  }
}
