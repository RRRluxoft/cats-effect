package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.IORuntime

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object IOErrorHandling {

  // IO: pure, delay, defer
  // create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A FAILURE"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail")) // preferable instead of first one
  val aDefer: IO[Int] = IO.defer(IO.raiseError(new RuntimeException("a proper fail")))

  // handle exceptions
  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here"))
    // add more cases
  }

  // turn into an Either
  type ErrorOr[A] = Either[Throwable, A]
  val effectAsEither: IO[ErrorOr[Int]] = aFailedCompute.attempt
  // redeem: transform the failure and the success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => s"FAIL: $ex", int => s"SUCCESS: $int")
  // redeemWith
  val resultAsEffect: IO[Unit] = aFailure.redeemWith(ex => IO(println(s"FAIL: $ex")), int => IO(println(s"SUCCESS: $int")))

  /**
    * Exercises
    */
  // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case Some(value) => IO.pure(value)
    case None        => IO.raiseError(ifEmpty)
  }

  def try2IO[A](aTry: Try[A]): IO[A] = aTry match {
    case Success(value) => IO.pure(value)
    case Failure(ex)    => IO.raiseError(ex)
  }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = anEither match {
    case Right(value) => IO.pure(value)
    case Left(ex)     => IO.raiseError(ex)
  }

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem[A](handler, identity[A])

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, a => IO.pure(a))

  def main(args: Array[String]): Unit = {
//    import cats.effect.unsafe.implicits.global
    given IORuntime = cats.effect.unsafe.implicits.global
//    import IORuntime.global
//    aFailedCompute.unsafeRunSync()
    println(
      s"""
         |resultAsString.redeem
         |${resultAsString.unsafeRunSync()}
         |""".stripMargin)

    resultAsEffect.unsafeRunSync()
  }
}
