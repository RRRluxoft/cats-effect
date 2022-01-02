package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  //  import scala.concurrent.ExecutionContext.Implicits.global
  given ExecutionContext = ExecutionContext.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("I quite like CE", "Scala is great", "looking forward to some awesome stuff")

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list.*
  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {
    // traverse
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    // ^^ this stores ALL the results
    singleFuture.foreach(println)
  }

  import cats.Applicative
  import cats.syntax.traverse.toTraverseOps
  def traverseFutures_v2[T[_]: Traverse, G[_]: Applicative, A, B](fa: T[A])(f: A => G[B]) = {
    val sf: G[T[B]] = fa.traverse(f)
    IO.delay(sf)
  }

  import com.rockthejvm.utils.*

  // traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)
  def singleIO_v3(using listTraverse: Traverse[List]): IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel.* // parTraverse extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /**
    * Exercises
    */
  // hint: use Traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    Traverse[List].traverse(listOfIOs)(identity)

  // hard version
  def sequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(identity)

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  // hard version
  def parSequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(identity)


  // existing sequence API
  val singleIO_v2: IO[List[Int]] = Traverse[List].sequence(ios)

  // parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from the exercise
  val parallelSingleIO_v3: IO[List[Int]] = ios.parSequence  // extension method from the Parallel syntax package

  var i = 0

  override def run: IO[Unit] =
    singleIO_v2.map(_.sum).debug.void
//    parallelSingleIO.map(_.sum).debug.void
//    traverseFutures_v2(workLoad)(heavyComputation).debug.void
//    parallelSingleIO_v3.map(_.sum).debug.void
//    IO(traverseFutures_v2(workLoad)(heavyComputation)).debug
}
