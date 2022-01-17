package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp, Resource}
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

import java.io.{File, FileReader}
import java.util.{Scanner, UUID}
import scala.concurrent.duration.*
import scala.util.Try

object Resources extends IOApp.Simple {

  import com.rockthejvm.utils.*

  // use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open(): IO[String] = IO(s"opening connection to $url").debug
    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open() >> IO(println(s"${System.currentTimeMillis()}")) >> IO.sleep(60 seconds)).start
    _   <- IO.sleep(1 second) >> fib.cancel >> IO(println(s"${System.currentTimeMillis()}"))
  } yield ()
  // problem: leaking resources

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib  <- (conn.open() >> IO(println(s"${System.currentTimeMillis()}")) >> IO.sleep(60 seconds)).onCancel(conn.close().void).start
    _    <- IO.sleep(1 second) >> fib.cancel >> IO(println(s"${System.currentTimeMillis()} "))
  } yield ()

  /*
    bracket pattern: someIO.bracket(useResourceCb)(releaseResourceCb)
    bracket is equivalent to try-catches (pure FP)
   */
  val bracketFetchUrl: IO[Unit] = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close().void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _   <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  val bracket_2 = IO(UUID.randomUUID()).debug
    .bracket(uuid => IO.sleep(1 second) >> IO(UUID.fromString(uuid.toString)).debug >> IO.sleep(1 second))(uuid => IO(println(s"UUID ... $uuid")).debug)
  val uuidProgram = for {
    fib <- bracket_2.start
    _   <- IO.sleep(5000 millis) >> fib.cancel
//    res <- fib.join
  } yield ()

  /**
    * Exercise: read the file with the bracket pattern
    *  - open a scanner
    *  - read the file line by line, every 100 millis
    *  - close the scanner
    *  - if cancelled/throws error, close the scanner
    */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100 millis) >> readLineByLine(scanner)
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") >>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner =>
//      /*IO(s"closing file at $path").debug >>*/
        IO(scanner.close())
      }

  /**
    * Resources
    */
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket { scanner =>
        // acquire a connection based on the file
        IO(new Connection(scanner.nextLine())).bracket { conn =>
          conn.open() >> IO.never
        }(conn => conn.close().void)
      }(scanner => IO("closing file").debug >> IO(scanner.close()))
  // nesting resources are tedious

  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close().void)
  // ... at a later part of your code

  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
//    _   <- fib.join
    _   <- IO.sleep(600 millis) >> fib.cancel
  } yield ()

  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string").debug
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").debug.void

  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)

  val simpleConnection: IO[Connection] = IO(new Connection(url = "http://"))
  val usingConnection: Connection => IO[String] = conn => IO(s"Doing smth useful with ${conn.open()}").debug <* IO.sleep(150 millis)
  val releaseConnection: Connection => IO[Unit] = conn => IO("closing").debug >> IO(conn.close()).debug.void

  Resource.make(simpleConnection)(releaseConnection).use(usingConnection)
  simpleConnection.bracket[String](usingConnection)(releaseConnection)

  /**
    *  Exercise: read a text file with one line every 100 millis, using Resource
    *  (refactor the bracket exercise to use Resource)
    */
  def getResourceFromFile(path: String): Resource[IO, Scanner] = Resource
    .make(openFileScanner(path)) { scanner =>
      IO(s"closing file at path: $path").debug >> IO(scanner.close()).debug
    }

  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") >>
      getResourceFromFile(path).use { scanner =>
        readLineByLine(scanner)
      }

  val pathFile = "src/main/scala/com/rockthejvm/part3concurrency/Resources.scala"
  Resource.make(openFileScanner(pathFile))(scanner => IO(scanner.close()))
    .use(readLineByLine)

  def nonCanceledReadFile(path: String): IO[Unit] = for {
    fib <- resourceReadFile(path).start
    _   <- fib.join
  } yield ()

  def cancelReadFile(path: String): IO[Unit] = for {
    fib <- resourceReadFile(path).start
    _   <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  // nested resources
  def connFromConfResource(path: String): Resource[IO, Connection] =
    Resource.make(IO("opening file").debug >> openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void))

  val openConn_v1 = connFromConfResource("src/main/resources/connection.txt")
    .use(conn => conn.open() >> IO.never)

  // equivalent
  def connFromConfResourceClean(path: String): Resource[IO, Connection] = for {
    scanner <- Resource.make(IO("opening file").debug >> openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
    conn    <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void)
  } yield conn

  val openConnection = connFromConfResourceClean("src/main/resources/connection.txt").use(conn => conn.open() >> IO.never)
  val canceledConnection = for {
    fib <- openConnection.start
    _   <- IO.sleep(1.second) >> IO("cancelling!").debug >> fib.cancel
  } yield ()

  // connection + file will close automatically

  // finalizers to regular IOs
  val ioWithFinalizer: IO[String] = IO("some resource").debug.guarantee(IO("freeing resource").debug.void)
  val ioWithFinalizer_v2 = IO("some resource").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(result => IO(s"releasing resource: $result").debug).void
    case Errored(e)    => IO("nothing to release").debug.void
    case Canceled()    => IO("resource got canceled, releasing what's left").debug.void
  }

  override def run: IO[Unit] =
//    uuidProgram.void
//    bracketReadFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
//    resourceFetchUrl.void
//    cancelReadFile(pathFile)
//    nonCanceledReadFile(pathFile)
//    openConn_v1
//    openConnection.void
    canceledConnection
}
