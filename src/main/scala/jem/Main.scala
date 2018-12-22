package jem

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset

import cats.data.State

object Main {

  def main(args: Array[String]): Unit = {
    val ints = (1 to 99).map(Encode.int)
    ints.map(Decode.int.run).map(_.value).foreach(println)
  }

}

object Decode {

  val int: State[Seq[Byte], Int] = State[Seq[Byte], Int] {
    case bs if bs.length >= 4 =>
      bs.drop(4) -> ByteBuffer.wrap(bs.take(4).toArray).getInt
    case _ => throw new EOFException()
  }

  val long: State[Seq[Byte], Long] = State[Seq[Byte], Long] {
    case bs if bs.length >= 8 =>
      bs.drop(8) -> ByteBuffer.wrap(bs.take(8).toArray).getLong
    case _ => throw new EOFException()
  }

  val bool: State[Seq[Byte], Boolean] = int.map(_ == 1)

  def bytes(len: Int): State[Seq[Byte], Seq[Byte]] = State[Seq[Byte], Seq[Byte]] {
    case bs if bs.length >= len => bs.drop(len) -> bs.take(len)
    case _ => sys.error(s"Insufficient data remains to parse $len bytes")
  }

  val bytes: State[Seq[Byte], Seq[Byte]] = for {
    len <- int
    bs <- bytes(len)
  } yield bs

  val string: State[Seq[Byte], String] = bytes.map(_.toArray).map(new String(_, Charset.forName("UTF-8")))

  def opt[T](parseT: State[Seq[Byte], T]): State[Seq[Byte], Option[T]] = bool.flatMap {
    case true => parseT.map(Some(_))
    case false => State.pure(None)
  }

  def arr[T](parseT: State[Seq[Byte], T]): State[Seq[Byte], Seq[T]] = {
    def inner(qty: Int, ts: Seq[T]): State[Seq[Byte], Seq[T]] = qty match {
      case 0 => State.pure(ts)
      case _ => for {
        t <- parseT
        ts_ <- inner(qty - 1, t +: ts)
      } yield ts_
    }
    int.flatMap(inner(_, Nil)).map(_.reverse)
  }
}

object Encode {

  def int(i: Int): Stream[Byte] = {
    val buffer = ByteBuffer.allocate(4)
    buffer.putInt(i)
    buffer.array().toStream
  }

  def long(l: Long): Stream[Byte] = {
    val buffer = ByteBuffer.allocate(8)
    buffer.putLong(l)
    buffer.array().toStream
  }

  def bytes(bs: Seq[Byte]): Stream[Byte] = int(bs.length) ++ bs

  def string(s: String): Stream[Byte] = bytes(s.getBytes(Charset.forName("UTF-8")))

  def opt[T](f: T => Stream[Byte])(o: Option[T]): Stream[Byte] = o.map(t => int(1) ++ f(t)).getOrElse(int(0))

  def arr[T](f: T => Stream[Byte])(xs: Seq[T]): Stream[Byte] = int(xs.size) ++ xs.flatMap(f(_))

  def bool(b: Boolean): Stream[Byte] = if (b) int(1) else int(0)
}