package jem

import cats.data.State
import jem.Decode._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class SerdeSpec extends Specification with ScalaCheck {

  "round trip serialisation" should {
    "work for ints" >> prop { i: Int =>
      val (remainder, result) = int.run(Encode.int(i)).value
      remainder must beEmpty
      result mustEqual i
    }

    "work for longs" >> prop { l: Long =>
      val (remainder, result) = long.run(Encode.long(l)).value
      remainder must beEmpty
      result mustEqual l
    }

    "work for false" >> {
      val (remainder, result) = bool.run(Encode.bool(false)).value
      remainder must beEmpty
      result must beFalse
    }

    "work for true" >> {
      val (remainder, result) = bool.run(Encode.bool(true)).value
      remainder must beEmpty
      result must beTrue
    }

    "work for opaque bytes" >> prop { bs: Seq[Byte] =>
      val (remainder, result) = bytes.run(Encode.bytes(bs)).value
      remainder must beEmpty
      result mustEqual bs
    }

    "work for strings" >> prop { s: String =>
      val (remainder, result) = string.run(Encode.string(s)).value
      remainder must beEmpty
      result mustEqual s
    }

    "work for optional ints" >> prop { i: Option[Int] =>
      val (remainder, result) = opt(int).run(Encode.opt(Encode.int)(i)).value
      remainder must beEmpty
      result mustEqual i
    }

    "work for optional longs" >> prop { l: Option[Long] =>
      val (remainder, result) = opt(long).run(Encode.opt(Encode.long)(l)).value
      remainder must beEmpty
      result mustEqual l
    }

    "work for optional strings" >> prop { s: Option[String] =>
      val (remainder, result) = opt(string).run(Encode.opt(Encode.string)(s)).value
      remainder must beEmpty
      result mustEqual s
    }

    "work for a list of encodables" >> prop { xs: Seq[String] =>
      val (remainder, result) = arr(string).run(Encode.arr(Encode.string)(xs)).value
      remainder must beEmpty
      result mustEqual xs
    }

    "work for a composite of encodables" >> prop { c: CompositeThing =>
      val (remainder, result) = CompositeThing.decode.run(CompositeThing.encode(c)).value
      remainder must beEmpty
      result mustEqual c
    }
  }

  implicit private val arbCompositeThing: Arbitrary[CompositeThing] = Arbitrary(genCompositeThing)
  private def genCompositeThing: Gen[CompositeThing] = for {
    b <- Gen.oneOf(true, false)
    s <- Gen.identifier
    bs <- Gen.option(Gen.containerOf[Seq, Byte](Gen.choose(0x00, 0xff).map(_.toByte)))
    ct <- Gen.option(genCompositeThing)
  } yield CompositeThing(b, s, bs, ct)

  case class CompositeThing(b: Boolean, s: String, bs: Option[Seq[Byte]], next: Option[CompositeThing])

  object CompositeThing {
    def decode: State[Seq[Byte], CompositeThing] = for {
      bs <- Decode.opt(Decode.bytes)
      b <- Decode.bool
      next <- Decode.opt[CompositeThing](CompositeThing.decode)
      s <- Decode.string
    } yield CompositeThing(b, s, bs, next)

    def encode(ct: CompositeThing): Stream[Byte] =
      Encode.opt(Encode.bytes)(ct.bs) ++
        Encode.bool(ct.b) ++
        Encode.opt(CompositeThing.encode)(ct.next) ++
        Encode.string(ct.s)
  }


}