package zio.config.examples

import zio.config._
import zio.config.examples.typesafe.EitherImpureOps
import zio.config.magnolia.DeriveConfigDescriptor._
import zio.config.typesafe._

object OptionalExample extends App with EitherImpureOps {
  case class AppConfig(detail: Option[Detail])
  case class Detail(a: String, b: Option[Detail2])
  case class Detail2(c: String, e: Option[Detail3])
  case class Detail3(f: Option[String], g: Option[String], i: String, h: String)

  val validConfig =
    """
       detail: {
       a : 10
       b : {
         c : 1
         e : {
           f : 1
         }
       }
       }
    """

  println(
    read(descriptor[AppConfig] from getSource(validConfig)).swap
      .map(_.prettyPrint())
  )

  def getSource(str: String): ConfigSource =
    TypesafeConfigSource.fromHoconString(str).loadOrThrow

  final case class CaseClass1(a: Option[CaseClass2])
  final case class CaseClass2(a: String, b: Option[CaseClass3], c: Option[CaseClass4])
  final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int])
  final case class CaseClass4(a: String, b: Option[CaseClass5], c: Option[CaseClass6], d: Option[CaseClass7])
  final case class CaseClass5(a: String)
  final case class CaseClass6(a: String)
  final case class CaseClass7(a: String)

  val validConfig2 =
    """
       a : {
         a : 1
         b : { 
           a : 1
           b : 1
           c : 1
         }
         c : {
           a : 1
           b : {
             a : 1
           }
           c : {
             a : 10
           }
           
           d : {
             a : 15 
           }
         }
       }
    """

  //println(read((descriptor[CaseClass1]) from getSource(validConfig2)).swap.map(_.prettyPrint()))

  object TestCase3 {

    final case class CaseClass1(a: Option[CaseClass2])

    final case class CaseClass2(a: String, b: Option[Either[CaseClass3, CaseClass4]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
    final case class CaseClass4(a1: String, b1: String)

  }

  val validConfig3 =
    """
       a : { 
         a : 1
         b : {
          a1 : 1
          b1 : 2
         }
       }
    """

  //println(read(descriptor[TestCase3.CaseClass1] from getSource(validConfig3)).swap.map(_.prettyPrint()))

  object TestCase4 {
    final case class CaseClass1(a: Option[CaseClass2])

    final case class CaseClass2(a: String, b: Either[Int, CaseClass3])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig4 =
    """
       a : {
         a : 1
         b : 1
       }
    """

  //println(read(descriptor[TestCase4.CaseClass1] from getSource(validConfig4)).swap.map(_.prettyPrint()))

  object TestCase5 {
    final case class CaseClass1(a: Option[CaseClass2])

    final case class CaseClass2(a: String, b: Either[Int, Option[CaseClass3]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig5 =
    """
       a : {
         a : 1
         b : {
         }
       }
    """

  //println(read(descriptor[TestCase5.CaseClass1] from getSource(validConfig5)).swap.map(_.prettyPrint()))

  object TestCase6 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[Either[Int, CaseClass3]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig6 =
    """
       a : {
         a : 1
       }
    """

  // println(read(descriptor[TestCase6.CaseClass1] from getSource(validConfig6)).swap.map(_.prettyPrint()))

  object TestCase7 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[Either[Option[CaseClass3], Option[CaseClass3]]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig7 =
    """
       a : {
         a : 1
         b : {
           a : 1
           b : 2
           e : 3
         }
       }
    """

  //println(read(descriptor[TestCase7.CaseClass1] from getSource(validConfig7)).swap.map(_.prettyPrint()))

  object TestCase8 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[List[CaseClass3]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig8 =
    """
       a : {
         a : 1
         b : [
          {
           e : 3
           b : 1
           a : 1
          }
          {
           e : 2
           a : 1
           b : 1
          }
           {
           a : 1
           b : 1
           e : 2
          }
          {
           a : 1
           b : 1
           e : 2
          }
         ]
       }
    """

  //println(read(descriptor[TestCase8.CaseClass1] from getSource(validConfig8)).swap.map(_.prettyPrint()))

  object TestCase9 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[List[Option[CaseClass3]]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig9 =
    """
       a : {
         a : 1
         b : [
          {
           e : 3
           b : 1
           a : 1
          }
          {
           a : 1
           b : 1
           e : 2
          }
          {
           a : 1
           b : 1
           e : 2
          }
         ]
       }
    """

  //println(read(descriptor[TestCase9.CaseClass1] from getSource(validConfig9)).swap.map(_.prettyPrint()))

  object TestCase10 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[List[Either[CaseClass3, Int]]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig10 =
    """
       a : {
         a : sdsdas
         b : [
          1,
          {
           a : 1
           b : 1
           e : 2
          },
          {
           a : 1
           b : 1
           e : 2
          }
         ]
       }
    """

  // println(read(descriptor[TestCase10.CaseClass1] from getSource(validConfig10)).swap.map(_.prettyPrint()))

  object TestCase11 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[List[Either[CaseClass3, Either[Int, String]]]])
    final case class CaseClass3(a: Int, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig11 =
    """
       a : {
         a : 1
         b : [
          afsal,
          {
           a : 1
           b : 2
           e : 2
          },
         thaj
         ]
       }
    """

  // println(read(descriptor[TestCase11.CaseClass1] from getSource(validConfig11)).swap.map(_.prettyPrint()))

  object TestCase12 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[List[CaseClass3]])
    final case class CaseClass3(a: String, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig12 =
    """
       a : {
         a : 1
         b : []
       }
    """

  // println(read(descriptor[TestCase12.CaseClass1] from getSource(validConfig12)).swap.map(_.prettyPrint()))

  object TestCase13 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[List[CaseClass3]])
    final case class CaseClass3(a: Int, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig13 =
    """
       a : {
         a : 1
         b : [{
          a : 1
          b : s
          e : s
         }
         ]
       }
    """

  // println(read(descriptor[TestCase13.CaseClass1] from getSource(validConfig13)).swap.map(_.prettyPrint()))

  object TestCase14 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[List[CaseClass3]])
    final case class CaseClass3(a: Int, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig14 =
    """
       a : {
         a : 1
         b : [{
          a : 1
          b : s
          e :s
         }
         ]
       }
    """

  // println(read(descriptor[TestCase14.CaseClass1] from getSource(validConfig14)).swap.map(_.prettyPrint()))

  object TestCase15 {
    final case class CaseClass1(a: CaseClass2)

    final case class CaseClass2(a: String, b: Option[Map[String, CaseClass3]])
    final case class CaseClass3(a: Int, b: String, c: Option[Int], d: Option[Int], e: String, f: Option[Int])
  }

  val validConfig15 =
    """
       a : {
         a : 1
         b : {
           x : {
             d : 2
             f : 2
             c : 2
             a: 1
             b : 1
             e : 1
           }
           y : {
             a : 2
             b : 2
             e : 3
            } 
         }
       }
    """

  //println(read(descriptor[TestCase15.CaseClass1] from getSource(validConfig15)).swap.map(_.prettyPrint()))

  object TestCase16 {
    final case class CaseClass1(a: Option[CaseClass2])
    final case class CaseClass2(
      a: String,
      b: String = "2"
    )
    final case class CaseClass3(a: String, b: String)
  }

  val validConfig16 =
    """
       a : {
         a : 1
       }
    """

  //println(read(descriptor[TestCase16.CaseClass1] from getSource(validConfig16)).swap.map(_.prettyPrint()))

  object TestCase17 {
    final case class CaseClass1(a: Option[CaseClass2])
    final case class CaseClass2(
      a: String,
      b: String = "2"
    )
    final case class CaseClass3(a: String, b: String)
  }

  val validConfig17 =
    """
       a : {
         a : 1
       }
    """

  // println(read(descriptor[TestCase17.CaseClass1] from getSource(validConfig17)).swap.map(_.prettyPrint()))

}
