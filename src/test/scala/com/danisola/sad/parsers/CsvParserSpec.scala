package com.danisola.sad.parsers

import org.specs2.mutable._
import org.specs2.scalaz.ValidationMatchers

import scalaz._
import Scalaz._

class CsvParserSpec extends Specification with ValidationMatchers {

   s"This is a specification for ${CsvParser.getClass.getSimpleName}".txt

   val csv = "null, false, true, 83490.5, -890.4, 5, \"hello\""

   "A valid CSV object should" >> {
     "be parsed correctly" >> {
       val expected = Map(
         "0" -> None,
         "1" -> false,
         "2" -> true,
         "3" -> 83490.5d,
         "4" -> -890.4d,
         "5" -> 5,
         "6" -> "hello"
       )
       CsvParser.parse(csv) must beSuccessful(expected)
     }
   }
 }