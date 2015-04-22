package com.danisola.sad.parsers

import org.specs2.mutable._
import org.specs2.scalaz.ValidationMatchers

class JsonParserSpec extends Specification with ValidationMatchers {

  s"This is a specification for ${JsonParser.getClass.getSimpleName}".txt

  val json = """{
               |  "schema": "item_purchased/1-0-0",
               |  "data": {
               |    "item_id": "D6KML",
               |    "purchase_counter": 11,
               |    "customer_country": "UK",
               |    "customer_phone_prefix": "44",
               |    "customer_requests": null
               |  }
               |}""".stripMargin

  "A valid JSON object should" >> {
    "be parsed correctly" >> {
      val expected = Map(
        "schema"                     -> "item_purchased/1-0-0",
        "data.item_id"               -> "D6KML",
        "data.purchase_counter"      -> 11,
        "data.customer_country"      -> "UK",
        "data.customer_phone_prefix" -> "44",
        "data.customer_requests"     -> None
      )
      JsonParser.parse(json) must beSuccessful(expected)
    }
  }
}