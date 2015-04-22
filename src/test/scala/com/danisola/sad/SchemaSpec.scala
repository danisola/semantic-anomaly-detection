package com.danisola.sad

import org.specs2.mutable._
import org.specs2.scalaz.ValidationMatchers

class SchemaSpec extends Specification with ValidationMatchers {

  s"This is a specification for ${Schema.getClass.getSimpleName}".txt

  val event = Event("item_purchased/1-0-0",
    Map(
      "schema"                     -> "item_purchased/1-0-0",
      "data.item_id"               -> "D6KML",
      "data.purchase_counter"      -> 11,
      "data.customer_country"      -> "UK",
      "data.customer_phone_prefix" -> "44",
      "data.customer_requests"     -> None
    )
  )

  "An event should" >> {
    "get its type metadata extracted" >> {
      val expected: Schema = Schema(Map(
        "schema" -> idSeq(classOf[String]),
        "data.item_id" -> idSeq(classOf[String]),
        "data.purchase_counter" -> idSeq(classOf[Integer]),
        "data.customer_country" -> idSeq(classOf[String]),
        "data.customer_phone_prefix" -> idSeq(classOf[String]),
        "data.customer_requests" -> idSeq(None.getClass)
      ))

      Schema.extract(event) mustEqual expected
    }
  }

  def idSeq(clazz: Class[_]) = Seq(Schema.classId(clazz))
}