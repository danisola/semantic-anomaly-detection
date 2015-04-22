package com.danisola.sad.patterns

import com.danisola.sad.Event._
import com.danisola.sad.{Config, Schema, EventsMatrix}

import scala.collection.immutable.Iterable

object NumericPatternFinder extends PatternFinder {

  override def findPatterns(events: EventsMatrix, schema: Schema, config: Config): Seq[Pattern] = {
    val numericAttrs: Iterable[AttrName] = schema.getAttributesOfType(classOf[java.lang.Double])

    val patterns: Iterable[Seq[Pattern]] = for {
      numericAttrName <- numericAttrs
    } yield {
        val values: Seq[Double] = events(numericAttrName).map(_.asInstanceOf[Double])
        SignPattern.build(numericAttrName, values.map(_.doubleValue()), config)
      }

    patterns.flatten.toSeq
  }

  object SignPattern {

    val id = "NUMERIC_SIGN"

    def build(attrName: AttrName, values: Seq[Double], config: Config): Seq[Pattern] = {
      
      if (values.forall(_ > 0))
        Seq(Pattern.create(
          _id = id,
          _description = s"Attribute $attrName is expected to be positive",
          _isValid = (e) => ifAttrExistsApply[Double](e, attrName, _ > 0)
        ))
      else if (values.forall(_ < 0))
        Seq(Pattern.create(
          _id = id,
          _description = s"Attribute $attrName is expected to be negative",
          _isValid = (e) => ifAttrExistsApply[Double](e, attrName, _ < 0)
        ))
      else
        Seq()

    }
  }
}
