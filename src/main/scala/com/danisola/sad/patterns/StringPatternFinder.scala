package com.danisola.sad.patterns

import java.util.regex

import com.danisola.sad.Event.AttrName
import com.danisola.sad._

import scala.collection.immutable.{IndexedSeq, Iterable}

object StringPatternFinder extends PatternFinder {

  override def findPatterns(events: EventsMatrix, schema: Schema, config: Config): Seq[Pattern] = {
    val strAttrs: Iterable[AttrName] = schema.getAttributesOfType(classOf[String])

    val patterns: Iterable[Seq[Pattern]] = for {
      strAttrName <- strAttrs
    } yield {
        val values: Seq[String] = events(strAttrName).map(_.asInstanceOf[String])
        LengthPattern.build(strAttrName, values, config) ++
          RegexPattern.build(strAttrName, values, config) ++
          EnumerationPattern.build(strAttrName, values, config)
      }

    patterns.flatten.toSeq
  }

  object LengthPattern {
    val id = "STRING_LENGTH"

    def build(attrName: AttrName, values: Seq[String], config: Config): Seq[Pattern] = {
      val lengths: Set[Int] = values.map(_.length).toSet
      if (lengths.size <= config.maxValuesForPattern) {
        val lengthPatterns: Set[Pattern] = lengths.map(length => Pattern.create(
          _id = id,
          _description = s"The length of attribute $attrName is expected to be $length",
          _isValid = (e) => ifAttrExistsApply[String](e, attrName, _.length == length)
        ))

        Seq(Pattern.anyOf(lengthPatterns))
      } else
        Seq()
    }
  }

  object RegexPattern {
    val id = "STRING_REGEX"

    val regexes = List(
      "numbersPattern" -> "\\d+".r.pattern,
      "lowercasePattern" -> "[a-z]+".r.pattern,
      "uppercasePattern" -> "[A-Z]+".r.pattern,
      "wordPattern" -> "\\w+".r.pattern
    )

    val maxAllowedLength = 40

    def build(attrName: AttrName, values: Seq[String], config: Config): Seq[Pattern] = {
      val maxLength: Int = values.map(_.length).max
      if (maxLength <= maxAllowedLength) {
        val matchedRegex: Option[(String, regex.Pattern)] = regexes.find(p => values.forall(value => p._2.matcher(value).matches()))
        matchedRegex match {
          case Some(pair) => Seq(Pattern.create(
            _id = id,
            _description = s"Attribute $attrName is expected to follow the regex '${pair._2.pattern()}'",
            _isValid = (e) => ifAttrExistsApply[String](e, attrName, pair._2.matcher(_).matches())
          ))
          case None => Seq()
        }
      } else
        Seq()
    }
  }

  object PositionalPattern {
    val id = "STRING_POSITIONAL"

    val markers = """[]()\/- ,."""
    val maxAllowedLength = 40

    def build(attrName: AttrName, values: Seq[String], config: Config): Seq[Pattern] = {

      val lengths: Seq[Int] = values.map(_.length)
      if (lengths.size == 1 && lengths.head <= maxAllowedLength) {
        val first = values.head
        val rest = values.tail
        val positions: Seq[(Char, Int)] = for {
          marker <- markers
          (char, index) <- first.zipWithIndex
          if markers.contains(char)
          if rest.forall(str => str.length > index && str(index) == char)
        } yield (char, index)

        Seq(Pattern.create(
          _id = id,
          _description = s"Attribute $attrName is expected to have those values: $positions",
          _isValid = (e) => ifAttrExistsApply[String](e, attrName, str =>
            positions.forall(p => str.length < p._1 || str(p._1) == p._2))
        ))
      } else
        Seq()
    }
  }

  object EnumerationPattern {

    val id = "STRING_ENUMERATION"
    val maxDifferentValues = 10

    def build(attrName: AttrName, values: Seq[String], config: Config): Seq[Pattern] = {
      val uniqueValues: Set[String] = values.toSet
      if (uniqueValues.size <= maxDifferentValues) {
        Seq(Pattern.create(
          _id = id,
          _description = s"Attribute $attrName is expected to have one of these values: '$uniqueValues'",
          _isValid = (e) => ifAttrExistsApply[String](e, attrName, uniqueValues.contains)
        ))
      } else
        Seq()
    }
  }

}
