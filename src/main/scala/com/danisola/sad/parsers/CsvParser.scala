package com.danisola.sad.parsers

import scala.util.parsing.combinator._
import scalaz.Validation

import scalaz._
import Scalaz._

object CsvParser extends JavaTokenParsers with Parser {

  private val intPattern = "[+\\-]?\\d+".r.pattern

  override def parse(content: String): Validation[String, Map[String, Any]] = {
    parse(csvRecord, content) match {
      case Success(values, input) =>
        (values.indices zip values).map(p => (p._1.toString, p._2)).toMap.success
      case NoSuccess(msg, input) =>
        msg.failure
    }
  }

  override def skipWhitespace = false

  def whitespace: Parser[String] = """[ \t]*""".r

  def csvRecord: Parser[List[Any]] = repsep(rawField, ",") ^^ (List() ::: _)

  def rawField: Parser[Any] = opt(whitespace) ~ value ~ opt(whitespace) ^^ { case a ~ b ~ c => b}

  def value: Parser[Any] = (
      stringLiteral ^^ (_.replace("\"", ""))
    | decimalNumber ^^ (_.toDouble)
    | floatingPointNumber ^^ (_.toDouble)
    | "null"  ^^ (_ => None)
    | "true"  ^^ (_ => true)
    | "false" ^^ (_ => false)
    )
}