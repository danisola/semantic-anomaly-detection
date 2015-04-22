package com.danisola.sad.parsers

import org.json4s._
import org.json4s.native.JsonMethods

import scala.util.{Failure, Success, Try}
import scalaz.Validation

import scalaz._
import Scalaz._

object JsonParser extends Parser {

  override def parse(content: String): Validation[String, Map[String, Any]] = {
    for {
      json <- safe(() => JsonMethods.parse(content))
      flat <- safe(() => flatten(List(), json))
    } yield flat
  }

  private def flatten(prefix: List[String], value: JValue): Map[String, Any] = value match {
    case JObject(fields) =>
      val fieldsList: List[Map[String, Any]] = for {
        pair <- fields
        fieldPrefix: List[String] = pair._1 :: prefix
      } yield flatten(fieldPrefix, pair._2)
      fieldsList.flatten.toMap
    case JString(str)      => Map(buildStr(prefix) -> str)
    case JBool(bool)       => Map(buildStr(prefix) -> bool)
    case JString(str)      => Map(buildStr(prefix) -> str)
    case JInt(num)         => Map(buildStr(prefix) -> num.toDouble)
    case JDecimal(num)     => Map(buildStr(prefix) -> num.toDouble)
    case JDouble(num)      => Map(buildStr(prefix) -> num)
    case JNothing | JNull  => Map(buildStr(prefix) -> None)
    case other             => throw new IllegalStateException(s"${other.getClass.getSimpleName} is not supported")
  }

  private def safe[A](f: () => A): Validation[String, A] = Try(f()) match {
    case Success(out) => out.success
    case Failure(ex)  => ex.getMessage.failure
  }

  private def buildStr(list: List[String]) = list.reverse.mkString(".")
}
