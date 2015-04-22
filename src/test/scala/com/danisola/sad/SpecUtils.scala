package com.danisola.sad

import com.danisola.sad.parsers.CsvParser
import org.specs2.matcher._
import org.specs2.scalaz.ValidationMatchers

import scalaz.Success

object SpecUtils extends ValidationMatchers {

  def event(csv: String): Event = CsvParser.parse(csv) match {
    case Success(attrs) => Event("test_event", attrs)
  }

  def mustHaveAnAnomalyOfType(anomalyType: String): Matcher[Set[Anomaly]] = new Matcher[Set[Anomaly]] {

    override def apply[S <: Set[Anomaly]](actual: Expectable[S]): MatchResult[S] = {
      val anomalies: Set[Anomaly] = actual.value
      result((anomalies.size == 1) && anomalies.head.patternType == anomalyType,
        s"Anomaly set contains only one anomaly of type $anomalyType",
        s"Anomaly does not contain only one anomaly of type $anomalyType: $anomalies",
        actual
      )
    }
  }
}

