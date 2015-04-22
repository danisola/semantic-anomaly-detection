package com.danisola.sad.patterns

import com.danisola.sad._
import org.specs2.mutable.Specification
import org.specs2.scalaz.ValidationMatchers
import com.danisola.sad.SpecUtils._

import scalaz.Success

class StringPatternFinderSpec extends Specification with ValidationMatchers {

  s"This is a specification for ${StringPatternFinder.getClass.getSimpleName}".txt

  val opts = Config(maxNumEvents = 3, updateProbability = 0)

  "The anomaly detector should" >> {

    "find an anomaly when a String field changes its length" >> {
      val detector = new AnomalyDetector(opts)
      detector.update(event("\"ID_001\""))
      detector.update(event("\"ID_002\""))
      detector.update(event("\"ID_003\""))

      val anomalies: Set[Anomaly] = detector.detect(event("\"ID_0019\""))
      anomalies must mustHaveAnAnomalyOfType(StringPatternFinder.LengthPattern.id)
    }

    "find an anomaly when a String does not follow a regex anymore" >> {
      val detector = new AnomalyDetector(opts)
      detector.update(event("\"ab\""))
      detector.update(event("\"cde\""))
      detector.update(event("\"fjki\""))

      val anomalies: Set[Anomaly] = detector.detect(event("\"aaBaa\""))
      anomalies must mustHaveAnAnomalyOfType(StringPatternFinder.RegexPattern.id)
    }
  }
}