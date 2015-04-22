package com.danisola.sad.patterns

import com.danisola.sad.SpecUtils._
import com.danisola.sad._
import org.specs2.mutable.Specification
import org.specs2.scalaz.ValidationMatchers

class NumericPatternFinderSpec extends Specification with ValidationMatchers {

  s"This is a specification for ${NumericPatternFinder.getClass.getSimpleName}".txt

  val opts = Config(maxNumEvents = 3, updateProbability = 0)

  "The anomaly detector should" >> {

    "find an anomaly when a numeric field changes its sign" >> {
      val detector = new AnomalyDetector(opts)
      detector.update(event("2343"))
      detector.update(event("7456"))
      detector.update(event("938"))

      val anomalies: Set[Anomaly] = detector.detect(event("-2367"))
      anomalies must mustHaveAnAnomalyOfType(NumericPatternFinder.SignPattern.id)
    }
  }
}