package com.danisola.sad

import com.danisola.sad.patterns.StringPatternFinder
import org.specs2.mutable._
import org.specs2.scalaz.ValidationMatchers
import com.danisola.sad.SpecUtils._

import scalaz._

class AnomalyDetectorSpec extends Specification with ValidationMatchers {

  s"This is a specification for ${Schema.getClass.getSimpleName}".txt

  val config = Config(maxNumEvents = 2, updateProbability = 0)

  "The anomaly detector should" >> {
    val detector = new AnomalyDetector(config)

    "find an anomaly when a field changes its type" >> {
      detector.update(event("3, true,  \"hey\", -9.5"))
      detector.update(event("3, false, \"hey\",  3.5"))

      val anomalies: Set[Anomaly] = detector.detect(event("3, true, 83490.5, 5.2"))
      anomalies must mustHaveAnAnomalyOfType(Schema.anomalyId)
    }
  }
}