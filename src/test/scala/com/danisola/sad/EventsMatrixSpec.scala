package com.danisola.sad

import com.danisola.sad.SpecUtils._
import org.specs2.mutable._
import org.specs2.scalaz.ValidationMatchers

class EventsMatrixSpec extends Specification with ValidationMatchers {

   s"This is a specification for EventsMatrix".txt

  val config = Config(maxNumEvents = 2)

   "An events matrix should" >> {
     val matrix = new EventsMatrix(config)

     "never exceed the maximum size" >> {
       val csvEvent = event("3, true,  \"hey\", -9.5")
       matrix.update(csvEvent)
       matrix.update(csvEvent)
       matrix.update(csvEvent)

       matrix.numEvents mustEqual config.maxNumEvents
     }
   }
 }