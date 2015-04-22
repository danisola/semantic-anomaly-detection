package com.danisola.sad

import com.danisola.sad.Event._
import com.danisola.sad.Schema.ClassId
import com.danisola.sad.patterns.{NumericPatternFinder, StringPatternFinder, Pattern}

import scala.collection.mutable.ListBuffer
import scala.util.Random

class AnomalyDetector(config: Config) {

  private val events: EventsMatrix = new EventsMatrix(config)
  private var schema: Schema = Schema.emptySchema
  private var patterns: Seq[Pattern] = Seq()
  private val rand = Random

  def detect(event: Event): Set[Anomaly] = {
    val schemaAnomalies = validateSchema(event, schema)

    if (schemaAnomalies.isEmpty)
      validatePatterns(event, schema)
    else
      schemaAnomalies
  }

  private def validateSchema(event: Event, eventsSchema: Schema): Set[Anomaly] =
    if (eventsSchema.isEmpty) Set()
    else eventsSchema.validateSchema(event)

  private def validatePatterns(event: Event, eventsSchema: Schema): Set[Anomaly] =
    if (patterns.isEmpty) Set()
    else {
      val anomalies: Seq[Anomaly] = patterns.map(_.check(event)).filter {
        case None => false
        case _ => true
      }.map(_.get)
      anomalies.toSet
    }

  def update(event: Event, anomaliesDetected: Boolean = false) = {
    val modelJustCompleted = events.numEvents == config.maxNumEvents - 1
    val rebuildModel = events.numEvents == config.maxNumEvents && rand.nextDouble() < config.updateProbability

    events.update(event)

    if (anomaliesDetected | modelJustCompleted | rebuildModel) {
      schema = buildSchema()
      patterns = buildPatterns()
    }
  }

  def buildSchema(): Schema = {
    val values: Iterable[(AttrName, Seq[String])] = for {
      field <- events
      attrName: AttrName = field._1
      vals: ListBuffer[Any] = field._2
      types: Set[ClassId] = vals.map(Schema.classId).toSet
    } yield (attrName, types.toSeq)

    Schema(values.toMap)
  }

  def buildPatterns(): Seq[Pattern] = {
    Seq(
      StringPatternFinder.findPatterns(events, schema, config),
      NumericPatternFinder.findPatterns(events, schema, config)
    ).flatten
  }
}
