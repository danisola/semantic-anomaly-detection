package com.danisola.sad

import com.danisola.sad.Event._
import com.danisola.sad.Schema.ClassId

import scala.collection.immutable.Iterable

object Schema {

  val anomalyId = "SCHEMA_ANOMALY"

  type ClassId = String

  def classId(value: Any): ClassId = classId(value.getClass)
  def classId(clazz: Class[_]): ClassId = clazz.getCanonicalName

  def emptySchema = Schema(Map())

  def extract(event: Event): Schema = {
    val vals: Map[AttrName, ClassId] = for {
      attribute <- event.attributes
    } yield (attribute._1, attribute._2.getClass.getCanonicalName)

    Schema(vals.mapValues(Seq(_)))
  }
}

private case class MetaType(attrType: Class[_])

case class Schema(types: Map[AttrName, Seq[ClassId]]) {
  
  def isEmpty = types.isEmpty

  def validateSchema(event: Event): Set[Anomaly] = {
    if (types.isEmpty) return Set()

    val eventTypes: Map[AttrName, Seq[ClassId]] = Schema.extract(event).types

    def checkAdditions: Set[Anomaly] = {
      val additions = eventTypes -- types.keySet
      if (additions.nonEmpty) Set(anomaly(s"Unexpected fields found: $additions"))
      else Set()
    }

    def checkMissing: Set[Anomaly] = {
      val missing = types -- eventTypes.keySet
      if (missing.nonEmpty) Set(anomaly(s"Missing fields: $missing"))
      else Set()
    }

    def checkValues: Set[Anomaly] = {
      val vals: Iterable[Anomaly] = for {
        attrMeta: (AttrName, Seq[ClassId]) <- types
        attrName = attrMeta._1
        attrVal  = attrMeta._2.head
        if eventTypes.contains(attrName) && !eventTypes(attrName).contains(attrVal)
      } yield anomaly(s"In attribute ${attrMeta._1}, any of ${printSeq(attrMeta._2)} is expected, but ${printSeq(eventTypes(attrMeta._1))} has been found")
      vals.toSet
    }

    checkAdditions ++ checkMissing ++ checkValues
  }

  def getAttributesOfType(clazz: Class[_]): Iterable[AttrName] = {
    if (types.isEmpty) Set()
    else {
      val classId = Schema.classId(clazz)
      for {
        pair <- types
        attrName  = pair._1
        attrTypes = pair._2
        if attrTypes.length == 1 && attrTypes.head == classId
      } yield attrName
    }
  }

  private def anomaly(msg: String) = Anomaly(Schema.anomalyId, msg)

  private def printSeq(seq: Seq[_]) = seq.mkString("[", ",", "]")
}