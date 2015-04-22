package com.danisola.sad

import com.danisola.sad.Event._

import scala.collection.mutable.ListBuffer
import scala.collection._

class EventsMatrix(config: Config) extends Iterable[(AttrName, ListBuffer[Any])] {

  private var events: mutable.HashMap[AttrName, ListBuffer[Any]] = mutable.HashMap()

  def apply(attrName: AttrName): Seq[Any] = {
    events(attrName)
  }

  def update(event: Event) = {
    for (attr: (AttrName, AttrValue) <- event.attributes) {
      val attrName = attr._1
      val attrValue = attr._2
      if (!events.contains(attrName))
        events += (attrName -> ListBuffer())

      events(attrName).append(attrValue)
    }

    if (numEvents > config.maxNumEvents) {
      for (attr: (AttrName, ListBuffer[Any]) <- events) {
        attr._2.remove(0)
      }
    }
  }

  def numEvents: Int =
    if (events.isEmpty) 0
    else events.head._2.size

  def numAttributes: Int = events.size

  override def iterator: scala.Iterator[(AttrName, ListBuffer[Any])] = events.iterator

  override def seq: Map[AttrName, ListBuffer[Any]] = events
}
