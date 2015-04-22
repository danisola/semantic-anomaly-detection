package com.danisola.sad.patterns

import com.danisola.sad.Event.AttrName
import com.danisola.sad.{Event, Config, Schema, EventsMatrix}

trait PatternFinder {

  def findPatterns(events: EventsMatrix, schema: Schema, config: Config): Seq[Pattern]

  def ifAttrExistsApply[T](event: Event, attrName: AttrName, func: (T) => Boolean): Boolean =
    !event.attributes.contains(attrName) || func(event.attributes(attrName).asInstanceOf[T])
}
