package com.danisola.sad

import com.danisola.sad.Event.{AttrValue, AttrName}

object Event {
  type AttrName  = String
  type AttrValue = Any
}

case class Event(eventType: String, attributes: Map[AttrName, AttrValue])

