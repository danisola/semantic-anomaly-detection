package com.danisola.sad.streams

import com.danisola.sad.Event

abstract class InputStream {
  def next: Event
}
