package com.danisola.sad.patterns

import com.danisola.sad.{Anomaly, Event}

object Pattern {

  def anyOf(patterns: Iterable[Pattern]): Pattern = {
    if (patterns.size == 1) patterns.head
    else create(
      _id = "ANY_OF_PATTERN",
      _description = s"Event must match at least one of those patterns: ${patterns.map(_.description).mkString(" | ")}",
      _isValid = (event) => patterns.map(_.check(event))
        .exists {
        case None => true
        case _ => false
      })
  }

  def anyOf(patterns: Pattern*): Pattern = anyOf(patterns.toIterable)

  def create(_id: String, _description: String, _isValid: (Event) => Boolean): Pattern = new Pattern {

    override def description: String = _description

    override def id: String = _id

    override def check(event: Event): Option[Anomaly] =
      if (_isValid(event)) None
      else this.anomaly()
  }
}

abstract class Pattern() {

  def check(event: Event): Option[Anomaly]

  def id: String

  def description: String

  protected def anomaly() = Option(Anomaly(id, description))
}
