package com.danisola.sad.parsers

import scalaz.Validation

trait Parser {

  def parse(content: String): Validation[String, Map[String, Any]]

}
