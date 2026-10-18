package io

import upickle.default.{ReadWriter, readwriter}

import java.time.{LocalDate, LocalTime}

object JsonFormats {
  given ReadWriter[LocalDate] = readwriter[String].bimap(
    date => date.toString,
    value => LocalDate.parse(value)
  )

  given ReadWriter[LocalTime] = readwriter[String].bimap(
    time => time.toString,
    value => LocalTime.parse(value)
  )
}