package model

import io.JsonFormats.given
import upickle.default.ReadWriter

import java.time.{Duration, LocalDate, LocalTime}

case class FreeTimeSlot(
                         date: LocalDate,
                         startTime: LocalTime,
                         endTime: LocalTime
                       ) derives ReadWriter {
  def durationMinutes: Long = {
    Duration.between(startTime, endTime).toMinutes
  }
}
