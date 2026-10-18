package model

import java.time.{Duration, LocalDate, LocalTime}

case class ScheduledBlock(
                         taskId: Int,
                         date: LocalDate,
                         startTime: LocalTime,
                         endTime: LocalTime
                         ){
  def durationMinutes: Long = {
    Duration.between(startTime,endTime).toMinutes
  }
}
