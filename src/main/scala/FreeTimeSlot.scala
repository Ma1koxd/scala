import java.time.{Duration, LocalDate, LocalTime}

case class FreeTimeSlot(
                         date: LocalDate,
                         startTime: LocalTime,
                         endTime: LocalTime
                       ) {
  def durationMinutes: Long = {
    Duration.between(startTime, endTime).toMinutes
  }
}
