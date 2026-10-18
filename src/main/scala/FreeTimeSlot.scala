import java.time.{Duration, LocalDate, LocalTime}
import JsonFormats.given
import upickle.default.ReadWriter

case class FreeTimeSlot(
                         date: LocalDate,
                         startTime: LocalTime,
                         endTime: LocalTime
                       ) derives ReadWriter {
  def durationMinutes: Long = {
    Duration.between(startTime, endTime).toMinutes
  }
}
