import java.time.{LocalDate, LocalTime}

case class FreeTimeSlot(
                         date: LocalDate,
                         startTime: LocalTime,
                         endTime: LocalTime
                       )
