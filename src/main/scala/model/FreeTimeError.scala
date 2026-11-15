package model

import java.time.{LocalDate, LocalTime}

enum FreeTimeError {
  case INVALID_TIME_RANGE
  case OVERLAPPING_SLOT
  case SLOT_NOT_FOUND(date: LocalDate, startTime: LocalTime)
}