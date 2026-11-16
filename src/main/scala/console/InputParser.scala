package console

import model.InputError

import java.time.{LocalDate, LocalTime}
import java.time.format.DateTimeParseException

object InputParser {
  def parseInt(input: String): Either[InputError, Int] = {
    input.toIntOption.toRight(InputError.INVALID_INTEGER(input))
  }

  def parseDouble(input: String): Either[InputError, Double] = {
    input.toDoubleOption.toRight(InputError.INVALID_DOUBLE(input))
  }

  def parseDate(input: String): Either[InputError, LocalDate] = {
    try {
      Right(LocalDate.parse(input))
    } catch {
      case _: DateTimeParseException =>
        Left(InputError.INVALID_DATE(input))
    }
  }

  def parseTime(input: String): Either[InputError, LocalTime] = {
    try {
      Right(LocalTime.parse(input))
    } catch {
      case _: DateTimeParseException =>
        Left(InputError.INVALID_TIME(input))
    }
  }
}