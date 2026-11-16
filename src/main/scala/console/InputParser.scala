package console

import model.{InputError, Priority}

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

  def parsePriority(input: String): Either[InputError, Priority] = {
    parseInt(input).flatMap(choice =>
      choice match {
        case 1 =>
          Right(Priority.LOW)
        case 2 =>
          Right(Priority.NORMAL)
        case 3 =>
          Right(Priority.HIGH)
        case 4 =>
          Right(Priority.CRITICAL)
        case _ =>
          Left(InputError.INVALID_PRIORITY_CHOICE(choice))
      }
    )
  }

  def parseBoolean(input: String): Either[InputError, Boolean] = {
    parseInt(input).flatMap(choice =>
      choice match {
        case 1 =>
          Right(true)
        case 2 =>
          Right(false)
        case _ =>
          Left(InputError.INVALID_BOOLEAN_CHOICE(choice))
      }
    )
  }
}