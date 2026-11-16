package console

import console.InputErrorPrinter.printInputError
import model.{InputError, Priority}

import java.time.format.DateTimeParseException
import java.time.{LocalDate, LocalTime}
import scala.io.StdIn.readLine

object InputUtils {
  def readInt(): Int = {
    var result = 0
    var valid = false

    while (!valid) {
      try {
        val task = readLine().toInt
        result = task
        valid = true
      } catch {
        case _: NumberFormatException =>
          println("Ошибка: это не целое число")
      }
    }
    result
  }

  def readDouble(): Double = {
    var result = 0.0
    var valid = false

    while (!valid) {
      try {
        val task = readLine().toDouble
        result = task
        valid = true
      } catch {
        case _: NumberFormatException =>
          println("Ошибка: введите корректное число. ")
      }
    }
    result
  }

  def readPriority(): Priority = {
    var valid = false
    var result = Priority.LOW

    while (!valid) {
      val choice = readInt()
      choice match {
        case 1 =>
          result = Priority.LOW
          valid = true
        case 2 =>
          result = Priority.NORMAL
          valid = true
        case 3 =>
          result = Priority.HIGH
          valid = true
        case 4 =>
          result = Priority.CRITICAL
          valid = true
        case _ =>
          println("Ошибка: введите число от 1 до 4.")
      }
    }
    result
  }

  def readDate(): LocalDate = {
    var valid = false
    var result = LocalDate.of(2000, 1, 1)
    while (!valid) {
      try {
        val date = LocalDate.parse(readLine())
        result = date
        valid = true
      } catch {
        case _: DateTimeParseException =>
          println("Ошибка: введите дату в формате ГГГГ-ММ-ДД")
      }
    }
    result
  }

  def readBoolean(): Boolean = {
    var valid = false
    var result = false

    while (!valid) {
      val choice = readInt()
      choice match {
        case 1 =>
          result = true
          valid = true
        case 2 =>
          result = false
          valid = true
        case _ =>
          println("Ошибка: введите число от 1 до 2")
      }
    }
    result
  }

  def readTime(): LocalTime = {
    var valid = false
    var result = LocalTime.of(0, 0)
    while (!valid) {
      try {
        val time = LocalTime.parse(readLine())
        result = time
        valid = true
      } catch {
        case _: DateTimeParseException =>
          println("Ошибка: введите время в формате ЧЧ:ММ")
      }
    }
    result
  }

  // читаем строку, при завершении ввода возвращаем None
  def readLineOption(): Option[String] = {
    Option(readLine())
  }

  // читаем одну строку и разбираем её переданным парсером
  def readParsedResult[A](parse: String => Either[InputError, A]): Either[InputError, Option[A]] = {
    readLineOption()
      .map(input => parse(input).map(value => Some(value)))
      .getOrElse(Right(None))
  }

  def readIntResult(): Either[InputError, Option[Int]] = {
    readParsedResult(InputParser.parseInt)
  }

  def readDoubleResult(): Either[InputError, Option[Double]] = {
    readParsedResult(InputParser.parseDouble)
  }

  // повторяем ввод при ошибке разбора
  // возвращаем Some(value), когда значение получено, или None, если ввод завершён
  def readParsedOption[A](parse: String => Either[InputError, A]): Option[A] = {
    var result = Option.empty[A]
    var reading = true

    while (reading) {
      readParsedResult(parse) match {
        case Right(value) =>
          result = value
          reading = false
        case Left(error) =>
          printInputError(error)
      }
    }

    result
  }

  def readIntOption(): Option[Int] = {
    readParsedOption(InputParser.parseInt)
  }

  def readDoubleOption(): Option[Double] = {
    readParsedOption(InputParser.parseDouble)
  }

  def readDateOption(): Option[LocalDate] = {
    readParsedOption(InputParser.parseDate)
  }

  def readTimeOption(): Option[LocalTime] = {
    readParsedOption(InputParser.parseTime)
  }

  def readPriorityOption(): Option[Priority] = {
    readParsedOption(InputParser.parsePriority)
  }

  def readBooleanOption(): Option[Boolean] = {
    readParsedOption(InputParser.parseBoolean)
  }
}
