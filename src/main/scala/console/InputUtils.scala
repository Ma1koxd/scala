package console

import console.InputErrorPrinter.printInputError
import model.{InputError, Priority}

import java.time.{LocalDate, LocalTime}
import scala.io.StdIn.readLine

object InputUtils {
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
