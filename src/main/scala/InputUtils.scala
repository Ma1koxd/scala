import java.time.LocalDate
import java.time.format.DateTimeParseException
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
}
