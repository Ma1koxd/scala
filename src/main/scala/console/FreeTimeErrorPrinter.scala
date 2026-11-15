package console

import model.FreeTimeError

object FreeTimeErrorPrinter {
  def printFreeTimeError(error: FreeTimeError): Unit = {
    error match {
      case FreeTimeError.INVALID_TIME_RANGE =>
        println("Ошибка: время окончания должно быть позже времени начала.")
      case FreeTimeError.OVERLAPPING_SLOT =>
        println("Ошибка: свободное время пересекается с уже существующим промежутком.")
    }
  }
}