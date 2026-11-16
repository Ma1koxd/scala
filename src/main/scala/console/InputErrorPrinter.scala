package console

import model.InputError

object InputErrorPrinter {
  def printInputError(error: InputError): Unit = {
    error match {
      case InputError.INVALID_EDIT_CHOICE(_) =>
        println("Ошибка: выберите пункт от 0 до 8.")
      case InputError.INVALID_INTEGER(_) =>
        println("Ошибка: это не целое число")
      case InputError.INVALID_DOUBLE(_) =>
        println("Ошибка: введите корректное число. ")
    }
  }
}