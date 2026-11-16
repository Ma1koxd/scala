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
      case InputError.INVALID_DATE(_) =>
        println("Ошибка: введите дату в формате ГГГГ-ММ-ДД")
      case InputError.INVALID_TIME(_) =>
        println("Ошибка: введите время в формате ЧЧ:ММ")
      case InputError.INVALID_PRIORITY_CHOICE(_) =>
        println("Ошибка: введите число от 1 до 4.")
      case InputError.INVALID_BOOLEAN_CHOICE(_) =>
        println("Ошибка: введите число от 1 до 2")
    }
  }
}