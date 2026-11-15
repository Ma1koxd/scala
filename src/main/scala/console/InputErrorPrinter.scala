package console

import model.InputError

object InputErrorPrinter {
  def printInputError(error: InputError): Unit = {
    error match {
      case InputError.INVALID_EDIT_CHOICE(_) =>
        println("Ошибка: выберите пункт от 0 до 8.")
    }
  }
}