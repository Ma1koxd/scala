package console

import model.TaskError

object TaskErrorPrinter {
  def printTaskError(error: TaskError): Unit = {
    error match {
      case TaskError.INVALID_TOTAL_HOURS =>
        println("Ошибка: трудоёмкость должна быть больше нуля.")
      case TaskError.COMPLETED_HOURS_EXCEED_TOTAL =>
        println("Ошибка: выполненное время не может превышать трудоёмкость.")
      case TaskError.NEGATIVE_COMPLETED_HOURS =>
        println("Ошибка: выполненное время не может быть отрицательным")
      case TaskError.DEADLINE_BEFORE_START =>
        println("Ошибка: дедлайн не может быть раньше даты начала")
      case TaskError.INVALID_MIN_BLOCK_MINUTES =>
        println("Ошибка: минимальная продолжительность блока должна быть больше нуля.")
      case TaskError.TASK_NOT_FOUND(_) =>
        println("Дело с таким ID не найдено.")
    }
  }
}