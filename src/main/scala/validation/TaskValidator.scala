package validation

import model.Task

object TaskValidator {
  def validateTask(task: Task): Boolean = {
    if (task.totalHours <= 0) {
      println("Ошибка: трудоёмкость должна быть больше нуля.")
      false
    }
    else if (task.completedHours > task.totalHours) {
      println("Ошибка: выполненное время не может превышать трудоёмкость.")
      false
    }
    else if (task.completedHours < 0) {
      println("Ошибка: выполненное время не может быть отрицательным")
      false
    }
    else if (task.deadline.isBefore(task.startDate)) {
      println("Ошибка: дедлайн не может быть раньше даты начала")
      false
    }
    else if (task.canSplit && task.minBlockMinutes <= 0) {
      println("Ошибка: минимальная продолжительность блока должна быть больше нуля.")
      false
    }
    else {
      true
    }
  }
}
