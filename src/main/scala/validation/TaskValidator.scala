package validation

import model.{Task, TaskError}

object TaskValidator {
  // данная функция возвращает какая ИМЕННО ошибка возникла или дело, если проверка успешна
  def validateTaskResult(task: Task): Either[TaskError, Task] = {
    if (task.totalHours <= 0) {
      Left(TaskError.INVALID_TOTAL_HOURS)
    } else if (task.completedHours > task.totalHours) {
      Left(TaskError.COMPLETED_HOURS_EXCEED_TOTAL)
    } else if (task.completedHours < 0) {
      Left(TaskError.NEGATIVE_COMPLETED_HOURS)
    } else if (task.deadline.isBefore(task.startDate)) {
      Left(TaskError.DEADLINE_BEFORE_START)
    } else if (task.canSplit && task.minBlockMinutes <= 0) {
      Left(TaskError.INVALID_MIN_BLOCK_MINUTES)
    } else {
      Right(task)
    }
  }

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
