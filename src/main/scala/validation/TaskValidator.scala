package validation

import model.{Task, TaskError, TaskStatus}

object TaskValidator {
  // данная функция возвращает какая ИМЕННО ошибка возникла или дело, если проверка успешна
  // UPD: в связи с использованием Either прошлый метод из лр0 здесь не нужен
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

  def validateCancellation(task: Task): Either[TaskError, Task] = {
    if (task.status == TaskStatus.COMPLETED) {
      Left(TaskError.CANNOT_CANCEL_COMPLETED_TASK(task.id))
    } else if (task.status == TaskStatus.CANCELLED) {
      Left(TaskError.TASK_ALREADY_CANCELLED(task.id))
    } else {
      Right(task)
    }
  }
}
