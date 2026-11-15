package service

import model.{Priority, Task, TaskError, TaskStatus}
import validation.TaskValidator.{validateCancellation, validateTaskResult}

import java.time.LocalDate

object TaskManager {
  def addTaskResult(tasks: List[Task], task: Task): Either[TaskError, List[Task]] = {
    validateTaskResult(task)
      .map(validTask => tasks :+ validTask)
  }

  def replaceTask(tasks: List[Task], updatedTask: Task): List[Task] = {
    tasks.map(currentTask =>
      if (currentTask.id == updatedTask.id) {
        updatedTask
      }
      else {
        currentTask
      }
    )
  }

  def changeTotalHoursResult(task: Task, newTotalHours: Double): Either[TaskError, Task] = {
    val newStatus =
      if (task.status == TaskStatus.CANCELLED) {
        TaskStatus.CANCELLED
      } else {
        Task.getInitialStatus(newTotalHours, task.completedHours)
      }

    val newMinBlockMinutes =
      if (task.canSplit) {
        task.minBlockMinutes
      } else {
        ((newTotalHours - task.completedHours) * 60).toInt
      }

    val updatedTask = task.copy(
      totalHours = newTotalHours,
      minBlockMinutes = newMinBlockMinutes,
      status = newStatus
    )

    validateTaskResult(updatedTask)
  }

  def changeCompletedHoursResult(task: Task, newCompletedHours: Double): Either[TaskError, Task] = {
    val newStatus =
      if (task.status == TaskStatus.CANCELLED) {
        TaskStatus.CANCELLED
      } else {
        Task.getInitialStatus(task.totalHours, newCompletedHours)
      }

    val newMinBlockMinutes =
      if (task.canSplit) {
        task.minBlockMinutes
      } else {
        ((task.totalHours - newCompletedHours) * 60).toInt
      }

    val updatedTask = task.copy(
      completedHours = newCompletedHours,
      minBlockMinutes = newMinBlockMinutes,
      status = newStatus
    )

    validateTaskResult(updatedTask)
  }

  def changeStartDateResult(task: Task, newStartDate: LocalDate): Either[TaskError, Task] = {
    val newStatus = getStatusAfterEdit(task)
    val updatedTask = task.copy(
      startDate = newStartDate,
      status = newStatus
    )

    validateTaskResult(updatedTask)
  }

  def changeDeadlineResult(task: Task, newDeadline: LocalDate): Either[TaskError, Task] = {
    val newStatus = getStatusAfterEdit(task)
    val updatedTask = task.copy(
      deadline = newDeadline,
      status = newStatus
    )

    validateTaskResult(updatedTask)
  }

  def changeName(task: Task, newName: String): Task = {
    task.copy(name = newName)
  }

  def changePriority(task: Task, newPriority: Priority): Task = {
    val newStatus = getStatusAfterPlanningEdit(task)

    task.copy(
      priority = newPriority,
      status = newStatus
    )
  }

  def enableSplittingResult(task: Task, minBlockMinutes: Int): Either[TaskError, Task] = {
    val newStatus = getStatusAfterPlanningEdit(task)

    val updatedTask = task.copy(
      canSplit = true,
      minBlockMinutes = minBlockMinutes,
      status = newStatus
    )

    validateTaskResult(updatedTask)
  }

  def disableSplittingResult(task: Task): Either[TaskError, Task] = {
    val newStatus = getStatusAfterPlanningEdit(task)
    val newMinBlockMinutes =
      ((task.totalHours - task.completedHours) * 60).toInt

    val updatedTask = task.copy(
      canSplit = false,
      minBlockMinutes = newMinBlockMinutes,
      status = newStatus
    )

    validateTaskResult(updatedTask)
  }

  def changeMinBlockMinutesResult(task: Task, newMinBlockMinutes: Int): Either[TaskError, Task] = {
    if (!task.canSplit) {
      Left(TaskError.TASK_NOT_SPLITTABLE(task.id))
    } else {
      val newStatus = getStatusAfterPlanningEdit(task)
      val updatedTask = task.copy(
        minBlockMinutes = newMinBlockMinutes,
        status = newStatus
      )

      validateTaskResult(updatedTask)
    }
  }

  // функция проверяет есть ли такая-то задача, tasks.find() - это у нас Option
  // поэтому получив результат Option мы отправляем его в Either, а точнее в Right если прошло
  // проверку, потому что здесь находится наш результат, иначе оно отправляется в Left
  // после чего с помощью map мы создаем список без дела с найденным ID, если поиск
  // завершился ошибкой, то map не выполнится
  def deleteTaskResult(tasks: List[Task], id: Int): Either[TaskError, List[Task]] = {
    tasks.find(task => task.id == id)
      .toRight(TaskError.TASK_NOT_FOUND(id))
      .map(foundTask => tasks.filter(task => task.id != foundTask.id))
  }

  // функция проверяет есть ли такая-то задача также, как и функции выше
  // опять таки tasks.find() это у нас Option, который выдаст Some() и None,
  // найденное Some(task) оно записывает в Right(task), после чего проверяет возможность
  // отмены. flatmap я использую по такой же причине, что и в прошлых функциях, дабы
  // избежать ситуации Either[что-то, Either]. После чего 2 map'а у нас занимаются:
  // 1) если дело найдено и прошло проверку -> создает новый список
  // 2) к каждому дело определяет либо новую, либо исходную его версии
  def cancelTaskResult(tasks: List[Task], id: Int): Either[TaskError, List[Task]] = {
    tasks.find(task => task.id == id)
      .toRight(TaskError.TASK_NOT_FOUND(id))
      .flatMap(foundTask => validateCancellation(foundTask))
      .map(validTask =>
        tasks.map(currentTask =>
          if (currentTask.id == validTask.id) {
            currentTask.copy(status = TaskStatus.CANCELLED)
          } else {
            currentTask
          }
        )
      )
  }

  def getStatusAfterEdit(task: Task): TaskStatus = {
    if (task.status == TaskStatus.CANCELLED) {
      TaskStatus.CANCELLED
    } else {
      Task.getInitialStatus(task.totalHours, task.completedHours)
    }
  }

  def getStatusAfterPlanningEdit(task: Task): TaskStatus = {
    if (task.status == TaskStatus.UNPLACED) {
      Task.getInitialStatus(task.totalHours, task.completedHours)
    } else {
      task.status
    }
  }
}
