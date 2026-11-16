package console

import console.InputErrorPrinter.printInputError
import console.InputUtils.{readBoolean, readDate, readDouble, readInt, readIntOption, readLineOption, readPriorityOption}
import console.TaskErrorPrinter.printTaskError
import console.TaskPrinter.printTask
import model.{Task, TaskEditAction, TaskError}
import service.TaskManager.{changeCompletedHoursResult, changeDeadlineResult, changeMinBlockMinutesResult, changeName, changePriority, changeStartDateResult, changeTotalHoursResult, disableSplittingResult, enableSplittingResult, replaceTask}

object TaskEditor {
  def editTask(tasks: List[Task], onEndOfInput: () => Unit): List[Task] = {
    var updatedTasks = tasks

    if (updatedTasks.isEmpty) {
      println("Список дел пуст.\n")
    } else {
      println("Введите ID дела для изменения: ")
      val id = readIntOption() match {
        case Some(value) =>
          value
        case None =>
          onEndOfInput()
          return updatedTasks
      }

      val foundTask = updatedTasks.find(task => task.id == id)
      foundTask match {
        case Some(task) =>
          println("Найдено дело: ")
          printTask(task)

          var currentTask = task
          var editing = true
          while (editing) {
            println("Что изменить?\n" +
              "\t1) Название\n" +
              "\t2) Трудоёмкость\n" +
              "\t3) Выполненные часы\n" +
              "\t4) Дата начала\n" +
              "\t5) Дедлайн\n" +
              "\t6) Приоритет\n" +
              "\t7) Возможность разделения на блоки\n" +
              "\t8) Минимальная продолжительность блока\n" +
              "\t0) Завершить изменение")
            val editActionResult = readIntOption() match {
              case Some(choice) =>
                TaskEditActionParser.parse(choice)
              case None =>
                onEndOfInput()
                return updatedTasks
            }
            editActionResult match {
              case Right(TaskEditAction.CHANGE_NAME) =>
                println("Введите новое название: ")
                val newName = readLineOption() match {
                  case Some(value) =>
                    value
                  case None =>
                    onEndOfInput()
                    return updatedTasks
                }
                val updatedTask = changeName(currentTask, newName)

                updatedTasks = replaceTask(updatedTasks, updatedTask)
                currentTask = updatedTask

                println("Название изменено успешно.")
                println("Измененное дело: ")
                printTask(updatedTask)
              case Right(TaskEditAction.CHANGE_TOTAL_HOURS) =>
                println("Введите новую трудоёмкость в часах: ")
                var newTotalHours = readDouble()

                while (newTotalHours <= 0 || newTotalHours < currentTask.completedHours) {
                  if (newTotalHours <= 0) {
                    println("Ошибка: трудоёмкость должна быть больше нуля.")
                  } else {
                    println("Ошибка: трудоёмкость не может быть меньше уже выполненного количества часов.")
                  }
                  println("Введите новую трудоёмкость в часах: ")
                  newTotalHours = readDouble()
                }

                changeTotalHoursResult(currentTask, newTotalHours) match {
                  // опять же проверка на валидность, если все круто, все происходит как было
                  // изначально в лр0, далее заменим все исходные однотипные проверки на это
                  case Right(validTask) =>
                    updatedTasks = replaceTask(updatedTasks, validTask)
                    currentTask = validTask

                    println("Трудоёмкость изменена успешно.")
                    println("Измененное дело: ")
                    printTask(validTask)
                  // если проверка провалилась, то пользователю выводится причина ошибки
                  case Left(error) =>
                    printTaskError(error)
                }
              case Right(TaskEditAction.CHANGE_COMPLETED_HOURS) =>
                println("Введите количество выполненных часов: ")
                var newCompletedHours = readDouble()

                while (newCompletedHours < 0 || newCompletedHours > currentTask.totalHours) {
                  if (newCompletedHours < 0) {
                    println("Ошибка: количество выполненных часов не может быть отрицательным.")
                  } else {
                    println("Ошибка: количество выполненных часов не может превышать общую трудоёмкость.")
                  }

                  println("Введите количество выполненных часов: ")
                  newCompletedHours = readDouble()
                }

                changeCompletedHoursResult(currentTask, newCompletedHours) match {
                  case Right(validTask) =>
                    updatedTasks = replaceTask(updatedTasks, validTask)
                    currentTask = validTask

                    println("Выполненные часы изменены успешно.")
                    println("Измененное дело: ")
                    printTask(validTask)
                  case Left(error) =>
                    printTaskError(error)
                }
              case Right(TaskEditAction.CHANGE_START_DATE) =>
                println("Введите новую дату начала (ГГГГ-ММ-ДД): ")
                var newStartDate = readDate()

                while (newStartDate.isAfter(currentTask.deadline)) {
                  println("Ошибка: дата начала не может быть позже дедлайна.")
                  println("Введите новую дату начала (ГГГГ-ММ-ДД): ")
                  newStartDate = readDate()
                }

                changeStartDateResult(currentTask, newStartDate) match {
                  case Right(validTask) =>
                    updatedTasks = replaceTask(updatedTasks, validTask)
                    currentTask = validTask

                    println("Дата начала изменена успешно.")
                    println("Измененное дело: ")
                    printTask(validTask)
                  case Left(error) =>
                    printTaskError(error)
                }
              case Right(TaskEditAction.CHANGE_DEADLINE) =>
                println("Введите новый дедлайн (ГГГГ-ММ-ДД): ")
                var newDeadline = readDate()

                while (newDeadline.isBefore(currentTask.startDate)) {
                  println("Ошибка: дедлайн не может быть раньше даты начала.")
                  println("Введите новый дедлайн (ГГГГ-ММ-ДД): ")
                  newDeadline = readDate()
                }

                changeDeadlineResult(currentTask, newDeadline) match {
                  case Right(validTask) =>
                    updatedTasks = replaceTask(updatedTasks, validTask)
                    currentTask = validTask

                    println("Дедлайн изменен успешно.")
                    println("Измененное дело: ")
                    printTask(validTask)
                  case Left(error) =>
                    printTaskError(error)
                }
              case Right(TaskEditAction.CHANGE_PRIORITY) =>
                println("Выберите новый приоритет:\n" +
                  "1) Низкий\n" +
                  "2) Средний\n" +
                  "3) Высокий\n" +
                  "4) Наивысший")

                val newPriority = readPriorityOption() match {
                  case Some(value) =>
                    value
                  case None =>
                    onEndOfInput()
                    return updatedTasks
                }

                val updatedTask = changePriority(currentTask, newPriority)

                updatedTasks = replaceTask(updatedTasks, updatedTask)
                currentTask = updatedTask

                println("Приоритет изменен успешно.")
                println("Измененное дело: ")
                printTask(updatedTask)
              case Right(TaskEditAction.CHANGE_CAN_SPLIT) =>
                println("Можно ли разделять дело на блоки?\n" +
                  "1) Да\n" +
                  "2) Нет")
                val newCanSplit = readBoolean()

                val splittingResult =
                  if (newCanSplit) {
                    println("Введите минимальную продолжительность блока в минутах: ")
                    var minutes = readInt()

                    while (minutes <= 0) {
                      println("Ошибка: минимальная продолжительность блока должна быть больше нуля.")
                      println("Введите минимальную продолжительность блока в минутах: ")
                      minutes = readInt()
                    }

                    enableSplittingResult(currentTask, minutes)
                  } else {
                    disableSplittingResult(currentTask)
                  }

                splittingResult match {
                  case Right(validTask) =>
                    updatedTasks = replaceTask(updatedTasks, validTask)
                    currentTask = validTask

                    println("Возможность разделения на блоки изменена успешно.")
                    println("Измененное дело: ")
                    printTask(validTask)
                  case Left(error) =>
                    printTaskError(error)
                }
              case Right(TaskEditAction.CHANGE_MIN_BLOCK_MINUTES) =>
                if (currentTask.canSplit) {
                  println("Введите новую минимальную продолжительность блока в минутах: ")
                  var newMinBlockMinutes = readInt()

                  while (newMinBlockMinutes <= 0) {
                    println("Ошибка: минимальная продолжительность блока должна быть больше нуля.")
                    println("Введите новую минимальную продолжительность блока в минутах: ")
                    newMinBlockMinutes = readInt()
                  }

                  changeMinBlockMinutesResult(currentTask, newMinBlockMinutes) match {
                    case Right(validTask) =>
                      updatedTasks = replaceTask(updatedTasks, validTask)
                      currentTask = validTask

                      println("Минимальная продолжительность блока изменена успешно.")
                      println("Измененное дело: ")
                      printTask(validTask)
                    case Left(error) =>
                      printTaskError(error)
                  }
                } else {
                  printTaskError(TaskError.TASK_NOT_SPLITTABLE(currentTask.id))
                }
              case Right(TaskEditAction.FINISH_EDITING) =>
                println("Изменение завершено.")
                editing = false
              case Left(error) =>
                printInputError(error)
            }
          }
        case None =>
          println("Дело с таким ID не найдено.")
      }
    }

    updatedTasks
  }
}