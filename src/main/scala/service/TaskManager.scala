package service

import console.InputErrorPrinter.printInputError
import console.InputUtils.{readBoolean, readDate, readDouble, readInt, readPriority}
import console.TaskEditActionParser
import console.TaskErrorPrinter.printTaskError
import console.TaskPrinter.printTask
import model.{Task, TaskEditAction, TaskError, TaskStatus}
import validation.TaskValidator.validateTaskResult

import scala.io.StdIn.readLine

object TaskManager {
  def createTask(id: Int): Task = {
    println("Введите название дела:")
    val name = readLine()

    println("Введите трудоёмкость в часах:")
    var totalHours = readDouble()

    while (totalHours <= 0) {
      println("Ошибка: трудоёмкость должна быть больше нуля.")
      println("Введите трудоёмкость в часах:")
      totalHours = readDouble()
    }

    println("Сколько часов уже выполнено:")
    var completedHours = readDouble()

    while (completedHours < 0 || completedHours > totalHours) {
      if (completedHours < 0) {
        println("Ошибка: количество выполненных часов не может быть отрицательным.")
      } else {
        println("Ошибка: количество выполненных часов не может превышать общую трудоёмкость.")
      }

      println("Сколько часов уже выполнено:")
      completedHours = readDouble()
    }

    println("Введите дату начала (ГГГГ-ММ-ДД): ")
    val startDate = readDate()

    println("Введите дату срока (ГГГГ-ММ-ДД): ")
    var deadline = readDate()

    while (deadline.isBefore(startDate)) {
      println("Ошибка: дедлайн не может быть раньше даты начала.")
      println("Введите дату срока (ГГГГ-ММ-ДД): ")
      deadline = readDate()
    }

    println("Выберите приоритет выполнения:\n1)Низкий\n2)Средний\n3)Высокий\n4)Наивысший")
    val priority = readPriority()

    println("Можно ли разделять дело на блоки?\n1)Да\n2)Нет")
    val canSplit = readBoolean()

    val minBlockMinutes = if (canSplit) {
      println("Введите минимальную продолжительность блока в минутах: ")
      var minutes = readInt()

      while (minutes <= 0) {
        println("Ошибка: минимальная продолжительность блока должна быть больше нуля.")
        println("Введите минимальную продолжительность блока в минутах: ")
        minutes = readInt()
      }
      minutes
    } else {
      ((totalHours - completedHours) * 60).toInt
    }

    val status = Task.getInitialStatus(totalHours, completedHours)

    Task(id,
      name,
      totalHours,
      completedHours,
      startDate,
      deadline,
      priority,
      canSplit,
      minBlockMinutes,
      status)
  }

  def addTask(tasks: List[Task], nextId: Int): (List[Task], Int) = {
    println("Добавление дела")
    val task = createTask(nextId)

    validateTaskResult(task) match {
      // насколько я понял смысл данной конструкции в ADT и Either, в том, что если проверка
      // вернула успешный результат, то у нас validTask получает дело, которое внутри Right
      case Right(validTask) =>
        val updatedTasks = tasks :+ validTask
        println("Дело успешно добавлено.")
        (updatedTasks, nextId + 1)
      // здесь же у нас неудачная проверка и мы получаем вместо дела,
      // причину ошибки и ее расшифрование, отображаемое пользователю, с помощью
      // функции printTaskError
      case Left(error) =>
        printTaskError(error)
        (tasks, nextId)
    }
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

  def editTask(tasks: List[Task]): List[Task] = {
    var updatedTasks = tasks

    if (updatedTasks.isEmpty) {
      println("Список дел пуст.\n")
    } else {
      println("Введите ID дела для изменения: ")
      val id = readInt()

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
            val editActionResult = TaskEditActionParser.parse(readInt())
            editActionResult match {
              case Right(TaskEditAction.CHANGE_NAME) =>
                println("Введите новое название: ")
                val newName = readLine()
                val updatedTask = currentTask.copy(name = newName)

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

                val newStatus =
                  if (currentTask.status == TaskStatus.CANCELLED) {
                    TaskStatus.CANCELLED
                  } else {
                    Task.getInitialStatus(newTotalHours, currentTask.completedHours)
                  }

                val newMinBlockMinutes =
                  if (currentTask.canSplit) {
                    currentTask.minBlockMinutes
                  } else {
                    ((newTotalHours - currentTask.completedHours) * 60).toInt
                  }

                val updatedTask = currentTask.copy(
                  totalHours = newTotalHours,
                  minBlockMinutes = newMinBlockMinutes,
                  status = newStatus
                )

                validateTaskResult(updatedTask) match {
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

                val newStatus =
                  if (currentTask.status == TaskStatus.CANCELLED) {
                    TaskStatus.CANCELLED
                  } else {
                    Task.getInitialStatus(currentTask.totalHours, newCompletedHours)
                  }

                val newMinBlockMinutes =
                  if (currentTask.canSplit) {
                    currentTask.minBlockMinutes
                  } else {
                    ((currentTask.totalHours - newCompletedHours) * 60).toInt
                  }

                val updatedTask = currentTask.copy(
                  completedHours = newCompletedHours,
                  minBlockMinutes = newMinBlockMinutes,
                  status = newStatus
                )

                validateTaskResult(updatedTask) match {
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

                val newStatus = getStatusAfterEdit(currentTask)

                val updatedTask = currentTask.copy(
                  startDate = newStartDate,
                  status = newStatus
                )

                validateTaskResult(updatedTask) match {
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

                val newStatus = getStatusAfterEdit(currentTask)

                val updatedTask = currentTask.copy(
                  deadline = newDeadline,
                  status = newStatus
                )

                validateTaskResult(updatedTask) match {
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

                val newPriority = readPriority()

                val newStatus = getStatusAfterPlanningEdit(currentTask)
                val updatedTask = currentTask.copy(priority = newPriority, status = newStatus)

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

                val newMinBlockMinutes =
                  if (newCanSplit) {
                    println("Введите минимальную продолжительность блока в минутах: ")
                    var minutes = readInt()

                    while (minutes <= 0) {
                      println("Ошибка: минимальная продолжительность блока должна быть больше нуля.")
                      println("Введите минимальную продолжительность блока в минутах: ")
                      minutes = readInt()
                    }
                    minutes
                  } else {
                    ((currentTask.totalHours - currentTask.completedHours) * 60).toInt
                  }

                val newStatus = getStatusAfterPlanningEdit(currentTask)
                val updatedTask = currentTask.copy(
                  canSplit = newCanSplit,
                  minBlockMinutes = newMinBlockMinutes,
                  status = newStatus
                )

                validateTaskResult(updatedTask) match {
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

                  val newStatus = getStatusAfterPlanningEdit(currentTask)
                  val updatedTask = currentTask.copy(
                    minBlockMinutes = newMinBlockMinutes,
                    status = newStatus
                  )

                  validateTaskResult(updatedTask) match {
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
                  println("Дело нельзя разделять на блоки.")
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

  def cancelTask(tasks: List[Task]): List[Task] = {
    if (tasks.isEmpty) {
      println("Список дел пуст.")
      tasks
    } else {
      println("Введите ID дела для отмены:")
      val id = readInt()

      tasks.find(task => task.id == id) match {
        case Some(task) =>
          if (task.status == TaskStatus.COMPLETED) {
            println("Нельзя отменить уже завершенное дело.")
            tasks
          } else if (task.status == TaskStatus.CANCELLED) {
            println("Дело уже отменено.")
            tasks
          } else {
            val updatedTasks = tasks.map(currentTask =>
              if (currentTask.id == id) {
                currentTask.copy(status = TaskStatus.CANCELLED)
              } else {
                currentTask
              }
            )

            println("Дело отменено.")
            updatedTasks
          }

        case None =>
          println("Дело с таким ID не найдено.")
          tasks
      }
    }
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
