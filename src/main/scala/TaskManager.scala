import InputUtils.{readBoolean, readDate, readDouble, readInt, readPriority}
import TaskPrinter.printTask
import TaskValidator.validateTask

import scala.io.StdIn.readLine

object TaskManager {
  def createTask(id: Int): Task = {
    println("Введите название дела:")
    val name = readLine()

    println("Введите трудоёмкость в часах:")
    val totalHours = readDouble()

    println("Сколько часов уже выполнено:")
    val completedHours = readDouble()

    println("Введите дату начала (ГГГГ-ММ-ДД): ")
    val startDate = readDate()

    println("Введите дату срока (ГГГГ-ММ-ДД): ")
    val deadline = readDate()

    println("Выберите приоритет выполнения:\n1)Низкий\n2)Средний\n3)Высокий\n4)Наивысший")
    val priority = readPriority()

    println("Можно ли разделять дело на блоки?\n1)Да\n2)Нет")
    val canSplit = readBoolean()

    val minBlockMinutes = if (canSplit) {
      println("Введите минимальную продолжительность блока в минутах: ")
      readInt()
    }
    else {
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

    if (validateTask(task)) {
      val updatedTasks = tasks :+ task
      println("Дело успешно добавлено.")
      (updatedTasks, nextId + 1)
    } else {
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
            val editChoice = readInt()
            editChoice match {
              case 1 =>
                println("Введите новое название: ")
                val newName = readLine()
                val updatedTask = currentTask.copy(name = newName)

                updatedTasks = replaceTask(updatedTasks, updatedTask)
                currentTask = updatedTask

                println("Название изменено успешно.")
                println("Измененное дело: ")
                printTask(updatedTask)
              case 2 =>
                println("Введите новую трудоёмкость в часах: ")
                val newTotalHours = readDouble()

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

                if (validateTask(updatedTask)) {
                  updatedTasks = replaceTask(updatedTasks, updatedTask)
                  currentTask = updatedTask

                  println("Трудоёмкость изменена успешно.")
                  println("Измененное дело: ")
                  printTask(updatedTask)
                }
              case 3 =>
                println("Введите количество выполненных часов: ")
                val newCompletedHours = readDouble()

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

                if (validateTask(updatedTask)) {
                  updatedTasks = replaceTask(updatedTasks, updatedTask)
                  currentTask = updatedTask

                  println("Выполненные часы изменены успешно.")
                  println("Измененное дело: ")
                  printTask(updatedTask)
                }
              case 4 =>
                println("Введите новую дату начала (ГГГГ-ММ-ДД): ")
                val newStartDate = readDate()

                val newStatus = getStatusAfterEdit(currentTask)

                val updatedTask = currentTask.copy(
                  startDate = newStartDate,
                  status = newStatus
                )

                if (validateTask(updatedTask)) {
                  updatedTasks = replaceTask(updatedTasks, updatedTask)
                  currentTask = updatedTask

                  println("Дата начала изменена успешно.")
                  println("Измененное дело: ")
                  printTask(updatedTask)
                }
              case 5 =>
                println("Введите новый дедлайн (ГГГГ-ММ-ДД): ")
                val newDeadline = readDate()

                val newStatus = getStatusAfterEdit(currentTask)

                val updatedTask = currentTask.copy(
                  deadline = newDeadline,
                  status = newStatus
                )

                if (validateTask(updatedTask)) {
                  updatedTasks = replaceTask(updatedTasks, updatedTask)
                  currentTask = updatedTask

                  println("Дедлайн изменен успешно.")
                  println("Измененное дело: ")
                  printTask(updatedTask)
                }
              case 6 =>
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
              case 7 =>
                println("Можно ли разделять дело на блоки?\n" +
                  "1) Да\n" +
                  "2) Нет")
                val newCanSplit = readBoolean()

                val newMinBlockMinutes =
                  if (newCanSplit) {
                    println("Введите минимальную продолжительность блока в минутах: ")
                    readInt()
                  } else {
                    ((currentTask.totalHours - currentTask.completedHours) * 60).toInt
                  }

                val newStatus = getStatusAfterPlanningEdit(currentTask)
                val updatedTask = currentTask.copy(
                  canSplit = newCanSplit,
                  minBlockMinutes = newMinBlockMinutes,
                  status = newStatus
                )

                if (validateTask(updatedTask)) {
                  updatedTasks = replaceTask(updatedTasks, updatedTask)
                  currentTask = updatedTask

                  println("Возможность разделения на блоки изменена успешно.")
                  println("Измененное дело: ")
                  printTask(updatedTask)
                }
              case 8 =>
                if (currentTask.canSplit) {
                  println("Введите новую минимальную продолжительность блока в минутах: ")
                  val newMinBlockMinutes = readInt()

                  val newStatus = getStatusAfterPlanningEdit(currentTask)
                  val updatedTask = currentTask.copy(
                    minBlockMinutes = newMinBlockMinutes,
                    status = newStatus
                  )

                  if (validateTask(updatedTask)) {
                    updatedTasks = replaceTask(updatedTasks, updatedTask)
                    currentTask = updatedTask

                    println("Минимальная продолжительность блока изменена успешно.")
                    println("Измененное дело: ")
                    printTask(updatedTask)
                  }
                } else {
                  println("Дело нельзя разделять на блоки.")
                }
              case 0 =>
                println("Изменение завершено.")
                editing = false
              case _ =>
                println("Ошибка: выберите пункт от 0 до 8.")
            }
          }
        case None =>
          println("Дело с таким ID не найдено.")
      }
    }

    updatedTasks
  }

  def deleteTask(tasks: List[Task]): List[Task] = {
    var updatedTasks = tasks

    if (updatedTasks.isEmpty) {
      println("Список дел пуст.\n")
    } else {
      println("Введите ID дела для удаления: ")
      val id = readInt()

      val oldSize = updatedTasks.length

      updatedTasks = updatedTasks.filter(task => task.id != id)

      if (updatedTasks.length < oldSize) {
        println("Дело успешно удалено.")
      } else {
        println("Дело с таким ID не найдено.")
      }
    }

    updatedTasks
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
