import scala.io.StdIn.readLine
import InputUtils.{readBoolean, readDate, readDouble, readInt, readPriority}

object Main {
  def replaceTask(tasks: List[Task], updatedTask: Task): List[Task] = {
    tasks.map(currentTask =>
      if (currentTask.id == updatedTask.id){
        updatedTask
      }
      else {
        currentTask
      }
    )
  }

  def printTask(task: Task): Unit = {
    println("ID: " + task.id)
    println("Название: " + task.name)
    println("Описание дела: " + task.description)
    println("Трудоёмкость: " + task.totalHours + " часов")
    println("Выполнено: " + task.completedHours + " часов")
    println("Осталось: " + (task.totalHours - task.completedHours) + " часов")
    println("Дата начала: " + task.startDate)
    println("Срок до: " + task.deadline)
    println("Приоритет выполнения: " + task.priority)
    println("Можно делить: " + task.canSplit)
    if (task.canSplit) {
      println("Минимальный блок: " + task.minBlockMinutes + " минут")
    }
    println("Состояние: " + task.status + "\n")
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

  def createTask(id: Int): Task = {
    println("Введите название дела:")
    val name = readLine()

    println("Введите описание дела:")
    val description = readLine()

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
      description,
      totalHours,
      completedHours,
      startDate,
      deadline,
      priority,
      canSplit,
      minBlockMinutes,
      status)
  }

  def main(args: Array[String]): Unit = {
    var tasks = List.empty[Task]
    var running = true
    var nextId = 1

    while (running){
      println("Меню:")
      println("1. Добавить дело")
      println("2. Показать дела")
      println("3. Изменить дело")
      println("4. Удалить дело")
      println("0. Выход")

      val choice = readInt()

      choice match{
        case 1 =>
          println("Добавление дела")
          val task = createTask(nextId)

          if (validateTask(task)) {
            tasks = tasks :+ task
            println("Дело успешно добавлено.")
            nextId += 1
          }
        case 2 =>
          if (tasks.isEmpty){
            println("Список дел пуст.\n")
          }else{
            println("\nСписок сохранённых дел: \n")
            for (task <- tasks) {
              printTask(task)
            }
          }
        case 3 =>
          if (tasks.isEmpty){
            println("Список дел пуст.\n")
          }else {
            println("Введите ID дела для изменения: ")
            val id = readInt()

            val foundTask = tasks.find(task => task.id == id)
            // а вот и материал, который нам на лекции поясняли, подъехал
            foundTask match {
              case Some(task) =>
                println("Найдено дело: ")
                printTask(task)

                var currentTask = task
                var editing = true
                while (editing){
                  println("Что изменить?\n" +
                    "\t1) Название\n" +
                    "\t2) Описание\n" +
                    "\t3) Трудоёмкость\n" +
                    "\t4) Выполненные часы\n" +
                    "\t5) Дата начала\n" +
                    "\t6) Дедлайн\n" +
                    "\t7) Приоритет\n" +
                    "\t8) Возможность разделения на блоки\n" +
                    "\t9) Минимальная продолжительность блока\n" +
                    "\t0) Завершить изменение")
                  val editChoice = readInt()
                  editChoice match {
                    case 1 =>
                      println("Введите новое название: ")
                      val newName = readLine()
                      val updatedTask = currentTask.copy(name = newName)

                      tasks = replaceTask(tasks, updatedTask)
                      currentTask = updatedTask

                      println("Название изменено успешно.")
                      println("Измененное дело: ")
                      printTask(updatedTask)
                    case 2 =>
                      println("Введите новое описание: ")
                      val newDescription = readLine()
                      val updatedTask = currentTask.copy(description = newDescription)

                      tasks = replaceTask(tasks, updatedTask)
                      currentTask = updatedTask

                      println("Описание изменено успешно.")
                      println("Измененное дело: ")
                      printTask(updatedTask)
                    case 3 =>
                      println("Введите новую трудоёмкость в часах: ")
                      val newTotalHours = readDouble()

                      val newStatus = Task.getInitialStatus(newTotalHours, currentTask.completedHours)

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
                        tasks = replaceTask(tasks, updatedTask)
                        currentTask = updatedTask

                        println("Трудоёмкость изменена успешно.")
                        println("Измененное дело: ")
                        printTask(updatedTask)
                      }
                    case 4 =>
                      println("Введите количество выполненных часов: ")
                      val newCompletedHours = readDouble()

                      val newStatus = Task.getInitialStatus(currentTask.totalHours, newCompletedHours)

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
                        tasks = replaceTask(tasks, updatedTask)
                        currentTask = updatedTask

                        println("Выполненные часы изменены успешно.")
                        println("Измененное дело: ")
                        printTask(updatedTask)
                      }
                    case 5 =>
                      println("Введите новую дату начала (ГГГГ-ММ-ДД): ")
                      val newStartDate = readDate()

                      val updatedTask = currentTask.copy(startDate = newStartDate)

                      if (validateTask(updatedTask)) {
                        tasks = replaceTask(tasks, updatedTask)
                        currentTask = updatedTask

                        println("Дата начала изменена успешно.")
                        println("Измененное дело: ")
                        printTask(updatedTask)
                      }
                    case 6 =>
                      println("Введите новый дедлайн (ГГГГ-ММ-ДД): ")
                      val newDeadline = readDate()

                      val updatedTask = currentTask.copy(deadline = newDeadline)

                      if (validateTask(updatedTask)) {
                        tasks = replaceTask(tasks, updatedTask)
                        currentTask = updatedTask

                        println("Дедлайн изменен успешно.")
                        println("Измененное дело: ")
                        printTask(updatedTask)
                      }
                    case 7 =>
                      println("Выберите новый приоритет:\n" +
                        "1) Низкий\n" +
                        "2) Средний\n" +
                        "3) Высокий\n" +
                        "4) Наивысший")

                      val newPriority = readPriority()

                      val updatedTask = currentTask.copy(priority = newPriority)

                      tasks = replaceTask(tasks, updatedTask)
                      currentTask = updatedTask

                      println("Приоритет изменен успешно.")
                      println("Измененное дело: ")
                      printTask(updatedTask)
                    case 8 =>
                      println("Можно ли разделять дело на блоки?\n" +
                        "1) Да\n" +
                        "2) Нет")
                      val newCanSplit = readBoolean()

                      val newMinBlockMinutes =
                        if (newCanSplit){
                          println("Введите минимальную продолжительность блока в минутах: ")
                          readInt()
                        } else {
                          ((currentTask.totalHours - currentTask.completedHours) * 60).toInt
                        }

                      val updatedTask = currentTask.copy(
                        canSplit = newCanSplit,
                        minBlockMinutes = newMinBlockMinutes
                      )

                      if (validateTask(updatedTask)) {
                        tasks = replaceTask(tasks, updatedTask)
                        currentTask = updatedTask

                        println("Возможность разделения на блоки изменена успешно.")
                        println("Измененное дело: ")
                        printTask(updatedTask)
                      }
                    case 9 =>
                      if (currentTask.canSplit){
                        println("Введите новую минимальную продолжительность блока в минутах: ")
                        val newMinBlockMinutes = readInt()

                        val updatedTask = currentTask.copy(
                          minBlockMinutes = newMinBlockMinutes
                        )

                        if (validateTask(updatedTask)) {
                          tasks = replaceTask(tasks, updatedTask)
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
                      println("Ошибка: выберите пункт от 0 до 9.")
                  }
                }
              case None =>
                println("Дело с таким ID не найдено.")
            }
          }
        case 4 =>
          if (tasks.isEmpty){
            println("Список дел пуст.\n")
          }else {
            println("Введите ID дела для удаления: ")
            val id = readInt()

            val oldSize = tasks.length

            tasks = tasks.filter(task => task.id != id)

            if (tasks.length < oldSize){
              println("Дело успешно удалено.")
            } else {
              println("Дело с таким ID не найдено.")
            }
          }
        case 0 => running = false
        case _ => println("Ошибка: выберите пункт от 0 до 4.")
      }
    }
  }
}