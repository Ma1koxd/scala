import scala.io.StdIn.readLine
import InputUtils.{readBoolean, readDate, readDouble, readInt, readPriority}

object Main {
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
          val id = tasks.length + 1
          val task = createTask(id)

          if (validateTask(task)) {
            tasks = tasks :+ task
            println("Дело успешно добавлено.")
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
        case 3 => println("Изменение дела")
        case 4 => println("Удаление дела")
        case 0 => running = false
        case _ => println("Ошибка: выберите пункт от 0 до 4.")
      }
    }
  }
}