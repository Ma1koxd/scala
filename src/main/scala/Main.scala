import scala.io.StdIn.readLine
import InputUtils.{readInt, readDouble, readDate, readPriority, readBoolean}

object Main {
  def main(args: Array[String]): Unit = {
    var tasks = List.empty[Task]

    println("Введите количество дел:")
    val taskCount = readInt()
    if (taskCount > 0){
      var i = 0
      while (i < taskCount) {
        val id = i+1
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

        val minBlockMinutes = if (canSplit){
          println("Введите минимальную продолжительность блока в минутах: ")
          readInt()
        }
        else{
          ((totalHours-completedHours)*60).toInt
        }

        val status = TaskStatus.PLANNED

        val task = Task(id, name, description, totalHours, completedHours, startDate, deadline, priority, canSplit, minBlockMinutes, status)

        if (task.totalHours <= 0) {
          println("Ошибка: трудоёмкость должна быть больше нуля.")
        }
        else if (task.completedHours > task.totalHours) {
          println("Ошибка: выполненное время не может превышать трудоёмкость.")
        }
        else if (task.completedHours < 0) {
          println("Ошибка: выполненное время не может быть отрицательным")
        }
        else if (deadline.isBefore(startDate)) {
          println("Ошибка: дедлайн не может быть раньше даты начала")
        }
        else {
          tasks = tasks :+ task
          println("Дело успешно добавлено.")
        }
        i += 1
      }
    }
    else{
      println("Ошибка: количество дел должно быть больше нуля")
    }
    println("\nСписок сохранённых дел: ")
    for (task <- tasks){
      println("ID: " + task.id)
      println("Название: " + task.name)
      println("Описание дела: " + task.description)
      println("Трудоёмкость: " + task.totalHours + " часов")
      println("Выполнено: " + task.completedHours + " часов")
      println("Осталось: " + (task.totalHours-task.completedHours) + " часов")
      println("Дата начала: " + task.startDate)
      println("Срок до: " + task.deadline)
      println("Приоритет выполнения: " + task.priority)
      println("Можно делить: " + task.canSplit)
      println("Минимальный блок: " + task.minBlockMinutes + " минут\n")
    }
  }
}