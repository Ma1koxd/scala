import scala.io.StdIn.readLine
import java.time.format.DateTimeParseException
import java.time.LocalDate

object Main {
  def readBoolean(): Boolean = {
    var valid = false
    var result = false

    while(!valid){
      val choice = readInt()
      choice match {
        case 1 =>
          result = true
          valid = true
        case 2 =>
          result = false
          valid = true
        case _ =>
          println("Ошибка: введите число от 1 до 2")
      }
    }
    result
  }

  def readDate(): LocalDate = {
    var valid = false
    var result = LocalDate.of(2000,1,1)
    while(!valid){
      try{
        val date = LocalDate.parse(readLine())
        result = date
        valid = true
      }catch{
        case _: DateTimeParseException =>
          println("Ошибка: введите дату в формате ГГГГ-ММ-ДД")
      }
    }
    result
  }

  def readPriority(): Priority = {
    var valid = false
    var result = Priority.LOW

    while(!valid){
      val choice = readInt()
      choice match {
        case 1 =>
          result = Priority.LOW
          valid = true
        case 2 =>
          result = Priority.NORMAL
          valid = true
        case 3 =>
          result = Priority.HIGH
          valid = true
        case 4 =>
          result = Priority.CRITICAL
          valid = true
        case _ =>
          println("Ошибка: введите число от 1 до 4.")
      }
    }
    result
  }

  def readInt(): Int = {
    var result = 0
    var valid = false
    
    while (!valid) {
      try {
        val task = readLine().toInt
        result = task
        valid = true
      } catch {
        case _: NumberFormatException =>
          println("Ошибка: это не целое число")
      }
    }
    result
  }

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
        val totalHours = readInt()

        println("Сколько часов уже выполнено:")
        val completedHours = readInt()

        println("Введите дату начала: ")
        val startDate = readDate()

        println("Введите дату срока: ")
        val deadline = readDate()

        println("Выберите приоритет выполнения:\n1)Низкий\n2)Средний\n3)Высокий\n4)Наивысший")
        val priority = readPriority()

        println("Можно ли разделять дело на части?\n1)Да\n2)Нет")
        val canSplit = readBoolean()

        val status = TaskStatus.PLANNED

        val task = Task(id, name, description, totalHours, completedHours, startDate, deadline, priority, canSplit, status)

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
      println("Минимальный блок: " + "\n")
    }
  }
}