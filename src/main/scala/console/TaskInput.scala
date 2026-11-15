package console

import console.InputUtils.{readBoolean, readDate, readDouble, readInt, readPriority}
import model.Task

import scala.io.StdIn.readLine

object TaskInput {
  def readTask(id: Int): Task = {
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
}