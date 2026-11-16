package console

import console.InputUtils.{readBooleanOption, readDateOption, readDoubleOption, readIntOption, readLineOption, readPriorityOption}
import console.TaskErrorPrinter.printTaskError
import model.{Task, TaskError}

import java.time.LocalDate

object TaskInput {
  def readTotalHoursOption(): Option[Double] = {
    println("Введите трудоёмкость в часах:")
    var totalHours = readDoubleOption()

    while (totalHours.exists(hours => hours <= 0)) {
      printTaskError(TaskError.INVALID_TOTAL_HOURS)
      println("Введите трудоёмкость в часах:")
      totalHours = readDoubleOption()
    }

    totalHours
  }

  def readCompletedHoursOption(totalHours: Double): Option[Double] = {
    println("Сколько часов уже выполнено:")
    var completedHours = readDoubleOption()

    while (completedHours.exists(hours => hours < 0 || hours > totalHours)) {
      if (completedHours.exists(hours => hours < 0)) {
        printTaskError(TaskError.NEGATIVE_COMPLETED_HOURS)
      } else {
        printTaskError(TaskError.COMPLETED_HOURS_EXCEED_TOTAL)
      }

      println("Сколько часов уже выполнено:")
      completedHours = readDoubleOption()
    }

    completedHours
  }

  def readDeadlineOption(startDate: LocalDate): Option[LocalDate] = {
    println("Введите дату срока (ГГГГ-ММ-ДД): ")
    var deadline = readDateOption()

    while (deadline.exists(date => date.isBefore(startDate))) {
      printTaskError(TaskError.DEADLINE_BEFORE_START)
      println("Введите дату срока (ГГГГ-ММ-ДД): ")
      deadline = readDateOption()
    }

    deadline
  }

  def readMinBlockMinutesOption(): Option[Int] = {
    println("Введите минимальную продолжительность блока в минутах: ")
    var minutes = readIntOption()

    while (minutes.exists(value => value <= 0)) {
      printTaskError(TaskError.INVALID_MIN_BLOCK_MINUTES)
      println("Введите минимальную продолжительность блока в минутах: ")
      minutes = readIntOption()
    }

    minutes
  }

  // собираем задачу, последовательно читая её поля через Option
  // каждый метод чтения возвращает Some(значение) или None, если ввод закончился
  // запись name <- readLineOption() означает, что если мы получили Some("Учёба"),
  // переменная name содержит строку "Учёба", затем считывается трудоёмкость и т.д.
  // если любой шаг вернул None, остальные шаги for и блок yield пропускаются
  // например, если трудоёмкость не получена, часы выполнения и даты уже не спрашиваем
  // результат всей функции в таком случае - None
  // когда все значения получены, выполняется yield, т.е. вычисляем статус и создаём Task
  // результат всей цепочки при этом - Some(созданная задача)
  // Some(false) тоже содержит полученный ответ - «нет», поэтому цепочка продолжается
  // для неделимого дела блок вычисляется; Some(...) передаёт это число в цепочку Option
  // for/yield - это буквально запись той же последовательности flatMap и map, но удобнее
  def readTaskOption(id: Int): Option[Task] = {
    println("Введите название дела:")

    for {
      name <- readLineOption()
      totalHours <- readTotalHoursOption()
      completedHours <- readCompletedHoursOption(totalHours)
      startDate <- {
        println("Введите дату начала (ГГГГ-ММ-ДД): ")
        readDateOption()
      }
      deadline <- readDeadlineOption(startDate)
      priority <- {
        println("Выберите приоритет выполнения:\n1)Низкий\n2)Средний\n3)Высокий\n4)Наивысший")
        readPriorityOption()
      }
      canSplit <- {
        println("Можно ли разделять дело на блоки?\n1)Да\n2)Нет")
        readBooleanOption()
      }
      minBlockMinutes <- if (canSplit) {
        readMinBlockMinutesOption()
      } else {
        Some(((totalHours - completedHours) * 60).toInt)
      }
    } yield {
      val status = Task.getInitialStatus(totalHours, completedHours)

      Task(
        id,
        name,
        totalHours,
        completedHours,
        startDate,
        deadline,
        priority,
        canSplit,
        minBlockMinutes,
        status
      )
    }
  }
}