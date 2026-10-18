import java.io.{File, PrintWriter}

object ReportManager {
  def buildReport(
                   tasks: List[Task],
                   schedule: List[ScheduledBlock],
                   remainingFreeTimeSlots: List[FreeTimeSlot]
                 ): String = {
    val report = new StringBuilder()

    report.append("ОТЧЁТ ПЛАНИРОВЩИКА\n\n")
    report.append("ДЕЛА\n")

    if (tasks.isEmpty) {
      report.append("Дела отсутствуют.\n")
    } else {
      for (task <- tasks) {
        report.append("ID: " + task.id + "\n")
        report.append("Название: " + task.name + "\n")
        report.append("Всего часов: " + task.totalHours + "\n")
        report.append("Выполнено часов: " + task.completedHours + "\n")
        report.append("Осталось часов: " + task.remainingMinutes / 60.0 + "\n")
        report.append("Статус: " + task.status + "\n")
        report.append("\n")
      }
    }
    report.append("\nРАСПИСАНИЕ\n")

    if (schedule.isEmpty) {
      report.append("Расписание отсутствует.\n")
    } else {
      for (block <- schedule) {
        val taskName = tasks.find(task => task.id == block.taskId) match {
          case Some(task) => task.name
          case None => "Неизвестное дело"
        }

        report.append("Дело: " + taskName + " (ID: " + block.taskId + ")\n")
        report.append("Дата: " + block.date + "\n")
        report.append("Время: " + block.startTime + " - " + block.endTime + "\n")
        report.append("Продолжительность: " + block.durationMinutes / 60.0 + " часов\n")
        report.append("\n")
      }
    }

    report.append("\nОБЩАЯ СТАТИСТИКА\n")

    val activeTasks = tasks.filter(task =>
      task.status != TaskStatus.CANCELLED
    )

    val totalHours = activeTasks.map(task => task.totalHours).sum
    val completedHours = activeTasks.map(task => task.completedHours).sum
    val remainingMinutes = activeTasks.map(task => task.remainingMinutes).sum
    val remainingHours = remainingMinutes / 60.0

    report.append("Всего работы: " + totalHours + " часов\n")
    report.append("Выполнено: " + completedHours + " часов\n")
    report.append("Осталось выполнить: " + remainingHours + " часов\n")

    val unplacedTasks = tasks.filter(task =>
      task.status == TaskStatus.UNPLACED
    )

    report.append("\nНЕРАЗМЕЩЁННЫЕ ДЕЛА\n")

    if (unplacedTasks.isEmpty) {
      report.append("Неразмещённые дела отсутствуют.\n")
    } else {
      for (task <- unplacedTasks) {
        report.append("ID: " + task.id + "\n")
        report.append("Название: " + task.name + "\n")
        report.append("Осталось часов: " + task.remainingMinutes / 60.0 + "\n")
        report.append("\n")
      }

      val totalUnplacedMinutes =
        unplacedTasks.map(task => task.remainingMinutes).sum

      val totalUnplacedHours =
        totalUnplacedMinutes / 60.0

      report.append(
        "Всего не размещено работы: " +
          totalUnplacedHours +
          " часов\n"
      )
    }

    val overdueTasks = tasks.filter(task =>
      task.status == TaskStatus.OVERDUE
    )

    report.append("\nПРОСРОЧЕННЫЕ ДЕЛА\n")

    if (overdueTasks.isEmpty) {
      report.append("Просроченные дела отсутствуют.\n")
    } else {
      for (task <- overdueTasks) {
        report.append("ID: " + task.id + "\n")
        report.append("Название: " + task.name + "\n")
        report.append("Дедлайн: " + task.deadline + "\n")
        report.append("Осталось часов: " + task.remainingMinutes / 60.0 + "\n")
        report.append("\n")
      }
    }

    report.append("\nОСТАВШЕЕСЯ СВОБОДНОЕ ВРЕМЯ\n")

    if (remainingFreeTimeSlots.isEmpty) {
      report.append("Свободного времени не осталось.\n")
    } else {
      for (slot <- remainingFreeTimeSlots) {
        report.append("Дата: " + slot.date + "\n")
        report.append("Время: " + slot.startTime + " - " + slot.endTime + "\n")
        report.append("Продолжительность: " + slot.durationMinutes / 60.0 + " часов\n")
        report.append("\n")
      }

      val totalFreeMinutes =
        remainingFreeTimeSlots.map(slot => slot.durationMinutes).sum

      val totalFreeHours =
        totalFreeMinutes / 60.0

      report.append("Всего осталось свободного времени: " + totalFreeHours + " часов\n")
    }
    report.toString()
  }

  def saveReport(reportDirectory: String, content: String): Unit = {
    try {
      val directory = new File(reportDirectory)

      if (!directory.exists()) {
        directory.mkdirs()
      }

      val file = new File(directory, "report.txt")
      val writer = new PrintWriter(file)

      try {
        writer.print(content)
        println("Отчёт успешно создан: " + file.getPath)
      } finally {
        writer.close()
      }
    } catch {
      case exception: Exception =>
        println("Ошибка при создании отчёта: " + exception.getMessage)
    }
  }
}