package io

import model.{FreeTimeSlot, ScheduledBlock, StorageError, Task, TaskStatus}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

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
        // find возвращает Some(task), если дело найдено, или None
        // map преобразует Some(task) в Some(task.name), а None остаётся None
        // getOrElse возвращает полученное название,
        // а если поиск вернул None -> текст "Неизвестное дело"
        val taskName = tasks.find(task => task.id == block.taskId)
          .map(task => task.name)
          .getOrElse("Неизвестное дело")

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

  // создаем папку для отчётов, если она ещё не существует
  // возвращаем путь к папке либо ошибку её создания
  def createReportDirectoryResult(reportDirectory: String): Either[StorageError, Path] = {
    try {
      Right(Files.createDirectories(Path.of(reportDirectory)))
    } catch {
      case NonFatal(exception) =>
        Left(StorageError.DIRECTORY_CREATION_FAILED(reportDirectory, exception.toString))
    }
  }

  // записываем содержимое отчёта и возвращаем путь к файлу после его закрытия
  // ошибки записи и закрытия возвращаем через FILE_WRITE_FAILED
  def writeReportResult(file: Path, content: String): Either[StorageError, Path] = {
    try {
      val writer = Files.newBufferedWriter(file, StandardCharsets.UTF_8)

      try {
        writer.write(content)
      } finally {
        writer.close()
      }

      Right(file)
    } catch {
      case NonFatal(exception) =>
        Left(StorageError.FILE_WRITE_FAILED(file.toString, exception.toString))
    }
  }

  // создаем папку, затем записываем report.txt
  // если создание папки завершилось ошибкой, запись не выполняется
  def saveReportResult(reportDirectory: String, content: String): Either[StorageError, Path] = {
    createReportDirectoryResult(reportDirectory)
      .flatMap(directory => writeReportResult(directory.resolve("report.txt"), content))
  }
}