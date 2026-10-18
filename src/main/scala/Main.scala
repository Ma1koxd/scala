import io.AppLogger.log
import io.ConfigManager.loadConfig
import service.FreeTimeManager.{addFreeTimeSlot, deleteFreeTimeSlot}
import console.FreeTimePrinter.printFreeTimeSlots
import console.InputUtils.{readDate, readInt}
import io.ReportManager.{buildReport, saveReport}
import console.ScheduledBlockPrinter.printSchedule
import service.Scheduler.scheduleAllTasks
import service.TaskManager.{addTask, cancelTask, deleteTask, editTask}
import console.TaskPrinter.{printTask, printTasks}
import io.StateManager.{loadState, saveState}
import model.{AppState, FreeTimeSlot, ScheduledBlock, Task, TaskStatus}

object Main {
  def main(args: Array[String]): Unit = {
    var tasks = List.empty[Task]
    var freeTimeSlots = List.empty[FreeTimeSlot]
    var schedule = List.empty[ScheduledBlock]
    var remainingFreeTimeSlots = List.empty[FreeTimeSlot]
    var running = true
    var nextId = 1
    var config = loadConfig("config.json")

    log(config.logFile, "Программа запущена")
    log(config.logFile, "Конфигурация приложения применена")

    loadState(config.stateFile) match {
      case Some(state) =>
        tasks = state.tasks
        freeTimeSlots = state.freeTimeSlots
        nextId = state.nextId
        log(config.logFile, "Состояние программы загружено")
      case None =>
        log(config.logFile, "Состояние программы не загружено")
    }

    remainingFreeTimeSlots = freeTimeSlots

    while (running){
      println("Меню:")
      println("1. Добавить дело")
      println("2. Показать дела")
      println("3. Изменить дело")
      println("4. Удалить дело")
      println("5. Отменить дело")
      println("6. Добавить свободное время")
      println("7. Показать свободное время")
      println("8. Удалить свободное время")
      println("9. Составить расписание")
      println("10. Показать расписание")
      println("11. Перезагрузить конфигурацию")
      println("12. Создать отчёт")
      println("0. Выход")

      val choice = readInt()

      choice match{
        case 1 =>
          val newTaskId = nextId
          val (updatedTasks, updatedNextId) = addTask(tasks, nextId)

          tasks = updatedTasks
          nextId = updatedNextId
          log(config.logFile, "Добавлено дело ID: " + newTaskId)
          schedule = List.empty[ScheduledBlock]
          remainingFreeTimeSlots = freeTimeSlots
        case 2 =>
          printTasks(tasks)
        case 3 =>
          val oldTasks = tasks
          tasks = editTask(tasks)
          val completedHoursChanged = oldTasks.exists(oldTask =>
            tasks.find(task => task.id == oldTask.id) match {
              case Some(updatedTask) =>
                updatedTask.completedHours != oldTask.completedHours
              case None =>
                false
            }
          )

          if (completedHoursChanged) {
            log(config.logFile, "Изменено количество выполненных часов дела")
          }

          if (tasks != oldTasks) {
            log(config.logFile, "Дело изменено")
          }

          schedule = List.empty[ScheduledBlock]
          remainingFreeTimeSlots = freeTimeSlots
        case 4 =>
          val oldTasks = tasks
          tasks = deleteTask(tasks)

          if (tasks != oldTasks) {
            log(config.logFile, "Дело удалено")
          }

          schedule = List.empty[ScheduledBlock]
          remainingFreeTimeSlots = freeTimeSlots
        case 5 =>
          val oldTasks = tasks
          tasks = cancelTask(tasks)

          if (tasks != oldTasks) {
            log(config.logFile, "Дело отменено")
          }

          schedule = List.empty[ScheduledBlock]
          remainingFreeTimeSlots = freeTimeSlots
        case 6 =>
          val oldFreeTimeSlots = freeTimeSlots
          freeTimeSlots = addFreeTimeSlot(freeTimeSlots)

          if (freeTimeSlots != oldFreeTimeSlots) {
            log(config.logFile, "Добавлен интервал свободного времени")
          }

          schedule = List.empty[ScheduledBlock]
          remainingFreeTimeSlots = freeTimeSlots
        case 7 =>
          printFreeTimeSlots(remainingFreeTimeSlots)
        case 8 =>
          val oldFreeTimeSlots = freeTimeSlots
          freeTimeSlots = deleteFreeTimeSlot(freeTimeSlots)

          if (freeTimeSlots != oldFreeTimeSlots) {
            log(config.logFile, "Удалён интервал свободного времени")
          }

          schedule = List.empty[ScheduledBlock]
          remainingFreeTimeSlots = freeTimeSlots
        case 9 =>
          println("Введите текущую дату планирования (ГГГГ-ММ-ДД):")
          val currentDate = readDate()
          log(config.logFile, "Начато построение расписания на дату: " + currentDate)

          val (blocks, updatedFreeTimeSlots, unplacedTasks, updatedTasks) = scheduleAllTasks(tasks, freeTimeSlots, currentDate)
          schedule = blocks
          remainingFreeTimeSlots = updatedFreeTimeSlots
          log(config.logFile, "Построение расписания завершено. Создано блоков: " + schedule.length)
          printSchedule(schedule)

          if (unplacedTasks.nonEmpty) {
            val totalUnplacedMinutes = unplacedTasks.map(task => task.remainingMinutes).sum
            val totalUnplacedHours = totalUnplacedMinutes / 60.0
            log(config.logFile, "Не удалось разместить дел: " + unplacedTasks.length
              + ", не размещено часов: " + totalUnplacedHours)

            println("\nПлан построен не полностью.")
            println("Не удалось разместить задач: " + unplacedTasks.length)
            println("Не размещено работы: " + totalUnplacedHours + " часов")
            println("\nНе удалось разместить следующие задачи: \n")

            for (task <- unplacedTasks) {
              printTask(task)
            }
          }

          val overdueTasks = updatedTasks.filter(task =>
            task.status == TaskStatus.OVERDUE
          )

          if (overdueTasks.nonEmpty) {
            log(config.logFile, "Обнаружено просроченных дел: " + overdueTasks.length)
            println("\nПросроченные задачи:\n")

            for (task <- overdueTasks) {
              printTask(task)
            }
          }

          println("Оставшееся свободное время после составления расписания:")

          if (remainingFreeTimeSlots.isEmpty) {
            println("Свободного времени не осталось.\n")
          } else {
            printFreeTimeSlots(remainingFreeTimeSlots)
          }

          tasks = updatedTasks
        case 10 =>
          printSchedule(schedule)
        case 11 =>
          config = loadConfig("config.json", config)
          log(config.logFile, "Выполнена попытка перезагрузки конфигурации")
        case 12 =>
          val report = buildReport(
            tasks,
            schedule,
            remainingFreeTimeSlots
          )

          val reportSaved =
            saveReport(config.reportDirectory, report)

          if (reportSaved) {
            log(config.logFile, "Создан текстовый отчёт")
          } else {
            log(config.logFile, "Ошибка при создании текстового отчёта")
          }
        case 0 =>
          val state = AppState(
            tasks,
            freeTimeSlots,
            nextId
          )
          val stateSaved =
            saveState(state, config.stateFile)

          if (stateSaved) {
            log(config.logFile, "Состояние программы сохранено")
          } else {
            log(config.logFile, "Ошибка при сохранении состояния программы")
          }

          log(config.logFile, "Программа завершена")
          running = false
        case _ => println("Ошибка: выберите пункт от 0 до 12.")
      }
    }
  }
}