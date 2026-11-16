import console.FreeTimeErrorPrinter.printFreeTimeError
import console.FreeTimeInput.readFreeTimeSlot
import io.AppLogger.log
import io.ConfigManager.{defaultConfig, loadConfigResult}
import service.FreeTimeManager.{addFreeTimeSlotResult, deleteFreeTimeSlotResult}
import console.FreeTimePrinter.printFreeTimeSlots
import console.InputUtils.{readDate, readInt, readTime}
import io.ReportManager.{buildReport, saveReport}
import console.ScheduledBlockPrinter.printSchedule
import console.StorageErrorPrinter.printStorageError
import console.TaskEditor.editTask
import console.TaskErrorPrinter.printTaskError
import console.TaskInput.readTask
import service.Scheduler.scheduleAllTasks
import service.TaskManager.{addTaskResult, cancelTaskResult, deleteTaskResult}
import console.TaskPrinter.{printTask, printTasks}
import io.StateManager.{loadStateResult, saveStateResult}
import menu.{ConsoleMenuView, MenuContainer, MenuController, MenuItem}
import model.{AppAction, AppState, FreeTimeSlot, ScheduledBlock, Task, TaskStatus}

object Main {
  def main(args: Array[String]): Unit = {
    var tasks = List.empty[Task]
    var freeTimeSlots = List.empty[FreeTimeSlot]
    var schedule = List.empty[ScheduledBlock]
    var remainingFreeTimeSlots = List.empty[FreeTimeSlot]
    var nextId = 1
    var config = defaultConfig

    loadConfigResult("config.json") match {
      case Right(Some(loadedConfig)) =>
        config = loadedConfig
        println("Конфигурация успешно загружена.")
      case Right(None) =>
        println("Файл конфигурации не найден. Сохраняются текущие настройки.")
      case Left(error) =>
        printStorageError(error)
        println("Используются настройки по умолчанию.")
    }

    log(config.logFile, "Программа запущена")
    log(config.logFile, "Конфигурация приложения применена")

    loadStateResult(config.stateFile) match {
      case Right(Some(state)) =>
        tasks = state.tasks
        freeTimeSlots = state.freeTimeSlots
        nextId = state.nextId
        println("Состояние успешно загружено.")
        log(config.logFile, "Состояние программы загружено")
      case Right(None) =>
        println("Файл состояния не найден.")
        log(config.logFile, "Состояние программы не загружено")
      case Left(error) =>
        printStorageError(error)
        log(config.logFile, "Состояние программы не загружено")
    }

    remainingFreeTimeSlots = freeTimeSlots

    val view = new ConsoleMenuView()
    val controller = new MenuController(view)

    // раздел с действиями для задач

    def addTaskAction(): Unit = {
      println("Добавление дела")
      val task = readTask(nextId)

      addTaskResult(tasks, task) match {
        case Right(updatedTasks) =>
          tasks = updatedTasks
          nextId = nextId + 1
          println("Дело успешно добавлено.")
          log(config.logFile, "Добавлено дело ID: " + task.id)
        case Left(error) =>
          printTaskError(error)
      }

      schedule = List.empty[ScheduledBlock]
      remainingFreeTimeSlots = freeTimeSlots
    }

    def showTasksAction(): Unit = {
      printTasks(tasks)
    }

    def editTaskAction(): Unit = {
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
    }

    def deleteTaskAction(): Unit = {
      if (tasks.isEmpty) {
        println("Список дел пуст.\n")
      } else {
        println("Введите ID дела для удаления: ")
        val id = readInt()

        deleteTaskResult(tasks, id) match {
          case Right(updatedTasks) =>
            tasks = updatedTasks
            println("Дело успешно удалено.")
            log(config.logFile, "Дело удалено")
          case Left(error) =>
            printTaskError(error)
        }
      }

      schedule = List.empty[ScheduledBlock]
      remainingFreeTimeSlots = freeTimeSlots
    }

    def cancelTaskAction(): Unit = {
      if (tasks.isEmpty) {
        println("Список дел пуст.")
      } else {
        println("Введите ID дела для отмены:")
        val id = readInt()

        cancelTaskResult(tasks, id) match {
          case Right(updatedTasks) =>
            tasks = updatedTasks
            println("Дело отменено.")
            log(config.logFile, "Дело отменено")
          case Left(error) =>
            printTaskError(error)
        }
      }

      schedule = List.empty[ScheduledBlock]
      remainingFreeTimeSlots = freeTimeSlots
    }

    // раздел с действиями для свободного времени

    def addFreeTimeAction(): Unit = {
      val slot = readFreeTimeSlot()

      addFreeTimeSlotResult(freeTimeSlots, slot) match {
        case Right(updatedFreeTimeSlots) =>
          freeTimeSlots = updatedFreeTimeSlots
          println("Свободное время добавлено успешно.")
          log(config.logFile, "Добавлен интервал свободного времени")
        case Left(error) =>
          printFreeTimeError(error)
      }

      schedule = List.empty[ScheduledBlock]
      remainingFreeTimeSlots = freeTimeSlots
    }

    def showFreeTimeAction(): Unit = {
      printFreeTimeSlots(remainingFreeTimeSlots)
    }

    def deleteFreeTimeAction(): Unit = {
      if (freeTimeSlots.isEmpty) {
        println("Свободное время не задано.")
      } else {
        println("Введите дату свободного времени для удаления (ГГГГ-ММ-ДД): ")
        val date = readDate()

        println("Введите время начала удаляемого промежутка (ЧЧ:ММ): ")
        val startTime = readTime()

        deleteFreeTimeSlotResult(freeTimeSlots, date, startTime) match {
          case Right(updatedFreeTimeSlots) =>
            freeTimeSlots = updatedFreeTimeSlots
            println("Свободное время успешно удалено.")
            log(config.logFile, "Удалён интервал свободного времени")
          case Left(error) =>
            printFreeTimeError(error)
        }
      }

      schedule = List.empty[ScheduledBlock]
      remainingFreeTimeSlots = freeTimeSlots
    }

    // раздел действий с расписанием

    def buildScheduleAction(): Unit = {
      println("Введите текущую дату планирования (ГГГГ-ММ-ДД):")
      val currentDate = readDate()
      log(config.logFile, "Начато построение расписания на дату: " + currentDate)

      val result = scheduleAllTasks(tasks, freeTimeSlots, currentDate)
      schedule = result.blocks
      remainingFreeTimeSlots = result.remainingFreeTimeSlots
      log(config.logFile, "Построение расписания завершено. Создано блоков: " + schedule.length)
      printSchedule(schedule)

      if (result.unplacedTasks.nonEmpty) {
        val totalUnplacedMinutes = result.unplacedTasks.map(task => task.remainingMinutes).sum
        val totalUnplacedHours = totalUnplacedMinutes / 60.0
        log(config.logFile, "Не удалось разместить дел: " + result.unplacedTasks.length
          + ", не размещено часов: " + totalUnplacedHours)

        println("\nПлан построен не полностью.")
        println("Не удалось разместить задач: " + result.unplacedTasks.length)
        println("Не размещено работы: " + totalUnplacedHours + " часов")
        println("\nНе удалось разместить следующие задачи: \n")

        for (task <- result.unplacedTasks) {
          printTask(task)
        }
      }

      val overdueTasks = result.updatedTasks.filter(task =>
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

      tasks = result.updatedTasks
    }

    def showScheduleAction(): Unit = {
      printSchedule(schedule)
    }

    // работа с конфигурацией

    def reloadConfigAction(): Unit = {
      loadConfigResult("config.json") match {
        case Right(Some(loadedConfig)) =>
          config = loadedConfig
          println("Конфигурация успешно загружена.")
        case Right(None) =>
          println("Файл конфигурации не найден. Применяются настройки по умолчанию.")
        case Left(error) =>
          printStorageError(error)
          println("Сохраняются текущие настройки.")
      }

      log(config.logFile, "Выполнена попытка перезагрузки конфигурации")
    }

    // работа с отчетом

    def createReportAction(): Unit = {
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
    }

    // выход из проги

    def exitAction(): Unit = {
      val state = AppState(
        tasks,
        freeTimeSlots,
        nextId
      )

      saveStateResult(state, config.stateFile) match {
        case Right(()) =>
          println("Состояние сохранено успешно.")
          log(config.logFile, "Состояние программы сохранено")
        case Left(error) =>
          printStorageError(error)
          log(config.logFile, "Ошибка при сохранении состояния программы")
      }

      log(config.logFile, "Программа завершена")
    }

    // пункты меню передают действия, представленные через ADT, в handleAppAction,
    // который вызывает нужный нам сценарий
    def handleAppAction(action: AppAction): Unit = {
      action match {
        case AppAction.ADD_TASK =>
          addTaskAction()
        case AppAction.SHOW_TASKS =>
          showTasksAction()
        case AppAction.EDIT_TASK =>
          editTaskAction()
        case AppAction.DELETE_TASK =>
          deleteTaskAction()
        case AppAction.CANCEL_TASK =>
          cancelTaskAction()
        case AppAction.ADD_FREE_TIME =>
          addFreeTimeAction()
        case AppAction.SHOW_FREE_TIME =>
          showFreeTimeAction()
        case AppAction.DELETE_FREE_TIME =>
          deleteFreeTimeAction()
        case AppAction.BUILD_SCHEDULE =>
          buildScheduleAction()
        case AppAction.SHOW_SCHEDULE =>
          showScheduleAction()
        case AppAction.RELOAD_CONFIG =>
          reloadConfigAction()
        case AppAction.CREATE_REPORT =>
          createReportAction()
      }
    }

    // переписываем менюшки, чтобы действия были явно описанными через ADT
    val tasksMenu = MenuContainer(
      "Работа с делами", List(
        MenuItem("Добавить дело", () => handleAppAction(AppAction.ADD_TASK)),
        MenuItem("Показать дела", () => handleAppAction(AppAction.SHOW_TASKS)),
        MenuItem("Изменить дело", () => handleAppAction(AppAction.EDIT_TASK)),
        MenuItem("Удалить дело", () => handleAppAction(AppAction.DELETE_TASK)),
        MenuItem("Отменить дело", () => handleAppAction(AppAction.CANCEL_TASK))
      )
    )

    val freeTimeMenu = MenuContainer(
      "Свободное время", List(
        MenuItem("Добавить свободное время", () => handleAppAction(AppAction.ADD_FREE_TIME)),
        MenuItem("Показать свободное время", () => handleAppAction(AppAction.SHOW_FREE_TIME)),
        MenuItem("Удалить свободное время", () => handleAppAction(AppAction.DELETE_FREE_TIME))
      )
    )

    val scheduleMenu = MenuContainer(
      "Расписание", List(
        MenuItem("Составить расписание", () => handleAppAction(AppAction.BUILD_SCHEDULE)),
        MenuItem("Показать расписание", () => handleAppAction(AppAction.SHOW_SCHEDULE))
      )
    )

    val mainMenu = MenuContainer(
      "Главное меню", List(
        tasksMenu,
        freeTimeMenu,
        scheduleMenu,
        MenuItem("Перезагрузить конфигурацию", () => handleAppAction(AppAction.RELOAD_CONFIG)),
        MenuItem("Создать отчёт", () => handleAppAction(AppAction.CREATE_REPORT))
      )
    )

    controller.run(mainMenu)
    exitAction()
  }
}