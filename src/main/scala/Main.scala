import FreeTimeManager.{addFreeTimeSlot, deleteFreeTimeSlot}
import FreeTimePrinter.printFreeTimeSlots
import InputUtils.{readDate, readInt}
import ScheduledBlockPrinter.printSchedule
import Scheduler.scheduleAllTasks
import TaskManager.{addTask, deleteTask, editTask}
import TaskPrinter.printTasks

object Main {
  def main(args: Array[String]): Unit = {
    var tasks = List.empty[Task]
    var freeTimeSlots = List.empty[FreeTimeSlot]
    var schedule = List.empty[ScheduledBlock]
    var running = true
    var nextId = 1

    while (running){
      println("Меню:")
      println("1. Добавить дело")
      println("2. Показать дела")
      println("3. Изменить дело")
      println("4. Удалить дело")
      println("5. Добавить свободное время")
      println("6. Показать свободное время")
      println("7. Удалить свободное время")
      println("8. Составить расписание")
      println("9. Показать расписание")
      println("0. Выход")

      val choice = readInt()

      choice match{
        case 1 =>
          val (updatedTasks, updatedNextId) = addTask(tasks, nextId)

          tasks = updatedTasks
          nextId = updatedNextId
          schedule = List.empty[ScheduledBlock]
        case 2 =>
          printTasks(tasks)
        case 3 =>
          tasks = editTask(tasks)
          schedule = List.empty[ScheduledBlock]
        case 4 =>
          tasks = deleteTask(tasks)
          schedule = List.empty[ScheduledBlock]
        case 5 =>
          freeTimeSlots = addFreeTimeSlot(freeTimeSlots)
          schedule = List.empty[ScheduledBlock]
        case 6 =>
          printFreeTimeSlots(freeTimeSlots)
        case 7 =>
          freeTimeSlots = deleteFreeTimeSlot(freeTimeSlots)
          schedule = List.empty[ScheduledBlock]
        case 8 =>
          println("Введите текущую дату планирования (ГГГГ-ММ-ДД):")
          val currentDate = readDate()

          val (blocks, updatedFreeTimeSlots, unplacedTasks, updatedTasks) = scheduleAllTasks(tasks, freeTimeSlots, currentDate)
          schedule = blocks
          printSchedule(schedule)

          tasks = updatedTasks
        case 9 =>
          printSchedule(schedule)
        case 0 => running = false
        case _ => println("Ошибка: выберите пункт от 0 до 9.")
      }
    }
  }
}