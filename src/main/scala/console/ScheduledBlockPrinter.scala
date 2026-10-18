package console

import model.ScheduledBlock

object ScheduledBlockPrinter {
  def printScheduledBlock(block: ScheduledBlock): Unit = {
    println("ID задачи: " + block.taskId)
    println("Дата: " + block.date)
    println("Начало: " + block.startTime)
    println("Окончание: " + block.endTime)
    println("Продолжительность: " + block.durationMinutes + " минут\n")
  }

  def printSchedule(blocks: List[ScheduledBlock]): Unit = {
    if (blocks.isEmpty) {
      println("Расписание пусто.\n")
    } else {
      println("\nРасписание: \n")
      for (block <- blocks) {
        printScheduledBlock(block)
      }
    }
  }
}
