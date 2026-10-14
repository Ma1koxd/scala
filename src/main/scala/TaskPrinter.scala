object TaskPrinter {
  def printTask(task: Task): Unit = {
    println("ID: " + task.id)
    println("Название: " + task.name)
    println("Трудоёмкость: " + task.totalHours + " часов")
    println("Выполнено: " + task.completedHours + " часов")
    println("Осталось: " + (task.totalHours - task.completedHours) + " часов")
    println("Дата начала: " + task.startDate)
    println("Срок до: " + task.deadline)
    println("Приоритет выполнения: " + task.priority)
    println("Можно делить: " + task.canSplit)
    if (task.canSplit) {
      println("Минимальный блок: " + task.minBlockMinutes + " минут")
    }
    println("Состояние: " + task.status + "\n")
  }

  def printTasks(tasks: List[Task]): Unit = {
    if (tasks.isEmpty) {
      println("Список дел пуст.\n")
    } else {
      println("\nСписок сохранённых дел: \n")
      for (task <- tasks) {
        printTask(task)
      }
    }
  }

}
