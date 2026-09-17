import scala.io.StdIn.readLine

case class Task(id: Int, name: String, totalHours: Int, completedHours: Int)

object Main {
  def main(args: Array[String]): Unit = {
    var tasks = List.empty[Task]

    println("Введите количество дел:")
    val taskCount = readLine().toInt
    if (taskCount > 0){
      var i = 0
      while (i < taskCount) {
        val id = i+1
        println("Введите название дела:")

        val name = readLine()
        println("Введите трудоёмкость в часах:")
        val totalHours = readLine().toInt

        println("Сколько часов уже выполнено:")
        val completedHours = readLine().toInt

        val task = Task(id, name, totalHours, completedHours)

        if (task.totalHours <= 0) {
          println("Ошибка: трудоёмкость должна быть больше нуля.")
        }
        else if (task.completedHours > task.totalHours) {
          println("Ошибка: выполненное время не может превышать трудоёмкость.")
        }
        else if (task.completedHours == task.totalHours) {
          println("Ошибка: дело уже выполнено.")
        }
        else if (task.completedHours < 0) {
          println("Ошибка: выполненное время не может быть отрицательным")
        }
        else {
          println("Ваше дело: " + task.name)
          println("Трудоёмкость: " + task.totalHours + " часов")
          println("Выполнено: " + task.completedHours + " часов")
          println("Осталось: " + (task.totalHours - task.completedHours) + " часов\n")
          tasks = tasks :+ task
          i += 1
        }
      }
    }
    else{
      println("Ошибка: количество дел должно быть больше нуля")
    }
    println("Количество сохранённых дел: " + tasks.length)
    for (task <- tasks){
      println(task.name)
    }
  }
}