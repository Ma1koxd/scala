import scala.io.StdIn.readLine

object Main {
  def main(args: Array[String]): Unit = {
    println("Введите название дела:")
    val name = readLine()
    println("Введите трудоёмкость в часах:")
    val number = readLine().toInt
    println("Сколько часов уже выполнено:")
    val hours = readLine().toInt

    if (hours > number){
      println("Ошибка: выполненное время не может превышать трудоёмкость.")
    }
    else if(hours < 0){
      println("Ошибка: выполненное время не может быть отрицательным")
    }
    else{
      println("Ваше дело: " + name)
      println("Трудоёмкость: " + number + " часов")
      println("Выполнено: " + hours + " часов")
      println("Осталось: " + (number - hours) + " часов")
    }
  }
}