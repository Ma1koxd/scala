package menu

class ConsoleMenuView extends MenuView {
  override def showMenu(menu: MenuContainer, canGoBack: Boolean): Unit = {
    println("\n" + menu.name)

    for ((item, index) <- menu.items.zipWithIndex) {
      println((index + 1) + ". " + item.name)
    }

    if (canGoBack) {
      println("0. Назад")
    } else {
      println("0. Выход")
    }
  }

  override def readChoice(): Option[Int] = {
    val input = scala.io.StdIn.readLine()
    input.toIntOption
  }

  override def showInvalidChoice(): Unit = {
    println("Ошибка: такого пункта меню нет.")
  }

  override def showInvalidInput(): Unit = {
    println("Ошибка: введите число.")
  }
}