import menu.{ConsoleMenuView, MenuContainer, MenuController, MenuItem}

object MenuTest {
  def main(args: Array[String]): Unit = {
    val tasksMenu = MenuContainer(
      name = "Работа с делами",
      items = List(
        MenuItem(
          name = "Добавить дело",
          action = () => println("Выбрано добавление дела")
        ),
        MenuItem(
          name = "Показать дела",
          action = () => println("Выбран показ дел")
        )
      )
    )

    val mainMenu = MenuContainer(
      name = "Главное меню",
      items = List(
        tasksMenu,
        MenuItem(
          name = "Создать отчёт",
          action = () => println("Выбрано создание отчёта")
        )
      )
    )

    val view = new ConsoleMenuView()
    val controller = new MenuController(view)

    controller.run(mainMenu)
  }
}