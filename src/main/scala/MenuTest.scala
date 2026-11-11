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
        ),
        MenuItem(
          name = "Изменить дело",
          action = () => println("Выбрано изменение дела")
        ),
        MenuItem(
          name = "Удалить дело",
          action = () => println("Выбрано удаление дела")
        ),
        MenuItem(
          name = "Отменить дело",
          action = () => println("Выбрана отмена дела")
        )
      )
    )

    val freeTimeMenu = MenuContainer(
      name = "Свободное время",
      items = List(
        MenuItem(
          name = "Добавить свободное время",
          action = () => println("Выбрано добавление свободного времени")
        ),
        MenuItem(
          name = "Показать свободное время",
          action = () => println("Выбран показ свободного времени")
        ),
        MenuItem(
          name = "Удалить свободное время",
          action = () => println("Выбрано удаление свободного времени")
        )
      )
    )

    val scheduleMenu = MenuContainer(
      name = "Расписание",
      items = List(
        MenuItem(
          name = "Построить расписание",
          action = () => println("Выбрано построение расписания")
        ),
        MenuItem(
          name = "Показать расписание",
          action = () => println("Выбран показ расписания")
        )
      )
    )

    val mainMenu = MenuContainer(
      name = "Главное меню",
      items = List(
        tasksMenu,
        freeTimeMenu,
        scheduleMenu,
        MenuItem(
          name = "Перезагрузить конфигурацию",
          action = () => println("Выбрана перезагрузка конфигурации")
        ),
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