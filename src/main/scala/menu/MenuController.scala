package menu

class MenuController(view: MenuView) {
  def getSelectedItem(menu: MenuContainer, choice: Int): Option[MenuNode] = {
    if (choice >= 1 && choice <= menu.items.length) {
      Some(menu.items(choice - 1))
    } else {
      None
    }
  }

  def handleNode(node: MenuNode): MenuResult = {
    node match {
      case item: MenuItem =>
        item.action()
        MenuResult.Stay
      case container: MenuContainer =>
        MenuResult.Open(container)
    }
  }

  def processChoice(menu: MenuContainer, choice: Int): MenuResult = {
    getSelectedItem(menu, choice) match {
      case Some(node) =>
        handleNode(node)
      case None =>
        MenuResult.Invalid
    }
  }

  def run(startMenu: MenuContainer): Unit = {
    var currentMenu = startMenu
    var previousMenus = List.empty[MenuContainer]
    var running = true

    while (running) {
      view.showMenu(
        currentMenu,
        canGoBack = previousMenus.nonEmpty
      )

      view.readChoice() match {
        case Some(0) =>
          previousMenus match {
            case previousMenu :: remainingMenus =>
              currentMenu = previousMenu
              previousMenus = remainingMenus

            case Nil =>
              running = false
          }
        case Some(choice) =>
          processChoice(currentMenu, choice) match {
            case MenuResult.Stay =>
            case MenuResult.Open(menu) =>
              previousMenus = currentMenu :: previousMenus
              currentMenu = menu
            case MenuResult.Invalid =>
              view.showInvalidChoice()
          }
        case None =>
          view.showInvalidInput()
      }
    }
  }
}