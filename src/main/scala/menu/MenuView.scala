package menu

trait MenuView {
  def showMenu(menu: MenuContainer, canGoBack: Boolean): Unit
  def readChoice(): Option[Int]
  def showInvalidChoice(): Unit
  def showInvalidInput(): Unit
}