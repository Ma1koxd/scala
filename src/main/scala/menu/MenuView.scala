package menu

import model.InputError

trait MenuView {
  def showMenu(menu: MenuContainer, canGoBack: Boolean): Unit
  def readChoice(): Either[InputError, Option[Int]]
  def showInvalidChoice(): Unit
  def showInvalidInput(): Unit
}