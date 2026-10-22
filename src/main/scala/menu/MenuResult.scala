package menu

enum MenuResult {
  case Stay
  case Open(menu: MenuContainer)
  case Invalid
}