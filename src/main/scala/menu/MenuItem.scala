package menu

case class MenuItem(
                     name: String,
                     action: () => Unit
                   ) extends MenuNode