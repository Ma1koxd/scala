package menu

case class MenuContainer(
                        name: String,
                        items: List[MenuNode]
                        ) extends MenuNode
