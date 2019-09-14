def linearsearch[A](list: List[A], key: A): Option[A] = {
  def search(as: List[A]): Option[A] =
    if (as.isEmpty) None
    else if (as.head == key) Some(as.head)
    else search(as.tail)

  search(list)
}
