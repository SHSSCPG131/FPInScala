def insertionsort[A <% Ordered[A]](list: List[A]): List[A] = {
  def sort(as: List[A], bs: List[A]): List[A] = as match {
    case h :: t => sort(t, insert(h, bs))
    case Nil => bs
  }

  def insert(a: A, as: List[A]): List[A] = as match {
    case h :: t if (a > h) => h :: insert(a, t)
    case _ => a :: as
  }

  sort(list, Nil)
}
