
def mergesort[A <% Ordered[A]](list: List[A]): List[A] = {
  def sort(p: (List[A], List[A])): List[A] = p match {
    case (Nil, Nil) => Nil
    case (ha :: Nil, Nil) => ha :: Nil
    case (Nil, hb :: Nil) => hb :: Nil
    case (as, bs) => merge(halfifyAndSort(as), halfifyAndSort(bs))
  }

  def halfify(as: List[A]): (List[A], List[A]) = {
    def loop(bs: List[A], fs: List[A], ss: List[A]): (List[A], List[A]) = bs match {
      case f :: s :: r => loop(r, f :: fs, s :: ss)
      case f :: Nil => (f :: fs, ss)
      case Nil => (fs, ss)
    }

    loop(as, Nil, Nil)
  }

  def merge(as: List[A], bs: List[A]): List[A] = {
    def loop(cs: List[A], ds: List[A], r: List[A]): List[A] = (cs, ds) match {
      case (ha :: ta, hb :: tb) =>
        if (ha < hb) loop(ta, ds, ha :: r)
        else loop(cs, tb, hb :: r)
      case (ha :: ta, Nil) => loop(ta, Nil, ha :: r)
      case (Nil, hb :: tb) => loop(Nil, tb, hb :: r)
      case (Nil, Nil) => r
    }

    loop(as, bs, Nil).reverse
  }

  def halfifyAndSort(as: List[A]) = sort(halfify(as))

  halfifyAndSort(list)
}