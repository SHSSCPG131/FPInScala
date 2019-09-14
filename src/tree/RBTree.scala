/**
 * A color for RB-Tree's nodes. 
 */
abstract sealed class Color

case object Red extends Color

case object Black extends Color

/**
 * A Red-Black Tree.
 */
abstract sealed class Tree[+A <% Ordered[A]] {

  /**
   * The color of this tree.
   */
  def color: Color

  /**
   * The value of this tree.
   */
  def value: A

  /**
   * The left child of this tree.
   */
  def left: Tree[A]

  /**
   * The right child of this tree.
   */
  def right: Tree[A]

  /**
   * Checks whether this tree is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Adds given element 'x' into this tree.
   *
   * Exercise 3.10a @ PFDS.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def add[B >: A <% Ordered[B]](x: B): Tree[B] = {
    def balancedAdd(t: Tree[A]): Tree[B] =
      if (t.isEmpty) Tree.make(Red, x)
      else if (x < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right)
      else if (x > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    def balanceLeft(c: Color, x: A, l: Tree[B], r: Tree[A]) = (c, l, r) match {
      case (Black, Branch(Red, y, Branch(Red, z, a, b), c), d) =>
        Tree.make(Red, y, Tree.make(Black, z, a, b), Tree.make(Black, x, c, d))
      case (Black, Branch(Red, z, a, Branch(Red, y, b, c)), d) =>
        Tree.make(Red, y, Tree.make(Black, z, a, b), Tree.make(Black, x, c, d))
      case _ => Tree.make(c, x, l, r)
    }

    def balanceRight(c: Color, x: A, l: Tree[A], r: Tree[B]) = (c, l, r) match {
      case (Black, a, Branch(Red, y, b, Branch(Red, z, c, d))) =>
        Tree.make(Red, y, Tree.make(Black, x, a, b), Tree.make(Black, z, c, d))
      case (Black, a, Branch(Red, z, Branch(Red, y, b, c), d)) =>
        Tree.make(Red, y, Tree.make(Black, x, a, b), Tree.make(Black, z, c, d))
      case _ => Tree.make(c, x, l, r)
    }

    def blacken(t: Tree[B]) = Tree.make(Black, t.value, t.left, t.right)

    blacken(balancedAdd(this))
  }

  def height: Int =
    if (isEmpty) 0
    else math.max(left.height, right.height) + 1

  /**
   * Fails with given message.
   */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case class Branch[A <% Ordered[A]](color: Color,
                                   value: A,
                                   left: Tree[A],
                                   right: Tree[A]) extends Tree[A] {
  def isEmpty = false
}

case object Leaf extends Tree[Nothing] {
  def color: Color = Black

  def value: Nothing = fail("An empty tree.")

  def left: Tree[Nothing] = fail("An empty tree.")

  def right: Tree[Nothing] = fail("An empty tree.")

  def isEmpty = true
}

object Tree {

  /**
   * Returns an empty red-black tree instance.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def empty[A]: Tree[A] = Leaf

  /**
   *
   */
  def make[A <% Ordered[A]](c: Color, x: A, l: Tree[A] = Leaf, r: Tree[A] = Leaf): Tree[A] =
    Branch(c, x, l, r)

  /**
   * Creates a new red-black tree from given 'xs' sequence.
   *
   * Time - O(n log n)
   * Space - O(log n)
   */
  def apply[A <% Ordered[A]](xs: A*): Tree[A] = {
    var r: Tree[A] = Leaf
    for (x <- xs) r = r.add(x)
    r
  }
}
