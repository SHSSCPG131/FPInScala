abstract sealed class Heap[+A <% Ordered[A]] {

  /**
   * Minimum of this heap.
   */
  def min: A

  /**
   * The left child of this heap.
   */
  def left: Heap[A]

  /**
   * The right child of this heap.
   */
  def right: Heap[A]

  /**
   * The size of this heap.
   */
  def size: Int

  /**
   * The height of this tree
   */
  def height: Int

  /**
   * Checks whether this heap empty or not.
   */
  def isEmpty: Boolean

  /**
   * Inserts given element 'x' into this heap.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def insert[B >: A <% Ordered[B]](x: B): Heap[B] =
    if (isEmpty) Heap.make(x)
    else if (left.size < math.pow(2, left.height) - 1)
      Heap.bubbleUp(min, left.insert(x), right)
    else if (right.size < math.pow(2, right.height) - 1)
      Heap.bubbleUp(min, left, right.insert(x))
    else if (right.height < left.height)
      Heap.bubbleUp(min, left, right.insert(x))
    else Heap.bubbleUp(min, left.insert(x), right)

  /**
   * Removes minimum element from this heap.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def remove: Heap[A] = {
    def floatLeft[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = l match {
      case Branch(y, lt, rt, _, _) => Heap.make(y, Heap.make(x, lt, rt), r)
      case _ => Heap.make(x, l, r)
    }

    def floatRight[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = r match {
      case Branch(y, lt, rt, _, _) => Heap.make(y, l, Heap.make(x, lt, rt))
      case _ => Heap.make(x, l, r)
    }

    def mergeChildren(l: Heap[A], r: Heap[A]): Heap[A] =
      if (l.isEmpty && r.isEmpty) Heap.empty
      else if (l.size < math.pow(2, l.height) - 1)
        floatLeft(l.min, mergeChildren(l.left, l.right), r)
      else if (r.size < math.pow(2, r.height) - 1)
        floatRight(r.min, l, mergeChildren(r.left, r.right))
      else if (r.height < l.height)
        floatLeft(l.min, mergeChildren(l.left, l.right), r)
      else floatRight(r.min, l, mergeChildren(r.left, r.right))

    def bubbleRootDown(h: Heap[A]): Heap[A] =
      if (h.isEmpty) Heap.empty
      else Heap.bubbleDown(h.min, h.left, h.right)

    if (isEmpty) fail("An empty heap.")
    else bubbleRootDown(mergeChildren(left, right))
  }

  /**
   * Fails with given message.
   */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case class Branch[A <% Ordered[A]](min: A, left: Heap[A], right: Heap[A], size: Int, height: Int) extends Heap[A] {
  def isEmpty: Boolean = false
}

case object Leaf extends Heap[Nothing] {
  def min: Nothing = fail("An empty heap.")

  def left: Heap[Nothing] = fail("An empty heap.")

  def right: Heap[Nothing] = fail("An empty heap.")

  def size: Int = 0

  def height: Int = 0

  def isEmpty: Boolean = true
}

object Heap {

  /**
   * Returns an empty heap.
   */
  def empty[A]: Heap[A] = Leaf

  /**
   * A smart constructor for heap's branch.
   */
  def make[A <% Ordered[A]](x: A, l: Heap[A] = Leaf, r: Heap[A] = Leaf): Heap[A] =
    Branch(x, l, r, l.size + r.size + 1, math.max(l.height, r.height) + 1)

  /**
   * Creates a new heap from given sorted array 'a'.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fromSortedArray[A <% Ordered[A]](a: Array[A]): Heap[A] = {
    def loop(i: Int): Heap[A] =
      if (i < a.length) Heap.make(a(i), loop(2 * i + 1), loop(2 * i + 2))
      else Heap.empty

    loop(0)
  }

  /**
   * Creates a new heap from given array 'a'.
   *
   * Time - O(n)
   * Space - O(log n)
   */
  def fromArray[A <% Ordered[A]](a: Array[A]): Heap[A] = {
    def loop(i: Int): Heap[A] =
      if (i < a.length) Heap.bubbleDown(a(i), loop(2 * i + 1), loop(2 * i + 2))
      else Heap.empty

    loop(0)
  }


  /**
   * Bubbles given heap ('x', 'l', 'r') up.
   *
   * Time - O(1)
   * Space - O(1)
   */
  private[Heap] def bubbleUp[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = (l, r) match {
    case (Branch(y, lt, rt, _, _), _) if (x > y) =>
      Heap.make(y, Heap.make(x, lt, rt), r)
    case (_, Branch(z, lt, rt, _, _)) if (x > z) =>
      Heap.make(z, l, Heap.make(x, lt, rt))
    case (_, _) => Heap.make(x, l, r)
  }

  /**
   * Bubbles given heap ('x', 'l', 'r') down.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  private[Heap] def bubbleDown[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = (l, r) match {
    case (Branch(y, _, _, _, _), Branch(z, lt, rt, _, _)) if (z < y && x > z) =>
      Heap.make(z, l, Heap.bubbleDown(x, lt, rt))
    case (Branch(y, lt, rt, _, _), _) if (x > y) =>
      Heap.make(y, Heap.bubbleDown(x, lt, rt), r)
    case (_, _) => Heap.make(x, l, r)
  }
}
