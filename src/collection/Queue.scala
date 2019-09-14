class Queue[+A](in: List[A] = Nil, out: List[A] = Nil) {
  def isEmpty: Boolean = in.isEmpty && out.isEmpty

  /** Dequeues the first element from this queue. */
  def dequeue: (A, Queue[A]) = out match {
    case hd :: tl => (hd, new Queue(in, tl))
    case Nil => in.reverse match {
      case hd :: tl => (hd, new Queue(Nil, tl))
      case Nil => throw new NoSuchElementException("Empty queue.")
    }
  }

  /** Enqueues given element 'x' into the end of this queue. */
  def enqueue[B >: A](x: B): Queue[B] = new Queue(x :: in, out)

  /** Returns the first element of this queue. */
  def front: A = dequeue match {
    case (a, _) => a
  }

  /** Returns the rear of this queue. */
  def rear: Queue[A] = dequeue match {
    case (_, q) => q
  }

  override def toString = (out ::: in.reverse).mkString("Queue(", ",", ")")
}

object Queue {
  /** Creates a new empty queue. */
  def empty[A]: Queue[A] = new Queue()

  /** Creates a new queue fromm given 'xs' sequence. */
  def apply[A](xs: A*) =
    xs.foldLeft(Queue.empty[A]) { case (acc, x) => acc.enqueue(x) }
}
