package ofdm

object TreeReduce {
  def apply[V](in: Seq[V], func: (V, V) => V): V = {
    if (in.length == 1) {
      return in.head
    }
    if (in.length == 2) {
      return func(in.head, in(1))
    }
    if (in.length % 2 == 0) {
      val withIdxs = in.zipWithIndex
      val evens = withIdxs.filter{case (_, idx) => idx % 2 == 0}.map(_._1)
      val odds  = withIdxs.filter{case (_, idx) => idx % 2 != 0}.map(_._1)
      val evenOddPairs: Seq[(V, V)] = evens zip odds
      TreeReduce(evenOddPairs.map(x => func(x._1, x._2)), func)
    } else {
      TreeReduce(Seq(in.head, TreeReduce(in.drop(1), func)), func)
    }
  }
}
