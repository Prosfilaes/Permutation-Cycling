package Permute {
  object Permute {
    type Perm = Vector[Int]
    def permToString (p : Perm) : String = {
      p.map (_.toString).reduce (_ ++ _)
    }
    def perm (n : Int) : Option[List[Perm]] = {
      val start = Range(1, n + 1).toVector
      val length = Range (1, n + 1).product
      
      def k (l : List[Perm], s : Set[Perm]) : Option[List[Perm]] = {

        def swap (a : Perm, b : Int) =
          a.updated(b - 1, a(b)).updated(b, a(b-1))

        if (l.size == length) {
          Some(l)
        }
        else {
          val m = l.head
          val newl = Range(1, n).map (x => swap(m, x)).filter (x => ! s.contains(x))
          val newl2 = if (l.size < 4) newl.parellel else newl.view
          val l2 =  newl.map (x => k(x +: l, s + x)).filter(! _.isEmpty).map(_.get).headOption
          if (l2.isEmpty) println ("*"+ l.size)
          else println ("!" + l2.map(_.map (permToString)).toString)
          l2
        }
      }

      k (List(start), Set(start)) 
    }
    def main (args: Array[String]) : Unit = {
      var i : Int = args(0).toInt
      // while (true) {
        val l = perm (i)
        val ls = l.map(_.map (permToString))
        println (i.toString + " : " + ls.toString)
      //  i = i + 1
      //}
    }
  }
}
