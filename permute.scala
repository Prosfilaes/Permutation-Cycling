import scala.collection.parallel.CollectionConverters._

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
          println ("!" + l.map(permToString).toString)
          Some(l)
        }
        else {
          val m = l.head
          val newl = Range(1, n).map (x => swap(m, x)).filter (x => ! s.contains(x))
          val l2 = (if (l.size < 5) 
              newl.toVector.par.map (x => k(x +: l, s + x)).filter(! _.isEmpty).map(_.get).headOption
            else {
              var finall : Option[List[Perm]] = None
              for (i <- newl) {
                if (finall == None) {
                  finall = k(i +: l, s + i)
                }
              }
              finall
            }
            )
          l2
        }
      }

      k (List(start), Set(start)) 
    }
    def main (args: Array[String]) : Unit = {
      var i : Int = 1; // args(0).toInt
      while (true) {
        val l = perm (i)
        val ls = l.map(_.map (permToString))
        println (i.toString + " : " + ls.toString)
        i = i + 1
      }
    }
  }
}
