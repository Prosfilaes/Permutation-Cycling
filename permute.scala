import scala.collection.parallel.CollectionConverters._
import java.util.concurrent.atomic.AtomicBoolean

package Permute {
  object Permute {
    
    type Perm = Vector[Int]
    def permToString (p : Perm) : String = {
      p.map (_.toString).reduce (_ ++ _)
    }
    def perm (n : Int) : Option[List[Perm]] = {
      val start = Range(1, n + 1).toVector
      val length = Range (1, n + 1).product
      val abort = new AtomicBoolean (false)
      
      def k (l : List[Perm], s : Set[Perm]) : Option[List[Perm]] = {

        def swap (a : Perm, b : Int) =
          a.updated(b - 1, a(b)).updated(b, a(b-1))

        if (abort.get()) {
          None
        }
        else if (l.size == length) {
          abort.set(true)
          Some(l)
        }
        else if (l.size == 1) {
          val firstSwap = swap (l.head, 1)
          k (firstSwap +: l, s + firstSwap) 
        }
        else {
          val m = l.head
          val newl = Range(1, n).map (x => swap(m, x)).filter (x => ! s.contains(x))
          val l2 = (if (l.size < 4) 
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
      var i : Int = args(0).toInt
      val l = perm (i)
      val ls = l.map(_.map (permToString))
      println (i.toString + " : " + ls.toString)
    }
  }
}
