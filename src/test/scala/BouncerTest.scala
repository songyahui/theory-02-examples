import Direction._
import org.scalatest.{FunSpec, Matchers}
import util.ThreadID

/**
  * @author Ilya Sergey
  */
class BouncerTest extends FunSpec with Matchers {
  val N = 20

  class MyThread(b: Bouncer) extends Thread {
    var result: Option[Value] = None

    override def run() = {
      result = Some(b.visit())
    }
  }


  describe(s"Bouncer class") {
    it("should return at most one STOP for any number of threads") {
      ThreadID.reset()
      val bouncer = new Bouncer
      
      // Fill the array with many threads visiting the bouncer
      val threads = Array.fill(N)(new MyThread(bouncer))

      // Start all threads
      for (i <- 0 until N) {
        threads(i).start()
      }

      // Wait for all threads to join
      for (i <- 0 until N) {
        threads(i).join()
      }
      
      // Get all results
      val results = threads.toList.map(_.result) 
      
      for (r <- results) println(r.get)

      // At most one STOP
      assert(results.count(_.get == STOP) <= 1)

      // At most (N - 1) RIGHT
      assert(results.count(_.get == RIGHT) <= N)

      // At most (N - 1) DOWN
      assert(results.count(_.get == DOWN) < N)


      
    }
  }


}
