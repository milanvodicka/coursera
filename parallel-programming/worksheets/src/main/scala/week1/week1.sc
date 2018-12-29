object foo1 {

  class HelloThread extends Thread {
    override def run(): Unit = {
      println("Hello")
      println("World!")
    }
  }

  val t = new HelloThread

  t.start()
  t.join()
}

object foo2 {

  private var uidCount = 0

  def getUid = {
    uidCount = uidCount + 1
    uidCount
  }

  def startThread() = {
    val t = new Thread {
      override def run() = {
        val uids = for (i <- 0 until 10) yield getUid
        println(uids)
      }
    }
    t.start()
    t
  }

}
