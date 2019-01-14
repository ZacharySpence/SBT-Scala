import java.util.concurrent.ConcurrentHashMap

//Simulate DB server
object DBServer{
  type Db = StringBuffer
  private val databases = new ConcurrentHashMap[String,Db]
  def createDB(name:String):Db={
    val db = new StringBuffer()
    databases.put(name,db)
    db
  }
  def removeDB(name:String): Unit ={
    databases.remove(name)
  }
}
