import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import org.scalatest._
import org.scalatest.tagobjects.Slow
import java.io._
import java.util.concurrent.ConcurrentHashMap
import java.util.UUID.randomUUID

//composing fixtures by stacking traits
//trait Builder extends SuiteMixin{ this: Suite =>
//  val builder = new StringBuilder
//  abstract override def withFixture(test:NoArgTest) ={
//    builder.append("ScalaTest is ")
//    try super.withFixture(test) //to be stackable must call super.withFixture
//    finally builder.clear()
//  }
//}
//trait Buffer extends SuiteMixin {this: Suite =>
//  val buffer = new ListBuffer[String]
//  abstract override def withFixture(test:NoArgTest)={
//    try super.withFixture(test)
//    finally buffer.clear()
//  }
//}
//
//class ExampleWithTraits extends FlatSpec with Builder with Buffer{
//  "Testing" should "be easy" in {
//    builder.append("easy!")
//    assert(builder.toString === "ScalaTest is easy!")
//    assert(buffer.isEmpty)
//    buffer += "sweet"
//  }
//  it should "be fun" in {
//    builder.append("fun!")
//    assert(builder.toString === "ScalaTest is fun!")
//    assert(buffer.isEmpty)
//    buffer += "clear"
//  }
//}
//abstract class UnitSpec extends FlatSpec with Matchers
//  with OptionValues with Inside with Inspectors{}
//
//
////overriding before and after functions
//trait BuilderBAA extends BeforeAndAfter{ this:Suite =>
//  val builder = new StringBuilder
//  override def beforeEach(): Unit ={
//    builder.append("ScalaTest is ")
//    super.beforeEach()
//  }
//  override def afterEach(): Unit ={
//    try super.afterEach()
//    finally builder.clear()
//  }
//}
//
//trait BufferBAA extends BeforeAndAfter{ this:Suite =>
//  val buffer = new ListBuffer[String]
//  override def afterEach(): Unit ={
//    try super.afterEach()
//    finally buffer.clear()
//  }
//}
//
//class ExampleSpecBAA extends FlatSpec with BuilderBAA with BufferBAA{
//  "testing" should "be easy" in{
//    builder.append("easy!")
//    assert(builder.toString === "ScalaTest is easy!")
//    assert(buffer.isEmpty)
//    buffer += "sweet"
//  }
//}


//SHARING TESTS
class Stack[T]{
  val MAX = 10
  private val buf = new ListBuffer[T]

  def push(o:T): Unit ={
    if (!full)
      buf.prepend(o)
    else
      throw new IllegalStateException("can't push onto a full stack")
  }
  def pop():T ={
    if (!empty)
      buf.remove(0)
    else
      throw new IllegalStateException("can't pop an empty stack")
  }
  def peek: T ={
    if (!empty)
      buf(0)
    else
      throw new IllegalStateException("can't pop an empty stack")
  }

  def full:Boolean = buf.size == MAX
  def empty:Boolean = buf.size == 0
  def size = buf.size

  override def toString = buf.mkString("Stack(",",",")")
}
//trait StackBehaviours{ this:FlatSpec =>
//  def nonEmptyStack(newStack: => Stack[Int],lastItemAdded:Int): Unit ={
//    it should "be non-empty" in{
//      assert(!newStack.empty)
//    }
//    it should "return the top item on peek" in{
//      assert(newStack.peek === lastItemAdded)
//    }
//    it should "not remove the top item on peek" in{
//      val stack = newStack
//      val size = stack.size
//      assert(stack.peek === lastItemAdded)
//      assert(stack.size === size)
//    }
//    it should "remove the top item on pop" in{
//      val stack = newStack
//      val size = stack.size
//      assert(stack.pop === lastItemAdded)
//      assert(stack.size === size - 1)
//    }
//  }
//  def nonFullStack(newStack:=>Stack[Int]){
//    it should "not be full" in {
//      assert(!newStack.full)
//    }
//
//    it should "add to teh top on push" in {
//      val stack = newStack
//      val size = stack.size
//      stack.push(7)
//      assert(stack.size === size+1)
//      assert(stack.peek === 7)
//    }
//  }
//}

//Call the stack behaviours like:
//it should behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
//it should behave like nonFullStack(stackWithOneItem)

//More functional way
//class sharedTestExampleSpec extends FlatSpec with StackBehaviours{
//  def emptyStack = new Stack[Int]
//  def fullStack = {
//    val stack = new Stack[Int]
//    for(i <- 0 until stack.MAX)
//      stack.push(i)
//    stack
//  }
//  def stackWithOneItem = {
//    val stack = new Stack(Int)
//    stack.push(9)
//    stack
//  }
//  def stackWithOneItemLessThanCapacity = {
//    val stack = new Stack[Int]
//    for(i<- 1 to 9)
//      stack.push(i)
//    stack
//  }
//  val lastValuePushed = 9
//  "A Stack (when empty)" should "be empty" in {
//    assert(emptyStack.empty)
//  }
//  it should "complain on pop" in {
//    intercept[IllegalStateException]{
//      emptyStack.pop
//    }
//
//  }
//  "A stack (with one item)" should behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
//  it should behave like nonFullStack(stackWithOneItem)
//  "A stack (with one item less than capacity)" should behave like nonEmptyStack(stackWithOneItemLessThanCapacity, lastValuePushed)
//  it should behave like nonFullStack(stackWithOneItemLessThanCapacity)
//  "A stack (full)" should "be full" in{
//    assert(fullStack.full)
//  }
//  it should behave like nonEmptyStack(fullStack,lastValuePushed)
//  it should "complain on a push" in{
//    intercept[IllegalStateException]{
//      fullStack.push(10)
//    }
//  }
//}

class StackSpecTest extends FlatSpec{
  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
  }

  ignore should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[String]
    assertThrows[NoSuchElementException] {
      emptyStack.pop()
    }
  }
}

object DBTest extends Tag("com.mycompany.tags.DBTest")

import DBServer._
import org.scalatest.fixture

class ExampleFlatSpec extends FlatSpec with BeforeAndAfter {
  val builder = new StringBuilder
  val buffer = new ListBuffer[String]

  before {
  builder.append("ScalaTest is " )
  }
  after {
    builder.clear()
    buffer.clear()
  }

  "testing" should "be easy" in {
    builder.append("easy!")
    assert(builder.toString === "ScalaTest is easy!")
    assert(buffer.isEmpty)
    buffer += "sweet"
  }
  it should "be fun" in {
    builder.append("fun!")
    assert(builder.toString === "ScalaTest is fun!")
    assert(buffer.isEmpty)
  }
//  override def withFixture(test: NoArgTest) = {
//    super.withFixture(test) match {
//      case failed: Failed =>
//        val currDir = new File(".")
//        val fileNames = currDir.list()
//        info("Dir snapshot: " + fileNames.mkString(", "))
//        failed
//      case other => other
//    }
//  }


//  "this test" should "succeed" in {
//    assert(1 + 1 === 2)
//  }
//  it should "fail" in {
//    assert(1 + 1 === 3)
//  }


//  //Loan-Fixture methods
//  //pass a fixture object into the test AND perform cleanup at the end
//  def withDatabase(testCode: Db => Any) = {
//    val dbName = randomUUID.toString
//    val db = createDB(dbName) //create fixture
//    try {
//      db.append("ScalaTest is ")
//      testCode(db)
//    }
//    finally removeDB(dbName)
//  }
//
//  def withFile(testCode: (File, FileWriter) => Any) = {
//    val file = File.createTempFile("hello", "world") //create temp fixture
//    val writer = new FileWriter(file)
//    try {
//      writer.write("ScalaTest is ") //set up fixture
//      testCode(file, writer) //"loan fixture to the test
//    }
//    finally writer.close() //clean up fixture
//  }
//
//  //Test needs file fixture
//  "Testing" should "be productive" in withFile { (file, writer) =>
//    writer.write("productive!")
//    writer.flush()
//    assert(file.length === 24)
//  }
//  //test needs database fixture
//  "Test code" should "be readable" in withDatabase { db =>
//    db.append("readable!")
//    assert(db.toString === "ScalaTest is readable!")
//  }
//  //test needs both
//  it should "be clear and concise" in withDatabase { db =>
//    withFile { (file, writer) =>
//      db.append("clear!")
//      writer.write("concise!")
//      writer.flush()
//      assert(db.toString === "ScalaTest is clear!")
//      assert(file.length === 21)
//    }
//  }

//  //get-fixture methods
//  def fixture = new {
//    val builder = new StringBuilder("ScalaTest is ")
//    val buffer = new ListBuffer[String]
//  }
//
//  "Testing" should "be easy" in {
//    val f = fixture
//    f.builder.append("easy!")
//    assert(f.builder.toString === "ScalaTest is easy!")
//    assert(f.buffer.isEmpty)
//    f.buffer += "sweet"
//  }
//
//  it should "be fun" in {
//    val f = fixture
//    f.builder.append("fun!")
//    assert(f.builder.toString === "ScalaTest is fun!")
//    assert(f.buffer.isEmpty)
//  }
//
//  "The Scala language" must "add correctly" taggedAs (Slow) in {
//    val sum = 1 + 1
//    assert(sum === 2)
//  }
//
//  it must "subtract correctly" taggedAs(Slow, DBTest) in {
//    val diff = 4 - 1
//    assert(diff === 3)
//  }

  //  //fixture-context objects
  //  trait Builder {
  //    val builder = new StringBuilder("ScalaTest is ")
  //  }
  //
  //  trait Buffer {
  //    val buffer = ListBuffer("ScalaTest", "is")
  //  }
  //
  //  //this test needs stringBuilder fixture
  //  "Testing" should "be productive" in new Builder {
  //    builder.append("productive!")
  //    assert(builder.toString === "ScalaTest is productive!")
  //  }
  //  //test needs listBuffer fixture
  //  "Test code" should "be readable" in new Buffer {
  //    buffer += ("readable!")
  //    assert(buffer === List("ScalaTest", "is", "readable!"))
  //  }
  //  //test needs both
  //  it should "be clear and concise" in new Builder with Buffer { //like functions that are called
  //    builder.append("!clear!")
  //    buffer += ("concise!")
  //    assert(builder.toString == "ScalaTest is clear!") //checks if true
  //    assert(buffer === List("ScalaTest", "is", "concise!"))
  //  }
  //
}

class ExampleFixtureFlatSpec extends fixture.FlatSpec {

  case class FixtureParam(file: File, writer: FileWriter)

  def withFixture(test: OneArgTest) = {
    val file = File.createTempFile("hello", "world")
    val writer = new FileWriter(file)
    val theFixture = FixtureParam(file, writer)
    try {
      writer.write("ScalaTest is ")
      withFixture(test.toNoArgTest(theFixture))
    }
    finally writer.close()
  }

  "testing" should "be easy" in { f =>
    f.writer.write("easy")
    f.writer.flush()
    assert(f.file.length === 18)
  }

  it should "be fun" in { f =>
    f.writer.write("fun!")
    f.writer.flush()
    assert(f.file.length === 17)
  }
}
//Assertions
//class MySpecTest extends UnitSpec{
//  val left = 3
//  val right = 1
//  val attempted = 2
//  //assert(attempted == 1,"Execution was attempted " + left + " times instead of 1 time")
////  assertResult(1){
////    left - right
////  }
//
//  //[Failures]
//  //fail()
//
//  //[Success]
//  //succeed
//
//  //[ExpectedExceptions]
//  val s = "hi"
//  try {
//    s.charAt(-1)
//    fail()
//  }
//  catch{
//    case _ : IndexOutOfBoundsException => //Expected so does nothing about it
//  }
//
//  withClue("this is a clue"){
//    assertThrows[IndexOutOfBoundsException] {
//      //Result type: IndexOutOfBoundsException
//      "hi".charAt(-1)
//    }
//  }
//
//  val caught = intercept[IndexOutOfBoundsException] {
//    //Result type: IndexOutOfBoundsException
//    s.charAt(-1)
//  }
//  assert(caught.getMessage.indexOf("-1") != -1)
//
//  assertDoesNotCompile("val a:String = 1")
//  assertTypeError("val a:String = 1")
//  assertCompiles("val a: Int=1")
//
//  //Assumptions
//  //assume(database.isAvailable, "Database was down again")
//
//  //[Forcing cancellations]
//  //cancel("Can't run the test because no internet connection was found")
//
//  //[Clues]
// // assert(1+1 ===3, "this is a clue")
//  //assertResult(3,"this is a clue"){1+1}
//
//}
