import java.nio.file.Paths
import TestHelpers.MockSilFrontend
import org.scalatest.FunSuite
import viper.silver.ast._
import java.io.File


class MyTests extends FunSuite {


  //test("going big") {
    //val testDirectories: Seq[String] =   Seq("all", "quantifiedpermissions", "quantifiedpredicates", "quantifiedcombinations", "termination", "examples")
      //val path = Paths.get("..", "resources", "all", "basic")
      val frontend = new MockSilFrontend

      val test = getClass.getResource("all/basic/abstract_funcs_and_preds.vpr")
      if (test != null) println(test.getPath)
      else println("woops")

      val path = getClass.getResource("all/basic")
      val folder = new File(path.getPath)
      if (folder.exists && folder.isDirectory)
        folder.listFiles
          .toList
          .foreach(file =>
            test("testing" + file.getName) {
              parse("all/basic/" + file.getName, frontend)
            }
          )

    //  parse(fileName, frontend)
//  }


  private def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }


  private def parse(testFile: String, frontend: MockSilFrontend): Unit = {

    val fileRes = getClass.getResource(testFile)
    assert(fileRes != null, s"File $testFile not found")
    val file = Paths.get(fileRes.toURI)
    print("halibut\n")
    var targetNode: Node = null

    frontend.translate(file) match {
      case (Some(p), _) => targetNode = p
      case (None, errors) => sys.error("Error occurred during translating: " + errors)
    }
    assert (targetNode != null)
  }
}
