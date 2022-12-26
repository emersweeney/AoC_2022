import scala.io.Source
import scala.util.Using

/*
  1: Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those
  directories?
  2: Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update.
  What is the total size of that directory?
 */
object Day7 extends Filereader {

  /* base directory to act as pointer to root directory */
  val workingDirectory: Directory = Directory("Working Directory", null)
  val navigator = Navigator(workingDirectory)

  Using(Source.fromFile(filename)) {
    source =>
      for (line <- source.getLines()){
        line match {
          case s"$$ $command" => execute(command)
          case s"dir $directory" => navigator.addDir(directory)
          case _ => navigator.addFile(line)
        }
      }
  }

  def execute(command: String): Unit = {
    command match {
      case "ls" => ()
      case "cd /" => navigator./()
      case "cd .." => navigator.up()
      case s"cd $dir" => navigator.cd(dir)
    }
  }

  /* collect directories of size at most 100,000 to smallDirs list */
  var smallDirs: List[Directory] = List.empty
  def addSmallDirs(directory: Directory): Unit = {
    if (directory.getSize<=100000){
      val newList = smallDirs ++ List(directory)
      smallDirs = newList
    } else ()
    directory.subDirectories.foreach(d => addSmallDirs(d))
  }

  val totalDiskSpace = 70000000
  val requiredSpace = 30000000
  val usedSpace = workingDirectory.getSize
  val freeSpace = totalDiskSpace-usedSpace
  val spaceToFreeUp = requiredSpace-freeSpace

  var potentialDirsToDelete: List[Directory] = List.empty
  def addPotentialDirs(directory: Directory): Unit = {
    if (directory.getSize>=spaceToFreeUp){
      val newList = potentialDirsToDelete ++ List(directory)
      potentialDirsToDelete = newList
    } else ()
    directory.subDirectories.foreach(d => addPotentialDirs(d))
  }

  addSmallDirs(workingDirectory)
  println("1: "+ smallDirs.collect(d => d.getSize).sum)

  addPotentialDirs(workingDirectory)
  println("2: "+potentialDirsToDelete.min.getSize)

}

case class File(name: String, size: Int)

case class Directory(name: String, parentDir: Directory) extends Ordered[Directory]{

  var files: List[File] = List.empty
  var subDirectories: List[Directory] = List.empty

  def addFile(file: File): Unit = {
    val newFiles = files.++(List(file))
    files = newFiles
  }

  def addDir(dir: Directory): Unit = {
    val newDirs = subDirectories.++(List(dir))
    subDirectories = newDirs
  }

  def getDir(name: String): Option[Directory] = {
    subDirectories.find(d => d.name==name)
  }

  def getSize: Int = {
    var size: Int = 0
    if (files.nonEmpty){
      for (file <- files){
        size += file.size
      }
    }
    if (subDirectories.nonEmpty){
      for (directory <- subDirectories){
        size += directory.getSize
      }
    }
    size
  }

  override def toString: String = "\n"+name+" ("+subDirectories.size+" directories, "+files.size+" files)"

  def compare(that: Directory): Int = this.getSize compare that.getSize
}

case class Navigator(baseDir: Directory){
  var currentDirectory: Directory = baseDir

  def /(): Unit ={
    baseDir.addDir(Directory("/", baseDir))
    currentDirectory = baseDir.getDir("/").getOrElse(currentDirectory)
  }

  def up(): Unit = currentDirectory = currentDirectory.parentDir

  def cd(dir: String): Unit = {
    val newDir = currentDirectory.getDir(dir)
    currentDirectory = newDir.getOrElse(currentDirectory)
  }

  def addDir(name: String): Unit = currentDirectory.addDir(Directory(name, currentDirectory))

  def addFile(line: String): Unit = {
    val arr = line.split(" ")
    val (name, size) = (arr(1), arr(0).toInt)
    currentDirectory.addFile(File(name, size))
  }
}

