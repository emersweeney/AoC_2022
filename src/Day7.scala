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
  val totalDiskSpace: Int = 70000000
  val spaceManager = SpaceManager(workingDirectory, totalDiskSpace)

  /* read input and delegate action */
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

  /* execute linux commands */
  def execute(command: String): Unit = {
    command match {
      case "ls" => ()
      case "cd /" => navigator./()
      case "cd .." => navigator.up()
      case s"cd $dir" => navigator.cd(dir)
    }
  }

  /* get all directories that pass boolean check */
  def getDirectories(directory: Directory, check: Directory => Boolean): List[Directory] = {
    var list: List[Directory] = List.empty
    if (check(directory)){
      val newList = list ++ List(directory)
      list = newList
    } else ()
    directory.subDirectories.foreach(
      d => {
        val newList = list ++ getDirectories(d, check)
        list = newList
      }
    )
    list
  }

  /* get total size of all directories that have size less than or equal to 100 000 */
  def smallDirsCheck(directory: Directory): Boolean = directory.getSize<=100000
  val smallList = getDirectories(workingDirectory, smallDirsCheck)
  println("1: "+ smallList.collect(d => d.getSize).sum)

  /* get size of smallest directory with size at least 30 000 000 */
  val requiredSpace: Int = 30000000
  val spaceToFreeUp: Int = spaceManager.getSpaceNeedingFreed(requiredSpace)
  def deletableDirsCheck(spaceToFreeUp: Int)(directory: Directory): Boolean = directory.getSize>=spaceToFreeUp
  val deletableDirs = getDirectories(workingDirectory, deletableDirsCheck(spaceToFreeUp))
  println("2: "+deletableDirs.min.getSize)
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

case class SpaceManager(rootDir: Directory, totalDiskSpace: Int){
  def getSpaceNeedingFreed(requiredSpace: Int): Int = {
    val usedSpace = rootDir.getSize
    val freeSpace = totalDiskSpace-usedSpace
    requiredSpace-freeSpace
  }
}

