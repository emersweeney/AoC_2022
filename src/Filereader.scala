trait Filereader extends App {
  def filename: String = "resources/" + getClass.getName.toLowerCase.dropRight(1)
}
