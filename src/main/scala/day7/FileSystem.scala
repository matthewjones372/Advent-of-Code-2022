package day7

import zio.Chunk

final case class FileSystem(currentDirectory: Directory, listings: Listings, elfFiles: ElfFiles) {
  def breakout: FileSystem =
    copy(currentDirectory = currentDirectory.dropLast)

  def addDir(dirName: String): FileSystem =
    copy(listings = listings.add(currentDirectory.add(dirName)))

  def updateCurrentDir(dirName: String): FileSystem =
    copy(currentDirectory = currentDirectory.add(dirName))

  def addFileSize(fileSize: String): FileSystem =
    copy(elfFiles = elfFiles.update(currentDirectory, file = File(fileSize.toInt)))

  def dirSizes: Chunk[Int] =
    listings.directorySizes(elfFiles)
}

object FileSystem {
  def empty: FileSystem = FileSystem(Directory.root, Listings.root, ElfFiles.empty)

  def from(lines: Seq[String]): FileSystem = lines.drop(1).foldLeft(FileSystem.empty) { case (fileSystem, command) =>
    command match
      case "$ cd .." =>
        fileSystem.breakout
      case "$ ls" =>
        fileSystem
      case s"dir $dirName" =>
        fileSystem.addDir(dirName)
      case s"$$ cd $dirName" =>
        fileSystem.updateCurrentDir(dirName).addDir(dirName)
      case s"${fileSize} $_" =>
        fileSystem.addFileSize(fileSize)
  }
}
