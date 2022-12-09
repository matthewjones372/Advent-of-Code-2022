package day7

import zio.Chunk
import zio.prelude.*
import zio.prelude.Associative.*

object ElfFiles extends Subtype[Map[Chunk[String], Chunk[File]]]:
  extension (elfFiles: ElfFiles)
    def update(path: Chunk[String], file: File): ElfFiles =
      elfFiles <> ElfFiles(Map(path -> Chunk(file)))

    def childDirsFrom(directory: Directory): Chunk[Directory] =
      Directory.from(elfFiles.keys.filter(_.startsWith(directory)).toChunk)

    def directorySum(path: Chunk[String]): Int =
      ElfFiles.unwrap(elfFiles).get(path).map(_.map(_.size).sum).getOrElse(0)

  def empty = ElfFiles(Map.empty)

  given Associative[ElfFiles] =
    new Associative[ElfFiles]:
      override def combine(l: => ElfFiles, r: => ElfFiles): ElfFiles =
        ElfFiles(ElfFiles.unwrap(l) <> ElfFiles.unwrap(r))

type ElfFiles = ElfFiles.Type

object Directory extends Subtype[Chunk[String]]:
  val root = Directory(Chunk.empty)

  def from(dir: Chunk[Chunk[String]]): Chunk[Directory] =
    dir.map((e: Chunk[String]) => Directory(e))

  extension (dir: Directory)
    def add(name: String): Directory =
      Directory(dir.appended(name))

    def dropLast: Directory =
      Directory(dir.dropRight(1))

type Directory = Directory.Type

object Listings extends Subtype[Chunk[Directory]]:
  val root = Listings(Chunk(Directory.root))

  extension (listings: Chunk[Directory])
    def add(directory: Directory): Listings =
      Listings(listings.appended(directory))

    def directorySizes(elfFiles: ElfFiles) = listings.distinct.map { dir =>
      elfFiles.childDirsFrom(dir).map(elfFiles.directorySum)
    }.map(_.sum)

type Listings = Listings.Type

final case class File(size: Int) extends AnyVal
