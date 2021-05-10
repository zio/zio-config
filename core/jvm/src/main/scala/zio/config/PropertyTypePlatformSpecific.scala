package zio.config

import zio.config.PropertyType.PropertyReadError

import java.io.File
import java.net.{URI, URL}

trait PropertyTypePlatformSpecific {

  protected def attempt[A, E](a: => A, f: Throwable => E): Either[E, A]

  case object UriType extends PropertyType[String, URI] {
    def read(value: String): Either[PropertyReadError[String], URI] =
      attempt(new URI(value), _ => PropertyReadError(value, "uri"))
    def write(value: URI): String                                   = value.toString
  }

  case object FileType extends PropertyType[String, File] {
    def read(value: String): Either[PropertyReadError[String], File] =
      attempt(new File(value), _ => PropertyReadError(value, "file"))

    def write(value: File): String = value.toString
  }

  case object UrlType extends PropertyType[String, URL] {
    def read(value: String): Either[PropertyReadError[String], URL] =
      attempt(new URL(value), _ => PropertyReadError(value, "url"))

    def write(value: URL): String = value.toString
  }

  case object JavaFilePathType extends PropertyType[String, java.nio.file.Path] {
    def read(value: String): Either[PropertyReadError[String], java.nio.file.Path] =
      attempt(java.nio.file.Paths.get(value), _ => PropertyReadError(value, " java.nio.file.Path"))

    def write(value: java.nio.file.Path): String = value.toString
  }

}
