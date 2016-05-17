import java.util.UUID

trait Genre
case object Sifi extends Genre
case object Epic extends Genre
case object Novel extends Genre
case object Drama extends Genre
case object Documentary extends Genre
case object Biography extends Genre

object Library extends App {

  case class Reader(name: String) {

    def borrow(work: Work, library: Library): Unit = {
      library.lookupByWork(work).find(_.borrower.isEmpty) match {
        case Some(book) =>
          book.borrower = Some(this)
          book.expiration = Some(System.currentTimeMillis() + (7 * 24 * 3600 * 1000))
          println(s"$name successfully borrowed a book ${work.name}")
        case None =>
          println(s"$name failed to borrow book ${work.name}")
      }
    }
  }

  case class Author(name: String)

  case class Work(name: String,
                  genres: Set[Genre],
                  author: Author)

  case class Book(id: String = UUID.randomUUID().toString,
                  work: Work,
                  var borrower: Option[Reader] = None,
                  var expiration: Option[Long] = None)

  case class Library(books: List[Book]) {
    def lookupByWork(work: Work): List[Book] = books.filter(_.work == work)
    def lookupByAuthor(author: Author): List[Book] = books.filter(_.work.author == author)
    def lookupByGenre(genre: Genre): List[Book] = books.filter(_.work.genres.contains(genre))
  }

  override def main(args: Array[String]) {
    val Shakespeare = Author("Shakespeare")
    val GeorgeRRMartin = Author("George R R Martin")
    val GeorgeOrwell = Author("George Orwell")

    val Hamnet = Work("Hamnet", Set(Drama, Novel), Shakespeare)
    val NineteenEightyFour = Work("1984", Set(Drama, Sifi), GeorgeOrwell)
    val ASongOfIceAndFire = Work("A Song of Ice and Fire", Set(Drama, Epic), GeorgeRRMartin)

    val library = Library(List(
      Book(work = Hamnet),
      Book(work = Hamnet),
      Book(work = NineteenEightyFour),
      Book(work = ASongOfIceAndFire),
      Book(work = ASongOfIceAndFire),
      Book(work = ASongOfIceAndFire)
    ))

    val john = Reader("John")
    john.borrow(Hamnet, library)
    john.borrow(Hamnet, library)
    john.borrow(Hamnet, library)
    john.borrow(Hamnet, library)

    library.books.foreach(println)
  }
}
