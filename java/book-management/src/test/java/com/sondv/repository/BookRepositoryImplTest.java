package com.sondv.repository;

import com.sondv.control.Try;
import com.sondv.model.Book;
import com.sondv.model.Product;
import org.junit.Test;

import java.util.Comparator;
import java.util.List;

import static org.junit.Assert.*;

public class BookRepositoryImplTest {
    private BookRepositoryImpl repo = new BookRepositoryImpl();

    /**
     * Requirement #3: Find and print details of book given an ISBN
     */
    @Test
    public void findByIsbn() {
        final String isbn = "5554-5545-4518";
        final String title = "Ich helf dir kochen. Das erfolgreiche Universalkochbuch mit großem Backteil";
        final String email = "pr-walter@optivo.de";
        final String desc = "Auf der Suche nach einem Basiskochbuch steht man heutzutage vor einer Fülle von Alternativen. Es fällt schwer, daraus die für sich passende Mixtur aus Grundlagenwerk und Rezeptesammlung zu finden. Man sollte sich darüber im Klaren sein, welchen Schwerpunkt man setzen möchte oder von welchen Koch- und Backkenntnissen man bereits ausgehen kann.";

        Try<Book> bookTry = repo.findByIsbn(isbn);
        assertTrue(bookTry.isSuccess());
        Book book = bookTry.getValue();
        assertTrue(book.getIsbn().equals(isbn));
        assertTrue(book.getTitle().equals(title));
        assertTrue(book.getDescription().equals(desc));
        assertTrue(book.getAuthors().contains(email));
        System.out.println(book);
    }

    /**
     * Requirement #3: Find and print details of book given an author's email
     */
    @Test
    public void findByAuthor() {
        final String isbn = "5554-5545-4518";
        final String title = "Ich helf dir kochen. Das erfolgreiche Universalkochbuch mit großem Backteil";
        final String email = "pr-walter@optivo.de";
        final String desc = "Auf der Suche nach einem Basiskochbuch steht man heutzutage vor einer Fülle von Alternativen. Es fällt schwer, daraus die für sich passende Mixtur aus Grundlagenwerk und Rezeptesammlung zu finden. Man sollte sich darüber im Klaren sein, welchen Schwerpunkt man setzen möchte oder von welchen Koch- und Backkenntnissen man bereits ausgehen kann.";
        Try<List<Book>> tryByAuthor = repo.findByAuthor(email);
        assertTrue(tryByAuthor.isSuccess());
        List<Book> books = tryByAuthor.getValue();
        assertTrue(books.size() == 3);
        Book book = books.get(0);
        assertTrue(book.getIsbn().equals(isbn));
        assertTrue(book.getTitle().equals(title));
        assertTrue(book.getDescription().equals(desc));
        assertTrue(book.getAuthors().contains(email));
        System.out.println(book);
    }

    /**
     * Requirement #2: Print out all details of all books
     */
    @Test
    public void findAll() {
        List<Book> books = repo.findAll();
        assertTrue(books.size() == 8);
        books.forEach(System.out::println);
    }

    /**
     * Requirement #4: Sort all books by title and print them out.
     */
    @Test
    public void sortByTitleAsc() {
        List<Book> books = repo.findAll();
        books.sort(Comparator.comparing(Product::getTitle));
        books.forEach(System.out::println);
        Book book = books.get(books.size() - 1);
        assertTrue(book.getIsbn().equals("1215-4545-5895"));
        assertTrue(book.getTitle().equals("Schuhbecks Kochschule. Kochen lernen mit Alfons Schuhbeck "));
    }

    /**
     * Requirement #4: Sort all books by title and print them out.
     */
    @Test
    public void sortByTitleDesc() {
        List<Book> books = repo.findAll();
        books.sort((b1, b2) -> b2.getTitle().compareTo(b1.getTitle()));
        books.forEach(System.out::println);
        Book book = books.get(0);
        assertTrue(book.getIsbn().equals("1215-4545-5895"));
        assertTrue(book.getTitle().equals("Schuhbecks Kochschule. Kochen lernen mit Alfons Schuhbeck "));
    }
}