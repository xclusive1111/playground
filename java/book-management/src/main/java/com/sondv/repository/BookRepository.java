package com.sondv.repository;

import com.sondv.control.Try;
import com.sondv.model.Book;

import java.util.List;

public interface BookRepository {
    Try<Book> findByIsbn(String isbn);
    Try<List<Book>> findByAuthor(String authorEmail);
    List<Book> findAll();
}
