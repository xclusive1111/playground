package com.sondv.repository;

import com.sondv.control.CollectionUtil;
import com.sondv.control.Try;
import com.sondv.model.Book;
import com.sondv.model.RawTabularData;
import com.sondv.reader.CsvFileConfig;
import com.sondv.reader.CsvFileReader;
import com.sondv.reader.CsvParser;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

public final class BookRepositoryImpl implements BookRepository {
    private CsvFileReader reader = new CsvFileReader();
    private CsvFileConfig conf = new CsvFileConfig(";", StandardCharsets.ISO_8859_1);

    private static final File dataSource;

    static {
        try {
            URL resource = BookRepositoryImpl.class.getClassLoader().getResource("books.csv");
            dataSource = new File(resource.toURI());
        } catch (URISyntaxException e) {
            throw new RuntimeException("Failed to load books.csv");
        }
    }

    @Override
    public Try<Book> findByIsbn(String isbn) {
        Function<RawTabularData, Try<Book>> find = table -> CsvParser.parseBooks(table)
            .flatMap(books -> CollectionUtil.findFirst(books, book -> book.getIsbn().equals(isbn)));

        return reader.apply(conf, dataSource).flatMap(find);
    }

    @Override
    public Try<List<Book>> findByAuthor(String authorEmail) {
        Function<RawTabularData, Try<List<Book>>> find = table -> CsvParser.parseBooks(table)
            .map(books -> CollectionUtil.findAll(books, book -> book.getAuthors().contains(authorEmail)));

        return reader.apply(conf, dataSource).flatMap(find);
    }

    @Override
    public List<Book> findAll() {
        return reader.apply(conf, dataSource)
            .flatMap(CsvParser::parseBooks)
            .fold(list -> list, failCause -> Collections.emptyList());
    }

}
