package com.sondv.reader;

import com.sondv.control.Try;
import com.sondv.model.Author;
import com.sondv.model.Book;
import com.sondv.model.Magazine;
import org.junit.Test;

import java.io.File;
import java.nio.charset.Charset;
import java.util.List;

import static com.sondv.TestUtil.RESOURCE_PATH;
import static org.junit.Assert.assertTrue;

public class CsvParserTest {
    private CsvFileReader reader = new CsvFileReader();
    private CsvFileConfig conf = new CsvFileConfig(";", Charset.defaultCharset());

    /**
     * Requirement #1: Read data from authors.csv file
     */
    @Test
    public void parseAuthors() {
        File file = new File(RESOURCE_PATH + "authors.csv");
        Try<List<Author>> authorsTry = reader.apply(conf, file)
            .flatMap(table -> CsvParser.parseAuthors(table));
        assertTrue(authorsTry.isSuccess());
        List<Author> authors = authorsTry.getValue();
        assertTrue(authors.size() == 6);
    }

    /**
     * Requirement #1: Read data from books.csv file
     */
    @Test
    public void parseBooks() {
        File file = new File(RESOURCE_PATH + "books.csv");
        Try<List<Book>> booksTry = reader.apply(conf, file)
            .flatMap(table -> CsvParser.parseBooks(table));
        assertTrue(booksTry.isSuccess());
        List<Book> books = booksTry.getValue();
        assertTrue(books.size() == 8);
    }

    /**
     * Requirement #1: Read data from magazines.csv file
     */
    @Test
    public void parseMagazines() {
        File file = new File(RESOURCE_PATH + "magazines.csv");
        Try<List<Magazine>> magazineTry = reader.apply(conf, file)
            .flatMap(table -> CsvParser.parseMagazines(table));
        assertTrue(magazineTry.isSuccess());
        List<Magazine> magazines = magazineTry.getValue();
        assertTrue(magazines.size() == 6);
    }
}