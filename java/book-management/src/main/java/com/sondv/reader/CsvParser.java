package com.sondv.reader;

import com.sondv.DateUtil;
import com.sondv.control.Try;
import com.sondv.function.CheckedFunction1;
import com.sondv.model.*;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public final class CsvParser {
    /**
     * Given a list of columns, find a value of a column that has label 'email' */
    private static Try<String> findEmail(List<KeyValue<String, String>> cols) {
        return findCol("email", cols);
    }

    /**
     * Given a list of columns, find a value of a column that has label 'firstname' */
    private static Try<String> findFirstName(List<KeyValue<String, String>> cols) {
        return findCol("firstname", cols);
    }

    /**
     * Given a list of columns, find a value of a column that has label 'lastname' */
    private static Try<String> findSurName(List<KeyValue<String, String>> cols) {
        return findCol("lastname", cols);
    }

    /**
     * Given a list of columns, find a value of a column that has label 'title' */
    private static Try<String> findTitle(List<KeyValue<String, String>> cols) {
        return findCol("title", cols);
    }

    /**
     * Given a list of columns, find a value of a column that has label 'isbn' */
    private static Try<String> findIsbn(List<KeyValue<String, String>> cols) {
        return findCol("isbn", cols);
    }

    /**
     * Given a list of columns, find a value of a column that has label 'description' */
    private static Try<String> findDescription(List<KeyValue<String, String>> cols) {
        return findCol("description", cols);
    }

    /**
     * Given a list of columns, find a value of a column that has label 'publicationdate' */
    private static Try<Date> findPublicationDate(List<KeyValue<String, String>> cols) {
        return findCol("publicationdate", cols)
            .flatMap(DateUtil::parseDefault);
    }

    /**
     * Given a list of columns, find a value of a column that has label 'authors' */
    private static Try<List<String>> findAuthors(List<KeyValue<String, String>> cols) {
        return findCol("authors", cols)
            .map(authorStr -> Arrays.asList(authorStr.split(",")));
    }

    /**
     * Given a list of columns, find a value of a column that has label 'author' */
    private static Try<List<String>> findAuthor(List<KeyValue<String, String>> cols) {
        return findCol("author", cols)
            .map(authorStr -> Arrays.asList(authorStr.split(",")));
    }

    /**
     * Given a list of columns, returns a function that take a {@link RawTabularData} and return a list of type <R> */
    private static <R> CheckedFunction1<RawTabularData, List<R>> getParserFn(Function<List<KeyValue<String, String>>, Try<R>> mapper) {
        return table -> table.getRows().stream()
            .map(mapper)
            .filter(Try::isSuccess)
            .map(Try::getValue)
            .collect(Collectors.toList());
    }

    /**
     * Try to get a list of authors from a give {@link RawTabularData} */
    public static Try<List<Author>> parseAuthors(RawTabularData table) {
        Function<List<KeyValue<String, String>>, Try<Author>> toAuthor = cols ->
            Try.map3(findEmail(cols), findFirstName(cols), findSurName(cols), Author::new);

        return parse(table, getParserFn(toAuthor));
    }

    /**
     * Try to get a list of books from a give {@link RawTabularData} */
    public static Try<List<Book>> parseBooks(RawTabularData table) {
        Function<List<KeyValue<String, String>>, Try<Book>> toBook = cols ->
            Try.map4(findTitle(cols), findIsbn(cols), findAuthors(cols), findDescription(cols), Book::new);

        return parse(table, getParserFn(toBook));
    }

    /**
     * Try to get a list of magazines from a give {@link RawTabularData} */
    public static Try<List<Magazine>> parseMagazines(RawTabularData table) {
        Function<List<KeyValue<String, String>>, Try<Magazine>> toMagazine = cols ->
            Try.map4(findTitle(cols), findIsbn(cols), findAuthor(cols), findPublicationDate(cols), Magazine::new);

        return parse(table, getParserFn(toMagazine));
    }

    /**
     * Try to get a list of type <R> from a give {@link RawTabularData} */
    public static <R> Try<List<R>> parse(RawTabularData table, CheckedFunction1<RawTabularData, List<R>> parser) {
        return Try.of(() -> parser.apply(table));
    }

    /**
     * Find a value associates a column name */
    public static Try<String> findCol(String colName, List<KeyValue<String, String>> cols) {
        Optional<String> value = cols.stream()
            .filter(kv -> kv.key().equals(colName))
            .findFirst()
            .map(KeyValue::value);
        return Try.fromOptional(value);
    }
}
