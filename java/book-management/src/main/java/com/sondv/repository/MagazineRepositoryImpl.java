package com.sondv.repository;

import com.sondv.control.CollectionUtil;
import com.sondv.control.Try;
import com.sondv.model.Magazine;
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

public final class MagazineRepositoryImpl implements MagazineRepository {
    private CsvFileReader reader = new CsvFileReader();
    private CsvFileConfig conf = new CsvFileConfig(";", StandardCharsets.ISO_8859_1);

    private static final File dataSource;

    static {
        try {
            URL resource = MagazineRepositoryImpl.class.getClassLoader().getResource("magazines.csv");
            dataSource = new File(resource.toURI());
        } catch (URISyntaxException e) {
            throw new RuntimeException("Failed to load magazines.csv");
        }
    }

    @Override
    public Try<Magazine> findByIsbn(String isbn) {
        Function<RawTabularData, Try<Magazine>> find = table -> CsvParser.parseMagazines(table)
            .flatMap(magazines -> CollectionUtil.findFirst(magazines, magazine -> magazine.getIsbn().equals(isbn)));

        return reader.apply(conf, dataSource).flatMap(find);
    }

    @Override
    public Try<List<Magazine>> findByAuthor(String authorEmail) {
        Function<RawTabularData, Try<List<Magazine>>> find = table -> CsvParser.parseMagazines(table)
            .map(magazines -> CollectionUtil.findAll(magazines, magazine -> magazine.getAuthors().contains(authorEmail)));

        return reader.apply(conf, dataSource).flatMap(find);
    }

    @Override
    public List<Magazine> findAll() {
        return reader.apply(conf, dataSource)
            .flatMap(CsvParser::parseMagazines)
            .fold(list -> list, failCause -> Collections.emptyList());
    }

}
