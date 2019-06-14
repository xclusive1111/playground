package com.sondv.reader;

import java.nio.charset.Charset;

/**
 * CSV configuration, this class contains all required configuration in order to read a CSV file.
 */
public class CsvFileConfig {
    private final String delimiter;
    private final Charset charset;

    public CsvFileConfig(String delimiter, Charset charset) {
        this.delimiter = delimiter;
        this.charset = charset;
    }

    public String getDelimiter() {
        return delimiter;
    }

    public Charset getCharset() {
        return charset;
    }
}
