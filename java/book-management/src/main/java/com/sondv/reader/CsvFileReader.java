package com.sondv.reader;

import com.sondv.control.CollectionUtil;
import com.sondv.control.Try;
import com.sondv.model.KeyValue;
import com.sondv.model.RawTabularData;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * An implementation of reading a CSV file and return a {@link RawTabularData}.
 */
public class CsvFileReader implements RawFileReader<CsvFileConfig, RawTabularData> {

    @Override
    public Try<RawTabularData> apply(CsvFileConfig conf, File file) {
        return Try.of(() -> {
            final String delimiter = conf.getDelimiter();
            final Charset charSet = conf.getCharset();
            boolean isFirstRow = true;

            List<String> headers = new ArrayList<>();
            List<List<KeyValue<String, String>>> rows = new ArrayList<>();

            try (
                FileInputStream is = new FileInputStream(file);
                InputStreamReader isr = new InputStreamReader(is, charSet);
                BufferedReader br = new BufferedReader(isr);
                )
            {
                String line;
                while ((line = br.readLine()) != null) {
                    if (isFirstRow) {
                        headers = Arrays.asList(line.split(delimiter));
                    } else {
                        List<KeyValue<String, String>> row = readRow(line, headers, delimiter);
                        rows.add(row);
                    }
                    isFirstRow = false;
                }

            }
            return new RawTabularData(rows);
        });
    }

    /**
     * Convert a string into a list of pairs. */
    private List<KeyValue<String, String>> readRow(String line, List<String> headers, String delimiter) {
        List<String> cols = Arrays.asList(line.split(delimiter));
        return CollectionUtil.zip2(headers, cols);
    }

}
