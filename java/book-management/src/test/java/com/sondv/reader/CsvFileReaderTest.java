package com.sondv.reader;

import com.sondv.control.Try;
import com.sondv.model.RawTabularData;
import org.junit.Test;

import java.io.File;
import java.nio.charset.Charset;

import static com.sondv.TestUtil.RESOURCE_PATH;
import static org.junit.Assert.assertTrue;

public class CsvFileReaderTest {
    private CsvFileReader reader = new CsvFileReader();
    private CsvFileConfig conf = new CsvFileConfig(";", Charset.defaultCharset());

    @Test
    public void testReadCsv() {
        File file = new File(RESOURCE_PATH + "books.csv");
        Try<RawTabularData> tryBook = reader.apply(conf, file);
        assertTrue(tryBook.isSuccess());
        RawTabularData table = tryBook.getValue();
        assertTrue(table.getRows().size() == 8);

        // File does not exist
        File tmpFile = new File(RESOURCE_PATH + "/blah.csv");
        Try<RawTabularData> tryBook2 = reader.apply(conf, tmpFile);
        assertTrue(tryBook2.isFailure());
    }
}