package com.sondv.model;

import java.util.List;

/**
 * A raw table consist of many rows.
 * Each row is a list of columns.
 * Each column is a {@link KeyValue} pair with key is a string and value is a string, hence the name 'raw'.
 */
public class RawTabularData {
    /**
     * A two dimensional array represent a table */
    private final List<List<KeyValue<String, String>>> rows;

    public RawTabularData(List<List<KeyValue<String, String>>> rows) {
        this.rows = rows;
    }

    public List<List<KeyValue<String, String>>> getRows() {
        return rows;
    }
}
