package com.sondv.model;

import java.util.List;

public class Product {
    private final String title;
    private final String isbn;
    private final List<String> authors;

    public Product(String title, String isbn, List<String> authors) {
        this.title = title;
        this.isbn = isbn;
        this.authors = authors;
    }

    public String getIsbn() {
        return isbn;
    }

    public List<String> getAuthors() {
        return authors;
    }

    public String getTitle() {
        return title;
    }
}
