package com.sondv.model;

import java.util.List;

public class Book extends Product {
    private final String description;

    public Book(String title, String isbn, List<String> authors, String description) {
        super(title, isbn, authors);
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    @Override
    public String toString() {
        String authors = String.join(",", getAuthors());
        return String.format("Book: \n\tTitle: %s\n\tISBN: %s\n\tAuthors: %s\n\tDescription: %s)", getTitle(), getIsbn(), authors, getDescription());
    }
}
