package com.sondv.model;

import java.util.Date;
import java.util.List;

public class Magazine extends Product {
    private final Date publicDate;

    public Magazine(String title, String isbn, List<String> authors, Date publicDate) {
        super(title, isbn, authors);
        this.publicDate = publicDate;
    }

    public Date getPublicDate() {
        return publicDate;
    }

    @Override
    public String toString() {
        String authors = String.join(",", getAuthors());
        return String.format("Magazine: \n\tTitle: %s\n\tISBN: %s\n\tAuthors: %s\n\tPublication Date: %s)", getTitle(), getIsbn(), authors, getPublicDate().toString());
    }
}
