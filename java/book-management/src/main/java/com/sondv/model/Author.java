package com.sondv.model;

public class Author {
    private final String email;
    private final String fistName;
    private final String surName;

    public Author(String email, String fistName, String surName) {
        this.email = email;
        this.fistName = fistName;
        this.surName = surName;
    }

    public String getEmail() {
        return email;
    }

    public String getFistName() {
        return fistName;
    }

    public String getSurName() {
        return surName;
    }
}
