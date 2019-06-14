package com.sondv.repository;

import com.sondv.control.Try;
import com.sondv.model.Magazine;

import java.util.Comparator;
import java.util.List;

public interface MagazineRepository {
    Try<Magazine> findByIsbn(String isbn);
    Try<List<Magazine>> findByAuthor(String authorEmail);
    List<Magazine> findAll();
}
