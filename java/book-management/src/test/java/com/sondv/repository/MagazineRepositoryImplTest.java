package com.sondv.repository;

import com.sondv.control.Try;
import com.sondv.model.Magazine;
import com.sondv.model.Product;
import org.junit.Test;

import java.util.Comparator;
import java.util.List;

import static org.junit.Assert.*;

public class MagazineRepositoryImplTest {
    private MagazineRepositoryImpl repo = new MagazineRepositoryImpl();

    /**
     * Requirement #3: Find and print details of magazine given an ISBN
     */
    @Test
    public void findByIsbn() {
        final String isbn = "5454-5587-3210";
        final String email = "pr-walter@optivo.de";

        Try<Magazine> tryByIsbn = repo.findByIsbn(isbn);
        assertTrue(tryByIsbn.isSuccess());
        Magazine mgz = tryByIsbn.getValue();
        assertTrue(mgz.getAuthors().contains(email));
    }

    /**
     * Requirement #3: Find and print details of magazine given an author's email
     */
    @Test
    public void findByAuthor() {
        final String isbn = "5454-5587-3210";
        final String email = "pr-walter@optivo.de";

        Try<List<Magazine>> tryByAuthor = repo.findByAuthor(email);
        assertTrue(tryByAuthor.isSuccess());
        List<Magazine> magazines = tryByAuthor.getValue();
        assertTrue(magazines.size() == 3);
        Magazine mgz = magazines.get(0);
        assertTrue(mgz.getAuthors().contains(email));
    }

    /**
     * Requirement #2: Print out all details of all magazines
     */
    @Test
    public void findAll() {
        List<Magazine> magazines = repo.findAll();
        assertTrue(magazines.size() == 6);
        magazines.forEach(System.out::println);
    }

    /**
     * Requirement #4: Sort all magazines by title and print them out.
     */
    @Test
    public void sortByTitleAsc() {
        List<Magazine> magazines = repo.findAll();
        magazines.sort(Comparator.comparing(Product::getTitle));
        Magazine mgz = magazines.get(0);
        assertTrue(mgz.getIsbn().equals("2547-8548-2541"));
        assertTrue(mgz.getTitle().equals("Der Weinkenner"));
        magazines.forEach(System.out::println);
    }

    /**
     * Requirement #4: Sort all magazines by title and print them out.
     */
    @Test
    public void sortByTitleDesc() {
        List<Magazine> magazines = repo.findAll();
        magazines.sort((b1, b2) -> b2.getTitle().compareTo(b1.getTitle()));
        Magazine mgz = magazines.get(0);
        assertTrue(mgz.getIsbn().equals("1313-4545-8875"));
        assertTrue(mgz.getTitle().equals("Vinum"));
        magazines.forEach(System.out::println);
    }
}