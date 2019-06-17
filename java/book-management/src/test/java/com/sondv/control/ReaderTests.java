package com.sondv.control;

import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import static org.junit.Assert.*;

public class ReaderTests {
    @Test
    public void testReaderMonad() {
        Function<Integer, Integer> plus1 = n -> n + 1;
        Integer num = new Reader<>(plus1)
            .flatMap(n -> new Reader<>((Integer i) -> n * i))
            .map(n -> n + 10)
            .run(10);

        assertEquals(120, (int) num);

        Db db = getDb();

        assertTrue(checkLogin(1, "Am_a_foo").run(db));
        assertFalse(checkLogin(2, "blahblah").run(db));
        assertTrue(checkLogin(2, "Am_a_bar").run(db));
        assertFalse(checkLogin(3, "no_password").run(db));
    }

    private Db getDb() {
        HashMap<Integer, String> unames = new HashMap<Integer, String>() {{
            put(1, "foo");
            put(2, "bar");
            put(3, "foobar");
        }};
        HashMap<String, String> passwords = new HashMap<String, String>() {{
            put("foo", "Am_a_foo");
            put("bar", "Am_a_bar");
        }};

        return new Db(unames, passwords);
    }

    private Reader<Db, Optional<String>> findUserName(int uid) {
        Function<Db, Optional<String>> find = db -> Optional.ofNullable(db.unames.get(uid));
        return new Reader<>(find);
    }

    private Reader<Db, Boolean> checkPassword(String uname, String pwd) {
        Function<Db, Boolean> find = db ->
            db.passwords.entrySet()
                .stream()
                .anyMatch(kv -> kv.getKey().equals(uname) && kv.getValue().equals(pwd));

        return new Reader<>(find);
    }

    private Reader<Db, Boolean> checkLogin(int uid, String pwd) {
        Function<Optional<String>, Reader<Db, Boolean>> checkPwd = uidOpt -> uidOpt
            .map(uname -> checkPassword(uname, pwd))
            .orElse(new Reader<>(db -> false));

        return findUserName(uid).flatMap(checkPwd);
    }

    private class Db {
        final Map<Integer, String> unames;
        final Map<String, String> passwords;

        Db(Map<Integer, String> unames, Map<String, String> passwords) {
            this.unames = unames;
            this.passwords = passwords;
        }
    }
}
