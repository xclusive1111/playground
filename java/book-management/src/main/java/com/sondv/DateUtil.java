package com.sondv;

import com.sondv.control.Try;

import java.text.SimpleDateFormat;
import java.util.Date;

public class DateUtil {
    private final static SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy");

    public static Try<Date> parse(String str, SimpleDateFormat formatter) {
        return Try.of(() -> formatter.parse(str));
    }

    public static Try<Date> parseDefault(String str) {
        return parse(str, formatter);
    }

}
