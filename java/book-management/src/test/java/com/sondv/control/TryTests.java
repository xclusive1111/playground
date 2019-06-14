package com.sondv.control;

import com.sondv.function.CheckedFunction1;
import org.junit.Test;

import java.util.function.BiFunction;
import java.util.function.Function;

import static org.junit.Assert.*;

public class TryTests {

    private Function<Integer, Double> times2 = n -> (double) (n * 2);

    private Function<Throwable, Double> defaultValue = cause -> {
        System.out.println("Opps!, something went wrong. Reason: " + cause.getMessage());
        return 0d;
    };

    @Test
    public void testOf() {
        Try<Integer> tryInt1 = Try.of(() -> Integer.parseInt("123a"));
        assertTrue(tryInt1.isFailure());

        Try<Integer> tryInt2 = Try.of(() -> Integer.parseInt("123"));
        assertTrue(tryInt2.isSuccess());
    }

    @Test
    public void testPeek() {
        final int[] nums = {0};
        Try<Integer> tryInt = Try.of(() -> 10).peek(num -> nums[0] = num);
        assertTrue(tryInt.getValue() == 10);
        assertTrue(nums[0] == 10);
    }

    @Test
    public void testFold() {
        Try<Integer> tryInt = Try.of(() -> Integer.parseInt("123a"));
        Double num = tryInt.fold(times2, defaultValue);
        assertTrue(num == 0d);

        Try<Integer> tryInt2 = Try.of(() -> Integer.parseInt("10"));
        Double num2 = tryInt2.fold(times2, defaultValue);
        assertTrue(num2 == 20);
    }

    @Test
    public void testMap() {
        Try<Double> tryDouble = Try.of(() -> Integer.parseInt("20")).map(times2);
        assertTrue(tryDouble.isSuccess());
        assertTrue(tryDouble.getValue() == 40d);

        Try<Double> tryDouble2 = Try.of(() -> Integer.parseInt("20a")).map(times2);
        assertTrue(tryDouble2.isFailure());
        assertTrue(tryDouble2.getCause().getMessage() != null);
    }

    @Test
    public void testMapTry() {
        Try<Integer> tryParseInt = Try.of(() -> "abc").mapTry(Integer::parseInt);
        assertTrue(tryParseInt.isFailure());

        Try<Integer> tryParseInt2 = Try.of(() -> "20").mapTry(Integer::parseInt);
        assertTrue(tryParseInt2.isSuccess());
        assertTrue(tryParseInt2.getValue() == 20);
    }

    @Test
    public void testFlatMap() {
        Function<String, Try<Integer>> toInt = str -> Try.of(() -> Integer.parseInt(str));
        Try<Integer> tryParseInt = Try.of(() -> "20").flatMap(toInt);
        assertTrue(tryParseInt.isSuccess());
        assertTrue(tryParseInt.getValue() == 20);
    }

    @Test
    public void testFlatMapTry() {
        CheckedFunction1<String, Try<Integer>> toInt = str -> Try.of(() -> Integer.parseInt(str));
        Try<Integer> tryParseInt = Try.of(() -> "20").flatMapTry(toInt);
        assertTrue(tryParseInt.isSuccess());
        assertTrue(tryParseInt.getValue() == 20);
    }

    @Test
    public void testRecover() {
        Try<Double> tryParseInt = Try.of(() -> Double.parseDouble("abcd"));
        assertTrue(tryParseInt.isFailure());

        Try<Double> recover = tryParseInt.recover(defaultValue);
        assertTrue(recover.isSuccess());
        assertTrue(recover.getValue() == 0d);
    }

    @Test
    public void recoverTry() {
        Try<Double> tryParseInt = Try.of(() -> Double.parseDouble("abcd"));
        assertTrue(tryParseInt.isFailure());

        Function<Throwable, Try<Double>> defaultDouble = cause -> Try.of(() -> defaultValue.apply(cause));
        Try<Double> doubleTry = tryParseInt.recoverTry(defaultDouble);
        assertTrue(doubleTry.isSuccess());
        assertTrue(doubleTry.getValue() == 0d);
    }

    @Test
    public void testMap2() {
        Try<Integer> tryInt1 = Try.of(() -> Integer.parseInt("20"));
        Try<Integer> tryInt2 = Try.of(() -> Integer.parseInt("10"));
        BiFunction<Integer, Integer, Double> plus = (n1, n2) -> (double) (n1 * n2);
        Try<Double> doubleTry = Try.map2(tryInt1, tryInt2, plus);
        assertTrue(doubleTry.isSuccess());
        assertTrue(doubleTry.getValue() == 200);

        Try<Integer> tryInt3 = Try.of(() -> Integer.parseInt("ab"));
        Try<Double> doubleTry2 = Try.map2(tryInt1, tryInt3, plus);
        assertTrue(doubleTry2.isFailure());
    }

}