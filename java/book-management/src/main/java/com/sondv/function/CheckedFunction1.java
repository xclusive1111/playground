package com.sondv.function;

/**
 * Represent a function of one parameter that might fail.
 * @param <T> Type of the parameter.
 * @param <R> Type of the result.
 */
@FunctionalInterface
public interface CheckedFunction1<T, R> {
    R apply(T t) throws Exception;
}
