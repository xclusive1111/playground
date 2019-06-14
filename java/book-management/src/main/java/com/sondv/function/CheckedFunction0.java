package com.sondv.function;

/**
 * Represent a function that return a value and might throw exception while computing.
 * @param <T> Type of the result.
 */
@FunctionalInterface
public interface CheckedFunction0<T> {
    T apply() throws Exception;
}
