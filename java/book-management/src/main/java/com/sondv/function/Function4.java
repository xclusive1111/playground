package com.sondv.function;

/**
 * Represent a function of 4 parameters.
 * @param <T1> Type of the 1st parameter.
 * @param <T2> Type of the 2nd parameter.
 * @param <T3> Type of the 3rd parameter.
 * @param <T4> Type of the 4th parameter.
 * @param <R> Type of the result.
 */
@FunctionalInterface
public interface Function4<T1, T2, T3, T4, R> {
    R apply(T1 t1, T2 t2, T3 t3, T4 t4);
}
