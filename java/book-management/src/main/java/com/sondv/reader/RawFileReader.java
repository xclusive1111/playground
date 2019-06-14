package com.sondv.reader;

import com.sondv.control.Try;

import java.io.File;
import java.util.function.BiFunction;

/**
 * Represent a function that take a {@link File} along with a configuration and return a type <R>.
 * @param <C> Type of the configuration.
 * @param <R> Type of the result.
 */
public interface RawFileReader<C, R> extends BiFunction<C, File, Try<R>> {
    Try<R> apply(C conf, File file);
}
