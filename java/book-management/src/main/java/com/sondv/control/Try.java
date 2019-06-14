package com.sondv.control;

import com.sondv.function.CheckedFunction0;
import com.sondv.function.CheckedFunction1;
import com.sondv.function.Function3;
import com.sondv.function.Function4;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * A {@link Try} is a data structure that wrap a side-effect computation and
 * allows to compose sequential computations in a functional way.
 *
 * Instance of {@link Try} is either a {@link Success} with a value of type T
 * or a {@link Failure} with a value of type {@link Throwable}.
 *
 * @param <T> Type of a successful computation.
 */
public interface Try<T> {

    boolean isSuccess();

    boolean isFailure();

    T getValue();

    Throwable getCause();

    default Try<T> peek(Consumer<? super T> action) {
        if (isSuccess()) {
            action.accept(getValue());
        }
        return this;
    }

    default <R> R fold(Function<? super T, ? extends R> onSuccess, Function<? super Throwable, ? extends R> onFailure) {
        if (isSuccess()) {
            return onSuccess.apply(getValue());
        } else {
            return onFailure.apply(getCause());
        }
    }

    default <R> Try<R> map(Function<? super T, ? extends R> mapper) {
        return mapTry(mapper::apply);
    }

    default <R> Try<R> mapTry(CheckedFunction1<? super T, ? extends R> mapper) {
        if (isSuccess()) {
            try {
                return success(mapper.apply(getValue()));
            } catch (Throwable cause) {
                return failure(cause);
            }
        } else {
            return (Failure<R>) this;
        }
    }

    default <R> Try<R> flatMap(Function<? super T, Try<R>> mapper) {
        return flatMapTry(mapper::apply);
    }

    default <R> Try<R> flatMapTry(CheckedFunction1<? super T, Try<R>> mapper) {
        if (isSuccess()) {
            try {
                return mapper.apply(getValue());
            } catch (Throwable e) {
                return failure(e);
            }
        } else {
            return (Failure<R>) this;
        }
    }

    default Try<T> recover(Function<? super Throwable, ? extends T> handler) {
        if (isSuccess()) {
            return this;
        } else {
            return success(handler.apply(getCause()));
        }
    }

    default Try<T> recoverTry(Function<? super Throwable, ? extends Try<? extends T>> handler) {
        if (isSuccess()) {
            return this;
        } else {
            return (Try<T>) handler.apply(getCause());
        }
    }

    static <T> Try<T> of(CheckedFunction0<T> value) {
        try {
            return success(value.apply());
        } catch (Throwable t) {
            return failure(t);
        }
    }

    static <T> Try<T> success(T value) {
        return new Success<>(value);
    }

    static <T> Try<T> failure(Throwable cause) {
        return new Failure<>(cause);
    }

    static <T1, T2, R> Try<R> map2(Try<T1> t1, Try<T2> t2, BiFunction<? super T1, ? super T2, R> mapper) {
        return t1.flatMap(v1 -> t2.map(v2 -> mapper.apply(v1, v2)));
    }

    static <T1, T2, T3, R> Try<R> map3(Try<T1> t1, Try<T2> t2, Try<T3> t3, Function3<? super T1, ? super T2, ? super T3, R> mapper) {
        return t1.flatMap(v1 -> t2.flatMap(v2 -> t3.map(v3 -> mapper.apply(v1, v2, v3))));
    }

    static <T1, T2, T3, T4, R> Try<R> map4(Try<T1> t1, Try<T2> t2, Try<T3> t3, Try<T4> t4, Function4<? super T1, ? super T2, ? super T3, ? super T4, R> mapper) {
        return t1.flatMap(v1 -> t2.flatMap(v2 -> t3.flatMap(v3 -> t4.map(v4 -> mapper.apply(v1, v2, v3, v4)))));
    }

    static <T> Try<T> fromOptional(Optional<T> opt) {
        return opt
            .map(Try::success)
            .orElseGet(() -> failure(new IllegalStateException("No value")));
    }

    static <T> Optional<T> toOptional(Try<T> tryOpt) {
        if (tryOpt.isSuccess()) {
            return Optional.of(tryOpt.getValue());
        } else {
            return Optional.empty();
        }
    }

    /**
     *
     * @param <T>
     */
    final class Success<T> implements Try<T> {
        private T value;

        private Success(T value) {
            this.value = value;
        }

        @Override
        public boolean isSuccess() {
            return true;
        }

        @Override
        public boolean isFailure() {
            return false;
        }

        @Override
        public T getValue() {
            return this.value;
        }

        @Override
        public Throwable getCause() {
            throw new UnsupportedOperationException("Cannot get cause from a Success");
        }
    }

    /**
     *
     * @param <T>
     */
    final class Failure<T> implements Try<T> {
        private Throwable cause;

        private Failure(Throwable cause) {
            this.cause = cause;
        }

        @Override
        public boolean isSuccess() {
            return false;
        }

        @Override
        public boolean isFailure() {
            return true;
        }

        @Override
        public T getValue() {
            throw new UnsupportedOperationException("Cannot get value from a Failure");
        }

        @Override
        public Throwable getCause() {
            return this.cause;
        }
    }
}
