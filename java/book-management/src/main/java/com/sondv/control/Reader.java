package com.sondv.control;

import java.util.function.Function;

public class Reader<DEP, A> {
    private final Function<DEP, A> run;

    public Reader(Function<DEP, A> run) {
        this.run = run;
    }

    public A run(DEP dep) {
        return this.run.apply(dep);
    }

    public <B> Reader<DEP, B> map(Function<A, B> mapper) {
        Function<DEP, B> f = dep -> mapper.apply(this.run.apply(dep));
        return new Reader<>(f);
    }

    public <B> Reader<DEP, B> flatMap(Function<A, Reader<DEP, B>> mapper) {
        Function<DEP, B> f = dep -> mapper.apply(this.run.apply(dep)).run.apply(dep);
        return new Reader<>(f);
    }
}
