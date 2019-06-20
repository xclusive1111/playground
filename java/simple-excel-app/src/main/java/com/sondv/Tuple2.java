package com.sondv;

public class Tuple2<K, V> {
    private final K _1;
    private final V _2;

    public Tuple2(K _1, V _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public K _1() {
        return _1;
    }

    public V _2() {
        return _2;
    }
}
