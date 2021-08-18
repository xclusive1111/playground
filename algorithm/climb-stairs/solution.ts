const climbStairs = (n: number): number => {
    const climb = (_n: number, a: number, b: number): number => {
        if (_n == 1) return a;
        else return climb(_n - 1, a + b, a);
    }
    return climb(n, 1, 1)
}

cnt = climbStairs(6);
console.log(cnt)
