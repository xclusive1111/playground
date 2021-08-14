const solutions = (nums: number[]): number => {
    const remain = new Set<number>();
    for (let i = 0; i <= nums.length; i++) {
        remain.add(i);
    }

    for (let n of nums) {
        remain.delete(n);
    }

    for (let n of remain) {
        return n;
    }

    return -1;
}

const arr = [9,6,4,2,3,5,7,0,1];
let missing = solutions(arr);
console.log(`missing: ${missing}`)
