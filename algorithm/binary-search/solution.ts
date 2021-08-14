const solutions = (nums: number[], target: number): number => {
    let left = 0;
    let right = nums.length - 1;

    while (left <= right) {
        const mid = Math.floor((left + right) / 2);
        console.log(`Left: ${left}, Right: ${right}, Mid: ${mid}`);
        if (nums[mid] == target) return mid;

        if (nums[mid] < target) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }

    return -1;
}

const arr = [1, 2, 3, 4, 5, 5, 7, 8, 9, 10, 11];
let idx = solutions(arr, 7);
console.log(`idx: ${idx}, value: ${arr[idx]}`);
idx = solutions(arr, 11);
console.log(`idx: ${idx}, value: ${arr[idx]}`)
