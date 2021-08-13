const solutions = (nums: number[]): number => {
    let slow = nums[0], fast = slow;
    do {
        slow = nums[slow];
        fast = nums[nums[fast]];
    } while (slow != fast);

    slow = nums[0];
    while (slow != fast) {
        slow = nums[slow];
        fast = nums[fast];
    }
    return slow;
}

console.log(solutions([4,2,3,4,1]));
