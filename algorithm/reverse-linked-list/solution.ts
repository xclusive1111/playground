class ListNode {
    val: any = undefined;
    next: ListNode | null = null;

    constructor(val: any, next: ListNode | null) {
        this.val = val;
        this.next = next;
    }
}

const solution = (l: ListNode): ListNode => {
    let stack = [];

    let tmp: ListNode | null = l;
    while (tmp != null) {
        stack.push(tmp);
        tmp = tmp.next;
    }

    return stack.reduce((acc, a) => {
        a.next = acc;
        return a;
    }, null!!);
}

let l: ListNode | null = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))));

const printNode = (n: ListNode) => {
    let tmp: ListNode | null = n;

    while (tmp != null) {
        console.log(tmp.val);
        tmp = tmp.next;
    }
}

let a = solution(l);
printNode(a);
