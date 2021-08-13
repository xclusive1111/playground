class ListNode {
    val: any = undefined;
    next: ListNode | null = null;

    constructor(val: any, next: ListNode | null) {
        this.val = val;
        this.next = next;
    }

    toString(): string {
        const next = this.next ? this.next.val : 'null';
        return `Val: ${this.val}, Next: ${next}`;
    }
}

const solution = (n: ListNode): ListNode => {
    const re = (head: ListNode | null): ListNode | null => {
        // reache the end
        if (head == null || head.next == null) return head;

        /* reverse the rest list and put
        the first element at the end */
        let tail: ListNode | null = re(head.next);
        head.next.next = head;

        /* tricky step -- see the diagram */
        head.next = null;

        /* fix the head pointer */
        return tail;
    }

    return re(n)!!;
}

let l: ListNode | null = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))));

const printNode = (n: ListNode) => {
    let tmp: ListNode | null = n;

    while (tmp != null) {
        console.log(tmp.val);
        tmp = tmp.next;
    }
}

printNode(l);

let a = solution(l!!);
printNode(a);
