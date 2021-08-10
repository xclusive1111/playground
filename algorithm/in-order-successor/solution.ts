// XNode represents a node in a binary tree
class XNode {
    data: number;
    left: XNode | null;
    right: XNode | null;
    parent: XNode | null;

    constructor(data: number) {
        this.data = data;
        this.left = this.right = this.parent = null;
    }

    public toString(): string {
        const left = this.left ? this.left.data : 'null';
        const right = this.right ? this.right.data : 'null';
        const parent = this.parent ? this.parent.data : 'null';

        return `data: ${this.data}, left: ${left}, right: ${right}, parent: ${parent}`;
    }
}

// Insert data into the given node, keep the orginal node unchanged
const insert = (node: XNode | null, data: number): XNode => {
    /* 1. If the tree is empty, return a new, single node */
    if (node == null) return new XNode(data);

    let temp;

    /* 2. Otherwise, recur down the tree */
    if (data <= node.data) {
        temp = insert(node.left, data);
        node.left = temp;
        temp.parent = node;
    } else {
        temp = insert(node.right, data);
        node.right = temp;
        temp.parent = node;
    }

    /* return the (unchanged) node pointer */
    return node;
}

let root: XNode | null = null;

// Create sample tree
root = insert(root, 20);
root = insert(root, 8);
root = insert(root, 22);
root = insert(root, 4);
root = insert(root, 12);
root = insert(root, 10);
root = insert(root, 14);

const inOrderSuccessor = (bst: XNode, n: XNode): XNode | null => {
    if (n === null) return null;

    const dir = upOrDown(n);
    switch (dir) {
        case 'up':
            return searchUp(n);
        case 'down':
            return searchDown(n);
    }
}

const upOrDown = (n: XNode): 'up' | 'down' => {
    if (n.right) {
        return 'down';
    }

    return 'up'
}

const searchUp = (n: XNode | null): XNode | null => {
    if (n === null) return null;

    if (n.parent && isLeftSide(n, n.parent)) {
        return n.parent;
    }

    return searchUp(n.parent);
}

// Returns true if n is on the left side of its parent p
const isLeftSide = (n: XNode, p: XNode): boolean => {
    if (p.left && p.left!!.data == n.data) return true;
    return false;
}

const searchDown = (n: XNode): XNode | null => {
    if (n == null) return null;

    if (n.right && n.right.left) {
        return n.right.left;
    }
    return n.right;
}

let temp = root!!.left!!.right!!.left;

let succ = inOrderSuccessor(root, temp!!)

const data = succ ? succ.data : 'null';
console.log(`InOrderSuccessor of ${temp!!.data} is ${data}`)
