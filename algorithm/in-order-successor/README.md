## Find the inorder successor of a node
![Binary Search Tree](https://media.geeksforgeeks.org/wp-content/cdn-uploads/2009/09/BST_LCA.gif)

In a Binary Search Tree (BST), an Inorder Successor of a node is the node with its value greater than the input node, but also has the smallest value among all successors.

For example, given the BST above:

+ The inorder successor of 8 is 10
+ The inorder successor of 12 is 14
+ The inorder successor of 14 is 20

Write a function that returns the inorder successor of a given node, or null if there's no such node


```
findInOrderSuccessor(root, node)
```

where `root` is a root BST and `node` is the input node

