## A simple console excel application
### Requirement
* **Input** - will be given via stdin.
   * The first line will contain a number `N` indicating the amount of cells.
   * The rest `2*N` lines will have the following structure:
      * The first line contains the cell name (e.g `A1`).
      * The second line contains the cell content. The content of each cell can be a number (`double`), or a mathematic formula (consists of `+-*/`) and the formula can also refers to other cells. the formula is written in polish postfix notation.

* **Output**: the final values of each cells, sorted alphabetically by the cell names, to stdout.

Example, for the following stdin input:

```
Enter an amount of cells: 3
Enter a cell name: A1
Enter a cell formula: 5

Enter a cell name: A2
Enter a cell formula: A1 5 * B1 +

Enter a cell name: B1
Enter a cell formula: 6
```
Will give below output, sorted alphabetically:

```
A1 = 5
A2 = 31
B1 = 6
```

### Important:
* The solution will be evaluated automatically, so the output must strictly adhere to the above format.
* The application must report for any circular dependencies.
* The formula of a cell can refer to another cell and can have `-` sign to indicate a negative value, for example: `A1 -A2 + -5 -C1 /`.

Example, for the following input:

```
Enter an amount of cells: 2

Enter a cell name: A1
Enter a cell formula: A2 2 *

Enter a cell name: A2
Enter a cell formula: A1 5 +
```

Will produce below error message:

```
Circular dependency between A1 and A2 detected.
```
