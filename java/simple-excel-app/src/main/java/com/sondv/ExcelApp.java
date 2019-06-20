package com.sondv;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ExcelApp {
    private static Predicate<String> posixPred    = Pattern.compile("^-?(\\d+(\\.\\d+)?)\\s+-?(\\d+(\\.\\d+)?)\\s+[+\\-*/](\\s+(-?)(\\d+(\\.\\d+)?)\\s+[+\\-*/])*$").asPredicate();
    private static Predicate<String> formulaPred  = Pattern.compile("^(-?((\\d+(\\.\\d+)?)|([A-Za-z][0-9]+)))\\s+(-?((\\d+(\\.\\d+)?)|([A-Za-z][0-9]+)))\\s+[+\\-*/](\\s+(-?((\\d+(\\.\\d+)?)|([A-Za-z][0-9]+)))\\s+[+\\-*/])*$").asPredicate();
    private static Predicate<String> cellNamePred = Pattern.compile("^([A-Za-z][0-9]+)$").asPredicate();
    private static Scanner scanner                = new Scanner(System.in);

    public static void main(String[] args) {
        int amount = getCellAmount();
        Map<String, String> cells = getCells(amount, new HashMap<>());
        try {
            cells.forEach((cellName, cellExpression) -> evalCells(cellName, cellExpression, cells, new HashSet<>()));
        } catch (Exception e) {
            System.err.println(e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
        printResult(cells);
        System.exit(0);
    }

    /**
     * Sort cell alphabetically and print the evaluation result to stdout. */
    public static void printResult(Map<String, String> evalResult) {
        System.out.println("Evaluation result:");
        evalResult.entrySet().stream()
            .sorted(Map.Entry.comparingByKey())
            .forEach(entry  -> System.out.println(String.format("%s = %s", entry.getKey(), entry.getValue())));
    }

    public static Map<String, String> getCells(int amount, Map<String, String> cells) {
        if (amount <= 0) return cells;
        System.out.println("\nRemain " + amount + " cells");
        cells.put(getCellName(), getCellFormula());
        return getCells(amount - 1, cells);
    }

    private static String getCellFormula() {
        System.out.print("Enter a cell formula: ");
        String formula  = scanner.nextLine().trim();
        if (isValidFormula(formula)) return formula;
        else {
            System.out.println("You entered an invalid formula. Please try again.");
            return getCellFormula();
        }
    }

    private static String getCellName() {
        System.out.print("Enter a cell name: ");
        String cellName = scanner.nextLine().trim();
        if (isValidCellName(cellName)) return cellName;
        else {
            System.out.println("You entered an invalid cell name. Please try again.");
            return getCellName();
        }
    }

    private static int getCellAmount() {
        System.out.print("Enter an amount of cells: ");
        String amount = scanner.nextLine().trim();
        try {
            return Integer.parseInt(amount);
        } catch (NumberFormatException e) {
            System.out.println("You entered an invalid number. Please try again.");
            return getCellAmount();
        }
    }

    /**
     * Evaluate cells by it's formula.
     *
     * @param cellName Name of the cell, e.g: 'A1'.
     * @param formula A string represent a formula of the cell.
     * @param currentResult Current evaluation result.
     * @param evaluatingCells Cells that are evaluating to track circular dependencies.
     * @return A {@link String} represent a value of a specified cell after evaluated it's formula.
     * @throws IllegalStateException if there're circular dependencies.
     */
    public static String evalCells(String cellName, String formula, Map<String, String> currentResult, Set<String> evaluatingCells) {
        if (evaluatingCells.contains(cellName)) {
            throw new IllegalStateException(getExceptionMessage(cellName, cellName));
        }

        if (isNumber(formula)) {
            evaluatingCells.remove(cellName);
            return formula;
        }

        // Check whether a formula is normalize, i.e the name of a cell is replaced by it's evaluated value.
        // For example:
        // 'A1 5 +' is false, because 'A1' is not a value.
        // '10 5 +' is true, because 'A1' is replace by it's evaluated value.
        if (isPosixNotation(formula)) {
            String value = evalPosix(formula);
            currentResult.put(cellName, value);
            evaluatingCells.remove(cellName);
            return value;
        }

        // Split the formula and filter out names of cells.
        Set<String> cellNamesToEval = Stream.of(formula.split("\\s+"))
            .map(str -> str.replaceAll("[+\\-*/]", ""))
            .filter(cellNamePred)
            .collect(Collectors.toSet());

        if (cellNamesToEval.contains(cellName)) { // In case like A1 = "1 A1 +"
            throw new IllegalStateException(getExceptionMessage(cellName, cellName));
        }

        // Check for non-exist cells
        Optional<String> nonExistCellNames = cellNamesToEval.stream()
            .filter(name -> !currentResult.containsKey(name))
            .reduce((acc, a) -> acc + ", " + a);

        if (nonExistCellNames.isPresent()) {
            String message = String.format("Failed to evaluating cell '%s'. The following cells does not exist: [%s]",
                cellName, nonExistCellNames.get());
            throw new IllegalStateException(message);
        }

        Optional<String> depOpt = findDependencies(evaluatingCells, cellNamesToEval);
        if (depOpt.isPresent()) {
            throw new IllegalStateException(getExceptionMessage(cellName, depOpt.get()));
        }

        // Perform evaluating each cell recursively.
        Map<String, String> vals = cellNamesToEval.stream().map(cell -> {
            String expression = currentResult.get(cell);
            evaluatingCells.add(cellName);
            return new Tuple2<>(cell, evalCells(cell, expression, currentResult, evaluatingCells));
        }).collect(Collectors.toMap(Tuple2::_1, Tuple2::_2));

        // Replace the name of a cell by it's evaluated value and perform posix calculate.
        String newExp = replaceExpression(vals, formula);
        String value = evalPosix(newExp);
        currentResult.put(cellName, value);
        evaluatingCells.remove(cellName);

        return value;
    }

    /**
     * Checks if an expression is a number. */
    public static boolean isNumber(String exp) {
        try {
            new BigDecimal(exp.trim());
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * Checks if an expression is an evaluated posix formula. */
    public static boolean isPosixNotation(String exp) {
        return posixPred.test(exp);
    }

    /**
     * Checks if a string is a valid cell name. */
    public static boolean isValidCellName(String name) {
        return cellNamePred.test(name);
    }

    /**
     * Checks if a string is a valid excel formula. */
    public static boolean isValidFormula(String formula) {
        return isNumber(formula) || isValidCellName(formula) || formulaPred.test(formula);
    }

    /**
     * Attempts to evaluate a posix formula.
     * @param exp A {@link String} represent a poxix formula.
     * @return A number in the form of a string.
     */
    public static String evalPosix(String exp) {
        List<String> parts = Arrays.asList(exp.trim().split("\\s+"));
        return String.valueOf(eval(parts));
    }

    /**
     * Attempts to evaluate a posix formula.
     * @param parts A list of element in a posix formula, usually after split the formula by spaces.
     * @return An double of evaluated value.
     */
    public static double eval(List<String> parts) {
        if (parts.size() < 3) return 0;
        else {
            Tuple2<List<String>, List<String>> tuple2 = split2(parts, 3);
            List<String> part1 = tuple2._1();
            List<String> part2 = tuple2._2();
            String operation = part1.get(2);
            double value = calculate(operation, Double.parseDouble(part1.get(0)), Double.parseDouble(part1.get(1)));

            if (part2.isEmpty()) return value;
            else {
                List<String> remains = prepend(String.valueOf(value), part2);
                return eval(remains);
            }
        }
    }

    /**
     * Calculates value based on operation. */
    public static double calculate(String operation, double n1, double n2) {
        switch (operation) {
            case "+": return n1 + n2;
            case "-": return n1 - n2;
            case "*": return n1 * n2;
            case "/": return n1 / n2;
            default: throw new IllegalArgumentException("Unknown operator. Got '" + operation + "'");
        }
    }

    /**
     * Inserts an element into the head of a list. */
    public static <T> List<T> prepend(T head, List<T> tails) {
        int length = tails.size() + 1;
        ArrayList<T> out = new ArrayList<>(length);
        out.add(head);
        out.addAll(tails);
        return out;
    }

    /**
     * Splits a list into two parts by an index.
     * Elements that have index smaller than a given index will be on the left of the Tuple2.
     * Elements that have index greater or equal than a given index will be on the right of the Tuple2.
     *
     * For example:
     * <pre>{@code
     *   List<Integer> ints = Arrays.asList(1, 3, 5, 7, 9);
     *   Tuple2<List<Integer>, List<Integer>> t2 = split2(ints, 3);
     *   // t2._1() == List(1, 3, 5)
     *   // t2._2() == List(7, 9)
     * }</pre>
     *
     * @param arr The source of elements.
     * @param index An index position to split the source.
     * @param <T> Type of the element.
     * @return A {@link Tuple2}.
     */
    public static  <T> Tuple2<List<T>, List<T>> split2(List<T> arr, int index) {
        int arrLen = arr.size();
        List<T> part1 = new ArrayList<>(index);
        List<T> part2 = new ArrayList<>(arrLen - index);

        if (arrLen >= index) {
            for (int i = 0; i < arrLen; i++) {
                T t = arr.get(i);
                if (i < index) part1.add(t);
                else part2.add(t);
            }
        }

        return new Tuple2<>(part1, part2);
    }

    /**
     * Formats an exception message. */
    public static String getExceptionMessage(String cel1, String cel2) {
        return String.format("Circular dependency between %s and %s detected", cel1, cel2);
    }

    /**
     * Replaces cell's name of an original posix formula by it's evaluated values. */
    public static String replaceExpression(Map<String, String> vals, String exp) {
        final String[] s = {exp};
        vals.forEach((s1, s2) -> {
            s[0] = s[0].replaceAll(s1, s2).replaceAll("--", "");
        });
        return s[0];
    }

    /**
     * Finds circular dependencies. */
    public static Optional<String> findDependencies(Set<String> evaluating, Set<String> toBeEvaluated) {
        for (String s1 : evaluating) {
            for (String s2 : toBeEvaluated) {
                if (s1.equals(s2)) {
                    return Optional.of(s1);
                }
            }
        }
        return Optional.empty();
    }
}
