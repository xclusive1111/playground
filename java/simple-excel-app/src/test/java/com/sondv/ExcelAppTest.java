package com.sondv;

import org.junit.Test;

import java.util.*;

import static com.sondv.ExcelApp.*;
import static org.junit.Assert.*;

public class ExcelAppTest {

    @Test
    public void testEvalCells() {
        {
            Map<String, String> map = new HashMap<String, String>() {{
                put("A1", "A2 2 +");
                put("A2", "A3 2 *");
                put("A3", "5");
                put("A4", "A1 A2 +");
            }};
            String message = "OK";

            try {
                Set<String> evaluatingCells = new HashSet<>();
                map.forEach((cellName, cellExpression) -> evalCells(cellName, cellExpression, map, evaluatingCells));
            } catch (Exception e) {
                message = e.getMessage();
            }
            assertEquals("OK", message);
            printResult(map);
        }

        {
            Map<String, String> map = new HashMap<String, String>() {{
                put("A1", "5");
                put("A2", "A1 5 * B1 +");
                put("B1", "6");
            }};
            String message = "OK";

            try {
                Set<String> evaluatingCells = new HashSet<>();
                map.forEach((cellName, cellExpression) -> evalCells(cellName, cellExpression, map, evaluatingCells));
            } catch (Exception e) {
                message = e.getMessage();
            }
            assertEquals("OK", message);
            printResult(map);
        }

        {
            Map<String, String> map = new HashMap<String, String>() {{
                put("A1", "A2 2 *");
                put("A2", "A1 5 +");
            }};
            String message = "OK";

            try {
                Set<String> evaluatingCells = new HashSet<>();
                map.forEach((cellName, cellExpression) -> evalCells(cellName, cellExpression, map, evaluatingCells));
            } catch (Exception e) {
                message = e.getMessage();
            }
            final String expectedMessage = "Circular dependency between A2 and A1 detected";
            assertEquals(expectedMessage, message);
        }

        {
            Map<String, String> map = new HashMap<String, String>() {{
                put("A1", "A2 2 *");
                put("A2", "a3 10 + b1 -");
                put("A3", "-10 5 *");
            }};
            String message = "OK";

            try {
                Set<String> evaluatingCells = new HashSet<>();
                map.forEach((cellName, cellExpression) -> evalCells(cellName, cellExpression, map, evaluatingCells));
            } catch (Exception e) {
                message = e.getMessage();
            }
            final String expectedMessage = "Failed to evaluating cell 'A2'. The following cells does not exist: [a3, b1]";
            assertEquals(expectedMessage, message);
        }

        {
            Map<String, String> map = new HashMap<String, String>() {{
                put("A1", "-A2 3 +");
                put("A2", "-5 A3 -");
                put("A3", "1 2 + 3 *");
            }};
            String message = "OK";

            try {
                Set<String> evaluatingCells = new HashSet<>();
                map.forEach((cellName, cellExpression) -> evalCells(cellName, cellExpression, map, evaluatingCells));
            } catch (Exception e) {
                message = e.getMessage();
                System.out.println(message);
            }
            assertEquals("OK", message);
        }
    }

    @Test
    public void testIsNumber() {
        String s = "-A2".replaceAll("[+\\-*/]", "");
        System.out.println(s);
    }

    @Test
    public void testIsFormula() {
        assertTrue(isValidFormula("A1"));
        assertTrue(isValidFormula("D190"));
        assertTrue(isValidFormula("15.")); // Equivalent to 15.0
        assertTrue(isValidFormula("5.5"));
        assertTrue(isValidFormula("10.0123"));
        assertTrue(isValidFormula("5 6.1 -"));
        assertTrue(isValidFormula("A1 6 +"));
        assertTrue(isValidFormula("-A1 A2 +"));
        assertTrue(isValidFormula("-A1 A2 + -5.1 *"));
        assertTrue(isValidFormula("-B2 -4 + -1 /"));
        assertTrue(isValidFormula("-B2 -4.9 + -A9 / C10 - D2112 *"));

        assertFalse(isValidFormula("1.1."));
        assertFalse(isValidFormula("15 6.0"));      // Missing operator
        assertFalse(isValidFormula("0 + A1"));
        assertFalse(isValidFormula("A9 6 + +"));    // Redundant '+' operator
        assertFalse(isValidFormula("9 6 + A3 ^"));  // Invalid operator
        assertFalse(isValidFormula("C 6 +"));       // Invalid cell identifier
        assertFalse(isValidFormula("+D1 -D2 *"));
        assertFalse(isValidFormula("D"));
        assertFalse(isValidFormula("120D"));
    }

    @Test
    public void testIsValidCellName() {
        assertTrue(isValidCellName("A1"));
        assertTrue(isValidCellName("A1123"));
        assertTrue(isValidCellName("B9"));

        assertFalse(isValidCellName("9B"));
        assertFalse(isValidCellName("BB9"));
        assertFalse(isValidCellName("B9B"));
        assertFalse(isValidCellName("B 9"));
        assertFalse(isValidCellName(" B9 "));
        assertFalse(isValidCellName("A")); // Missing column number

    }

    @Test
    public void testIsEvaluatedPosix() {
        assertTrue(isPosixNotation("10 5 + 7 /"));
        assertTrue(isPosixNotation("1 2.0 +"));
        assertTrue(isPosixNotation("-11.1 2 +"));
        assertTrue(isPosixNotation("9 -2 + 3 * -4 / -10 -"));

        assertFalse(isPosixNotation("A1 5 +"));
        assertFalse(isPosixNotation("+ +"));
        assertFalse(isPosixNotation("+1 +2 +"));
        assertFalse(isPosixNotation("1 + 2.5"));
        assertFalse(isPosixNotation("1 - 2"));
        assertFalse(isPosixNotation("1 -2 + -"));
        assertFalse(isPosixNotation(""));
        assertFalse(isPosixNotation("11.1. 5 -"));
        assertFalse(isPosixNotation("10 5"));     // Missing operation
        assertFalse(isPosixNotation("10 5 * 1")); // Missing operation
    }

    @Test
    public void testEvalPosix() {
        assertEquals("3.0", evalPosix("1 2 +"));
        assertEquals("0.0", evalPosix("1 2 + 3 -"));
        assertEquals("2.0", evalPosix("1 2 + 3 - 4 + 2 /"));
        assertEquals("2.0", evalPosix("1 2 + 2 - 4 * 2 /"));
    }

    @Test
    public void testCalculate() {
        double calculate = calculate("+", 1, 2);
        assertEquals(3, calculate, 0.0);
    }

    @Test
    public void testPrepend() {
        List<Integer> integers = Arrays.asList(1, 2, 3);
        List<Integer> newInts = prepend(0, integers);
        assertEquals(4, newInts.size());
        assertEquals(new Integer(0), newInts.get(0));
    }

    @Test
    public void testSplit2() {
        List<Integer> ints = Arrays.asList(1, 2, 3, 4, 5);
        Tuple2<List<Integer>, List<Integer>> tuple2 = split2(ints, 2);
        assertEquals(2, tuple2._1().size());
        assertEquals(3, tuple2._2().size());
    }

    @Test
    public void testReplaceExpression() {
        String exp = "A1 -A2 - 4 + -A3 /";
        Map<String, String> vals = new HashMap<String, String>() {{
            put("A1", "5");
            put("A2", "-3");
            put("A3", "6");
        }};
        String newExp = replaceExpression(vals, exp);
        assertEquals("5 3 - 4 + -6 /", newExp);
    }
}