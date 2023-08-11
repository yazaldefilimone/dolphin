import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";

describe("Evaluator", () => {
  it("evaluate infix  expression", () => {
    const tests = [
      {
        input: "5 + 5",
        expected: 10,
      },
      {
        input: "5 - 5",
        expected: 0,
      },
      {
        input: "5 * 5",
        expected: 25,
      },
      {
        input: "5 / 5",
        expected: 1,
      },
      {
        input: "5 > 5",
        expected: false,
      },
      {
        input: "5 < 5",
        expected: false,
      },
      {
        input: "5 == 5",
        expected: true,
      },
      {
        input: "5 != 5",
        expected: false,
      },
      {
        input: "true == true",
        expected: true,
      },
    ];
    tests.forEach((test) => {
      console.log(test.input);
      const evaluated = makeSut<number>(test.input);
      expect(evaluated).not.toBeNull();
      expect(evaluated?.value).toEqual(test.expected);
    });
  });

  it("evaluate complexity infix expression", () => {
    const tests = [
      {
        input: "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10",
        expected: 55,
      },
      {
        input: "1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10",
        expected: 3628800,
      },
      {
        input: "2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2",
        expected: 1024,
      },
      {
        input: "5 * 2 + 10",
        expected: 20,
      },
      {
        input: "4 - (5 + 2) * 10",
        expected: -66,
      },
      {
        input: "-50 + (100 -50)",
        expected: 0,
      },
    ];
    tests.forEach((test) => {
      const evaluated = makeSut<number>(test.input);
      expect(evaluated).not.toBeNull();
      expect(evaluated?.value).toEqual(test.expected);
    });
  });
});
