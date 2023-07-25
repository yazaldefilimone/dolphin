import { it, expect, describe } from "vitest";
import { makeSut } from "./sut";
import { Program } from "ast";
import { ErrorHandler } from "parser";
describe("Parser", () => {
  describe("Parse program", () => {
    it("error handler", () => {
      const code = `
    let x 5;
    let = 10;
    let 838383;
    `;
      const { program, parser, errors } = makeSut(code);
      expect(program).instanceOf(Program);
      expect(parser.errorHandler).instanceOf(ErrorHandler);
      expect(errors.length).toBe(4);
    });
  });
});
