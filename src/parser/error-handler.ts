export class ErrorHandler {
  errors: Error[] = [];
  push(message: string) {
    const error = new Error(message);
    this.errors.push(error);
  }
  getErrors() {
    return this.errors;
  }
  getErrorsInString(): string[] {
    return this.errors.map((e) => e.message);
  }
}
