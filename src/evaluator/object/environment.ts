import { Maybe } from "utils";
import { BaseObject } from "./base";

type EnvStoreType = Map<string, BaseObject>;

export class Environment {
  protected envStore: EnvStoreType;
  constructor() {
    this.envStore = new Map<string, BaseObject>();
  }

  public getStore(keyName: string): Maybe<BaseObject> {
    return this.envStore.get(keyName) ?? null;
  }

  public setStore(keyName: string, obj: BaseObject): void {
    this.envStore.set(keyName, obj);
  }
}

export class EnclosedEnvironment extends Environment {
  private outer: Environment;
  constructor(outer: Environment) {
    super();
    this.outer = outer;
  }

  public getStore(keyName: string): Maybe<BaseObject> {
    const encloseEnv = this.envStore.get(keyName);
    if (encloseEnv === undefined) {
      return this.outer.getStore(keyName);
    }
    return encloseEnv;
  }

  public setStore(keyName: string, obj: BaseObject): void {
    this.envStore.set(keyName, obj);
  }
}
