import { Maybe } from "utils";
import { BaseObject } from "./base";

type EnvStoreType = Map<string, BaseObject>;

export class Environment {
  private envStore: EnvStoreType;
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
