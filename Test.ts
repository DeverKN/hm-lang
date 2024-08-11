const Unit = Symbol("Unit");
type Unit = typeof Unit

type Closure <Env, Res, T, V> = {
  env: Env,
  f: (env: Env, t: T) => V,
  drop: (env: Env) => Res 
}

type ErasedClosure <T, V> = {
  f: (t: T) => V
  drop: () => Unit
}

const erase = <Env, T, V>(c: Closure<Env, Unit, T, V>): ErasedClosure<T, V> => {
  const {env, f, drop} = c
  return { f: (x) => f(env, x), drop: () => drop(env) }
}