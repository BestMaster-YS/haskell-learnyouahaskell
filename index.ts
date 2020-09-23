type f<T, R> = (t: T) => R

// add :: x -> y -> z
const add: f<number, f<number, number>> = (x: number) => {
  return function (y: number) {
    return x + y;
  }
}

// ap :: f (a -> b) -> f a -> f b
function ap() {}