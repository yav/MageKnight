

type T = { X: number, Y: boolean }

function f(x): keyof T {
  for (const i in x) { return i as keyof T }
}