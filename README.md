# The Zaklang Programming Language

Fully functional programming language, and its interpreter, with a `Scala`-like syntax and the Hindley-Milner type inference.

## Language features

### Labmda
```scala
val adder = fun (a, b) => a + b
adder(24, 42) == 66
```

### Currying
```scala
def add(x, y)(z) = x + y + z

val addThree = add(1, 2)
val addSeven = add(3, 4)
addThree(5) + addSeven(6) == 21
```

### Higher order functions
```scala
def higherOrderAdd(adder)(a, b) = adder(a, b)

higherOrderAdd(fun (a, b) => a + b + 1)(24, 42) == 67
```

### Parametric ADT...
```scala
type List[T] {
	Nil(),
	Cons(T, List[T])
}
```

### ...with nested pattern matching
```scala
def second(l) = match l {
	case Cons(x, Cons(y, z)) => y
}

val list = Cons(10, Cons(20, Cons(30, Nil())))
second(list) == 20
```

### Type inference

```scala
def S(x)(y)(z) = x(z)(y(z))  // S :: Forall a, b, c. (a -> b -> c) -> (a -> b) -> a -> c 

def id(x) = x  // id :: Forall a. a -> a
val g = id(true)  // g :: Boolean
id(3) == 3  // id(3) :: Int
```

### Type checker
```scala
val ifte = if true then 42 else false  // Type error!

val list = Cons(1, Cons(true, Nil()))  // Type error!

def startsWithTrue(l) = match l {  // Type error!
	case Cons(1, tail) => true
	case Cons(false, tail) => false
}

def omega(x) = x(x)  // Type error!
```

## Getting Started

To build the interpreter invoke:
```
make all
```
It will build the `zaklang` binary in the `bin` directory. 

The [Dockerfile](https://github.com/McCreaP/zaklang/blob/master/Dockerfile) specifies dependencies required to build the interpreter. If your environment doesn't meet those requirements, you can use provided docker development environment available through:
```
make dev
```

After building the interpreter, you can interpret your program with:
```
bin/zaklang [FILE]
```
If a `FILE` is not specified, stdin will be parsed. Source code must define a `main` function with a signature `() -> Int`. The output of this function will be printed to stdout.

To run the dijkstra example call:
```
bin/zaklang example/dijkstra.zak
```
It should print `20` to the console.

## Tests

To run tests invoke:
```
make test
```
