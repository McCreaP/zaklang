# The Zaklang Programming Language

Fully functional programming language, and its interpreter, with a `Scala`-like syntax and the Hindley-Milner type inference.

## Language features

### Labmda
```scala
def lambda() = {
	val adder = fun (a, b) => a + b
	adder(24, 42) == 66
}
```

### Currying
```scala
def addCurrying(x, y)(z) = x + y + z

def functionCallCurrying() = {
	val foo = addCurrying(1, 2)
	val bar = addCurrying(3, 4)
	foo(5) + bar(6) == 21
}
```

### Higher order functions
```scala
def higherOrderAdd(adder)(a, b) = adder(a, b)

def higherOrderTest() = higherOrderAdd(fun (a, b) => a + b + 1)(24, 42) == 67
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

def nestedMatching() = {
	val list = Cons(10, Cons(20, Cons(30, Nil())))
	second(list) == 20
}
```

### Type inference

```scala
def S(x)(y)(z) = x(z)(y(z))  // S :: Forall a, b, c. (a -> b -> c) -> (a -> b) -> a -> c 

def letGeneralization() = {
	def id(x) = x  // id :: Forall a. a -> a
	val g = id(true)  // g :: Boolean
	id(3) == 3  // id(3) :: Int
}

val ifte = if true then 42 else false  // Type error!

def main(x) = x(x)  // Type error!

val list = Cons(1, Cons(true, Nil()))  // Type error!

def main(l) = match l {  // Type error!
	case Cons(1, Nil()) => 0
	case Cons(true, Nil()) => 1
}
```
