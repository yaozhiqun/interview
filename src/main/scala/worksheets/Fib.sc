def fib(x: Int): Int = {
  if (x < 2)
    x
  else
    fib(x - 1) + fib(x - 2)
}

fib(3)

