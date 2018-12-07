# ScalaPluginInternTestAssignment
Abhinav Patil
Submission for the test assignment for summer 2019 Scala plugin internship

## Warmup

The code for the warmup exercise is located in `ScalaPluginInternTestAssignment/src/main/scala/warmup/Warmup.scala`.

The function I wrote has big-O complexity of O(x), as at each call of the function, x is only decremented by one, thus for all non-negative integers x, the function is guaranteed to run x+1 times, which corresponds to an asymptotic time complexity of O(x).

If we want to mantain some level of structural fidelity to the form of the given recurrence relation, we cannot improve the asymptotic runtime of this function. However, we can produce an equivalent function by noticing that the recurrence relation is equivalent to the closed-form expression f(x)=2^x for non-negative x. Several integer exponentiation algorithms can do much better than O(x). For example, exponentiation by squaring would have O(log(x)) runtime complexity.

We can do even better. Calculating powers of two is easy with bitshift operations: `1 << x`, where x is our exponent as before. This is effectively a O(1) operation.
