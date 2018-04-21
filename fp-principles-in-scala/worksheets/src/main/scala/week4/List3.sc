// immutable
// recursive
// arrays are flat in opposite
// Nil
// List[Nothing]

// :: - cons
// x :: xs
// a :: b :: c :: Nil
// associate to the right
// 1 :: 2 :: 3 :: 4 :: Nil is same as
// 1 :: (2 :: (3 :: (4 :: Nil)))
// so parenthesis are redundant

1 :: (2 :: (3 :: (4 :: Nil)))

1 :: 2 :: 3 :: 4 :: Nil

Nil.::(4).::(3).::(2).::(1)

// operators ending with a colon are right handed
// meaning that the right side is receiver
// so 1 :: Nil is same as Nil.::(1) - Nil is receiver

def insort(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case x :: xs => insert(x, insort(xs))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => List(x)
  case y :: ys =>
    if (x < y) x :: xs
    else y :: insert(x, ys)
}

// O(n^2)
insort(List(3,6,7,2,9))
4