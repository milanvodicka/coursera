// translation for

// 1. simple

for (x <- 1 to 10) yield x * 2

(1 to 10).map(x => x * 2)

// 2. filter

for (x <- 1 to 10 if x > 4) yield x * 2

for (x <- (1 to 10).withFilter(x => x > 4)) yield x * 2

// 3. multiple arrows

for (x <- 1 to 3; y <- 2 to 4) yield (x * 2, y * 2)

(1 to 3).flatMap(x => for(y <- 2 to 4) yield (x * 2, y * 2))

(1 to 3).flatMap(x => (2 to 4).map(y => (x * 2, y * 2)))

// patterns on the left side acts like implicit filters