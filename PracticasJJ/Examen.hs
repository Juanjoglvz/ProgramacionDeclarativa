sum'::(Num a) => [a] -> a
sum' s = foldl(\acc t -> t + acc) 0 s