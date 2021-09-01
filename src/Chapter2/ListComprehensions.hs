module Chapter2.ListComprehensions () where

ex1 = [x * 2 | x <- [1 .. 10]]

-- [2,4,6,8,10,12,14,16,18,20]

ex2 = [x * 2 | x <- [1 .. 10], x * 2 > 12]

-- [12,14,16,18,20]

ex3 =
  let boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
   in boomBangs [7 .. 13]

-- ["BOOM!","BOOM!","BANG!","BANG!"]

ex4 = [x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19]

-- [10,11,12,14,16,17,18,20]

ex5 = [x * y | x <- [2, 5, 10], y <- [8, 10, 11]]

-- [16,20,22,40,50,55,80,100,110]

ex6 =
  [x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y > 50]

-- [55,80,100,110]

ex7 =
  let nouns = ["hobo", "frog", "pope"]
      adjectives = ["lazy", "grouchy", "scheming"]
   in [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

-- ["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",
-- "grouchy pope","scheming hobo","scheming frog","scheming pope"

ex8 =
  let length' xs = sum [1 | _ <- xs]
   in length' [True, False, True, False]

-- 4

ex9 =
  let removeNonUppercase str = [c | c <- str, c `elem` ['A' .. 'Z']]
   in removeNonUppercase "IdontLIKEFROGS"

-- "ILIKEFROGS"

ex10 =
  let xxs = [[1, 3, 5, 2, 3, 1, 2, 4, 5], [1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 4, 2, 1, 6, 3, 1, 3, 2, 3, 6]]
   in [[x | x <- xs, even x] | xs <- xxs]

-- [[2, 2, 4], [2, 4, 6, 8], [2, 4, 2, 6, 2, 6]]