--finalgrade:: [a] -> [a] -> Int

finalgrade g w
mult<-zipwith (*) g->w->[Int]
sam <- sum mult
bot <- sum w
= sam/bot

sum a
|length a == 0 = 0 
|length a == 1 = head a 
|otherwise = sum (head a) + sum (tail a)
