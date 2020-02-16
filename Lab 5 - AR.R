#Question 1
x <- c(6, 8, 2, 4, 4, 5)
y <- c(7, 10, 4, 3, 5, 6)

wilcox.test (x, y, correct = FALSE)

wilcox.test (y, x, correct = FALSE)



#Question 2
#Paired
wi <- c(214, 159, 169, 202, 103, 119, 200, 109, 132, 142, 194, 104, 219, 119, 234)
wo <- c(159, 135, 141, 101, 102, 168, 62, 167, 174, 159, 66, 118, 181, 171, 112)

wilcox.test (wi, wo, paired = TRUE)

#p value greater than 0.05 and therefore we fail to reject H0
#banning boats for a day does not change pollution

diff <- wi - wo
diff <- diff [diff!=0]
diff.rank <- rank(abs(diff))
diff.rank.sign <- diff.rank * sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign>0])
ranks.neg <- sum(diff.rank.sign[diff.rank.sign<0])

ranks.pos
ranks.neg


#Question 3
#Paired on same computer, same processor but different file format

zip <- c(10, 44, 65, 77, 43, 44, 22, 66, 50, 100, 55, 99, 44, 23, 100, 88, 200, 220, 110, 551)
tar <- c(20, 55, 75, 60, 55, 88, 35, 33, 35, 80, 65, 82, 47, 35, 97, 110, 250, 190, 111, 600)

plot (zip)
plot (density(zip))

plot (tar)
plot (density(tar))

wilcox.test (zip, tar, paired = TRUE)

#p value is greater than >0.05 and therefore we fail to reject the null hypothesis