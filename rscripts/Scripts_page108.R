
# Matris ve Diziler - Matris Elemanlarını Seçme #

#SORU1
x <- seq(from = 3, to = 20, length.out = 16)

mat1 <- matrix(x, nrow = 4,ncol = 4)

mat1[2,2:3]

#SORU2
y <- seq(from = 2 , by = 2, length.out = 9)

mat2 <- matrix(y, nrow=3, ncol=3)

mat2[-2,]

#SORU3
mat3 <- matrix(1:12, nrow = 3, ncol = 4)

mat3[,4]>10
