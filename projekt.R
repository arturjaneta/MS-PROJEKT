setwd("C:/Users/Ar2r/Desktop/MS-PROJEKT-master")

dane1 <- read.table("dane1.txt", quote="\"", comment.char="")
dane2 <- read.table("dane2.txt", quote="\"", comment.char="")



zadanie1(dane1$V1, dane2$V1);

zadanie2(dane1$V1);
zadanie2(dane2$V1);

zadanie3(dane1$V1,0.05);
zadanie4(dane2$V1,100,0.05);

