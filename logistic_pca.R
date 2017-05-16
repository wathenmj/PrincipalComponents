# logistic_pca ####
library(VGAM)
library(nnet)
library(caret)
library(data.table)
setwd("C:/Users/WATS5I/prjct_PCA")
#load("pca-models") ####
#save.image("pca-models")
fourPCs_10k_snps <- read.delim("C:/Users/WATS5I/prjct_PCA/fourPCs_top1000_snps.txt")
pop_names <- read.csv("pop_names", sep="")

pca_50_A <- fread("pca_50_A.raw")
pca_50_A <- pca_50_A[ , -c(1:6)]


# models ####
pca_multi <- data.frame(pop_names$pop, pca_50_A)
names(pca_multi)[names(pca_multi) == 'pop_names.pop'] <- 'pop'
# pca_multi <- pca_multi[ ,-23] # remove columns where there is only one genotype value

for (i in 2:51) {
  pca_multi[ , i] <- as.factor(pca_multi[ , i])
}

# training set ----
set.seed(1569)
Train <- createDataPartition(pca_multi$pop, p=0.8, list=FALSE)
training <- pca_multi[ Train, ]
testing <- pca_multi[ -Train, ]
ctrl <- trainControl(method = "repeatedcv", number = 20, savePredictions = TRUE)

model_pca_50 <- train(pop ~ . ,
                      data=pca_multi, method= "nnet",
                      maxit = 300, MaxNWts = 3000,
                      family="binomial",
                      trControl = ctrl)

pred = predict(model_pca_50, newdata=testing)
confusion.pca_50 <-confusionMatrix(data=pred, testing$pop); 
print(confusion.pca_50)
pred_all_pca_50 <- predict(model_pca_50, newdata = ran_multi)


n <- c(5,10,15,20,25,30,35,40,45,50)
n_pca_ci <- c(5,10,15,20,25,30,35,40,45,50)
upper_ci <- c(0.8894,0.7638,0.8128,0.7803,0.8597,0.8747,0.8597,0.8894,0.968,0.9441)
lower_ci <- c()
est <- c()



pca_estimates <- data.frame(n, lower_ci, est, upper_pca_ci)
write.table(pca_estimates, "pca_estimates",
            quote = F,
            row.names = F,
            col.names = T)

plot(n_pca_set, acc_pca_est, type="b",
     col="red", lty=2, pch=22, lwd=2,
     main="Multinomial models - pca snp collections",
     ylim=c(0.3,1.1))

arrows(n_pca_set,acc_pca_est ,n_pca_set,upper_pca_ci,col="black",angle=90)
arrows(n_pca_set,acc_pca_est ,n_set,lower_pca_ci,col="black",angle=90)

# STRUCTURE ####
#  - run STRUCTURE on these markers
pca_5_A <- fread("pca_5_A.raw")
pca_5 <- pca_5_A[ , -c(1:6)]
pca_5 <- data.frame(pop_names, pca_5)

write.table(pca_5, "pca_5_STR",
            quote = F,
            row.names = F,
            col.names = T)
# open in Excel and modify for STRUCTURE

top500_snps <- read.delim("C:/Users/WATS5I/prjct_PCA/top500_snps.txt")

pca_snps_100 <- top500_snps[ ,1]
write.table(pca_snps_100, "pca_snps_100",
            quote = F,
            row.names = F,
            col.names = F)
