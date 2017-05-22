# lasso
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
library(glmnet)
setwd("C:/Users/WATS5I/prjct_PCA")
pca_50_A <- fread("pca_50_A.raw")
pca_50_A <- pca_50_A[ , -c(1:6)]
pop_names <- read.csv("pop_names", sep="")
# recode populations 1,2,3

# ifelse(pop_names$pop == "CEU", 1,
      # ifelse(pop_names$pop == "YRI", 2, 3))
pca_50_A <- as.matrix(pca_50_A)

fit = glmnet(pca_50_A, pop_names$pop, family = "multinomial", type.multinomial = "grouped")

plot(fit, xvar = "lambda", label = TRUE, type.coef = "2norm")

cvfit=cv.glmnet(pca_50_A, pop_names$pop, family="multinomial", type.multinomial = "grouped", parallel = TRUE)
plot(cvfit)
predict(cvfit, newx = pca_50_A[1:10,], s = "lambda.min", type = "class")
coef(cvfit,s="lambda.min")
