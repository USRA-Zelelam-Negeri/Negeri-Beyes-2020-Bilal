source("Paper3Functions.R")

library(faraway)
mu.nr <- list(logit(c(0.5,0.6)),logit(c(0.5,0.6)),logit(c(0.5,0.6)),logit(c(0.5,0.6)))
mu.or <- list(logit(c(0.9,0.95)),logit(c(0.9,0.6)),logit(c(0.5,0.95)),logit(c(0.5,0.6)))
Sigma.nr <- list(matrix(c(0.5,-0.2,-0.2,0.6),2,2), 
                 matrix(c(0.5,-0.2,-0.2,0.6),2,2),
                 matrix(c(0.5,-0.2,-0.2,0.6),2,2),
                 matrix(c(0.5,-0.2,-0.2,0.6),2,2))
Sigma.or <- list(matrix(c(1.5,-0.2,-0.2,1.6),2,2),
                 matrix(c(1.5,-0.2,-0.2,0.6),2,2),
                 matrix(c(0.5,-0.2,-0.2,1.6),2,2),
                 matrix(c(0.5,-0.2,-0.2,0.6),2,2))
n <- c(100,200)
k <- c(10,20,30,40)

Data.nr = Data.or = outl <- list()
Mat.Scen <- matrix(0, nrow = 0, ncol = 14)

library(OpenMx)

for(a in 1:4){
  #for(b in 1:1){
    for(c in 1:4){
      Mat.Scen <- rbind(Mat.Scen, c(mu.nr[[a]], mu.or[[a]], vech(Sigma.nr[[a]]), vech(Sigma.or[[a]]), n, k[c]))
    }
  #}
}

colnames(Mat.Scen) <- c("mu1.nr", "mu2.nr", "mu1.or", "mu2.or", "sigma11.nr", "sigma12.nr", "sigma22.nr",
                        "sigma11.or", "sigma12.or", "sigma22.or", "n1", "n2", "k", "nout")
dim(Mat.Scen)
head(Mat.Scen)

for(i in 1:16){
  if(Mat.Scen[i,13]==10){
    Mat.Scen[i,14] <- 2
  }else if(Mat.Scen[i,13]==20){
    Mat.Scen[i,14] <- 3
  }else if(Mat.Scen[i,13]==30){
    Mat.Scen[i,14] <- 4
  }else{
    Mat.Scen[i,14] <- 5
  }
}

seed <- c(60, 19112, 19103, 63, 54, 18256, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76)
for (i in 1:nrow(Mat.Scen)) {
  Data.nr[[i]] <- Sim_DTAData(mu = Mat.Scen[i,1:2], Sigma = vech2full(Mat.Scen[i,5:7]), n1 = rep(Mat.Scen[i,11], Mat.Scen[i,13]), n2 = rep(Mat.Scen[i,12], Mat.Scen[i,13]), k = Mat.Scen[i,13], seed = seed[i])
  Data.or[[i]] <- Sim_DTAData(mu = Mat.Scen[i,3:4], Sigma = vech2full(Mat.Scen[i,8:10]), n1 = rep(Mat.Scen[i,11], Mat.Scen[i,13]), n2 = rep(Mat.Scen[i,12], Mat.Scen[i,13]), k = Mat.Scen[i,13], seed = seed[i]+10)
  if(i==1|i==2|i==3|i==4){
    outl[[i]] <- order(Data.nr[[i]]$FN+Data.nr[[i]]$FP, decreasing = TRUE)[1:Mat.Scen[i,14]]
    Data.nr[[i]][order(Data.nr[[i]]$FN+Data.nr[[i]]$FP, decreasing = TRUE)[1:Mat.Scen[i,14]],] <- 
    Data.or[[i]][order(Data.or[[i]]$FN+Data.or[[i]]$FP, decreasing = FALSE)[1:Mat.Scen[i,14]],]
  }
  if(i==5|i==6|i==7|i==8){
    outl[[i]] <- order(Data.nr[[i]]$FN, decreasing = TRUE)[1:Mat.Scen[i,14]]
    Data.nr[[i]][order(Data.nr[[i]]$FN, decreasing = TRUE)[1:Mat.Scen[i,14]],] <- 
    Data.or[[i]][order(Data.or[[i]]$FN, decreasing = FALSE)[1:Mat.Scen[i,14]],] 
  }
  if(i==9|i==10|i==11|i==12){
    outl[[i]] <- order(Data.nr[[i]]$FP, decreasing = TRUE)[1:Mat.Scen[i,14]]
    Data.nr[[i]][order(Data.nr[[i]]$FP, decreasing = TRUE)[1:Mat.Scen[i,14]],] <- 
    Data.or[[i]][order(Data.or[[i]]$FP, decreasing = FALSE)[1:Mat.Scen[i,14]],] 
  }
  outl[13:16] <- 0
}

length(Data.nr); length(outl)

# To ensure that no more than intended outliers are randomly included in the data, we do the following check
Se.s6 = Sp.s7 = Se.l6 = Sp.l7 <- list(); mat.len.out <- matrix(0, nrow=16, ncol=4)
for(i in 1:16){
  Se.s6[[i]] <- which(ilogit(comp.y(Data.nr[[i]]))[,1] < 0.9)
  Se.l6[[i]] <- which(ilogit(comp.y(Data.nr[[i]]))[,1] >= 0.9)
  Sp.s7[[i]] <- which(ilogit(comp.y(Data.nr[[i]]))[,2] < 0.95)
  Sp.l7[[i]] <- which(ilogit(comp.y(Data.nr[[i]]))[,2] >= 0.95)
  mat.len.out[i,] <- c(length(Se.s6[[i]]), length(Se.l6[[i]]), length(Sp.s7[[i]]), length(Sp.l7[[i]]))
  
}

mat.len.out
