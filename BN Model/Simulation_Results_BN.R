source("Simulation_Functions_BN.R")
source("DataGeneration_BN.R")

library(parallel)
hosts <- rep(c("localhost", "machine1", "machine2"),
             c(10,10,8))
myCluster <- makePSOCKcluster(hosts,parallel=T,outfile="", type="FORK")
clusterEvalQ(myCluster, library(mada))
clusterExport(myCluster, c("comp.y", "comp.Psi", "fit_bnn", "fit_bbn", "MCQ.mu", "vec2full",
                           "vect", "diag2vech", "MCQ.sigma", "MH.mcmc", "FIM.Oakes", "WA_Cov",
                           "MCloglik.y", "Sim_DTAData", "mcemDTA","Data.nr", "outl"))
tryCatch({
  system.time(fit.bnlm.sim <- parLapply(myCluster, 1:28, function(d){mcemDTA(Scen=d, maxit = 100)
  }))}, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
stopCluster(myCluster)
save.image("BNLM_SimResFinal_BN.RData")


# We now fit the standard methods to the 16 datasets
fit.bnn.sim = fit.bbn.sim <- list()
for(i in 1:28){
  fit.bnn.sim[[i]] <- fit_bnn(Data = Data.nr[[i]], method = "ml")
  fit.bbn.sim[[i]] <- fit_bbn(Data = Data.nr[[i]], Study = Data.nr[[i]]$Study)
}


Create.Figure(mat.scen = Mat.Scen, Outl = "Se and Sp", Sigsize="large", fit.bnn = fit.bnn.sim, fit.bbn = fit.bbn.sim, fit.bnlm = fit.bnlm.sim)
Create.Figure(mat.scen = Mat.Scen, Outl = "Se and Sp", Sigsize="small", fit.bnn = fit.bnn.sim, fit.bbn = fit.bbn.sim, fit.bnlm = fit.bnlm.sim)

Create.Figure(mat.scen = Mat.Scen, Outl = "Se", Sigsize="large", fit.bnn = fit.bnn.sim, fit.bbn = fit.bbn.sim, fit.bnlm = fit.bnlm.sim)
Create.Figure(mat.scen = Mat.Scen, Outl = "Se", Sigsize="small", fit.bnn = fit.bnn.sim, fit.bbn = fit.bbn.sim, fit.bnlm = fit.bnlm.sim)

Create.Figure(mat.scen = Mat.Scen, Outl = "Sp", Sigsize="large", fit.bnn = fit.bnn.sim, fit.bbn = fit.bbn.sim, fit.bnlm = fit.bnlm.sim)
Create.Figure(mat.scen = Mat.Scen, Outl = "Sp", Sigsize="small", fit.bnn = fit.bnn.sim, fit.bbn = fit.bbn.sim, fit.bnlm = fit.bnlm.sim)

Create.Figure(mat.scen = Mat.Scen, Outl = "None", Sigsize="None", fit.bnn = fit.bnn.sim, fit.bbn = fit.bbn.sim, fit.bnlm = fit.bnlm.sim)


# Absolute percent change in Se and Sp
## BNLM vs BNN
scen=9:12 # 1:4; 5:8; 9:12; 13:16; 17:20; 21:24; 25:28
for(i in scen){
  print(fit.bnlm.sim[[i]]$SeSp-fit.bnn.sim[[i]]$SeSp)
}
## BNLM Vs BBN
for(i in scen){
  print(fit.bnlm.sim[[i]]$SeSp-fit.bbn.sim[[i]]$SeSp)
}

# Relative percent change in Se and Sp
## BNLM Vs BNN
for(i in scen){
  print(c((fit.bnlm.sim[[i]]$SeSp[1]-fit.bnn.sim[[i]]$SeSp[1])/fit.bnlm.sim[[i]]$SeSp[1], (fit.bnlm.sim[[i]]$SeSp[2]-fit.bnn.sim[[i]]$SeSp[2])/fit.bnlm.sim[[i]]$SeSp[2]))
}
## BNLM Vs BBN
for(i in scen){
  print(c((fit.bnlm.sim[[i]]$SeSp[1]-fit.bbn.sim[[i]]$SeSp[1])/fit.bnlm.sim[[i]]$SeSp[1],(fit.bnlm.sim[[i]]$SeSp[2]-fit.bbn.sim[[i]]$SeSp[2])/fit.bbn.sim[[i]]$SeSp[2]))
}



# Confidence intervals for Se and Sp (for discussion)
CI.Se.bnlm.bn=CI.Sp.bnlm.bn=CI.Se.bnn.bn=CI.Sp.bnn.bn=CI.Se.bbn.bn=CI.Sp.bbn.bn <- rep(0,28)

for(i in 1:28){
  CI.Se.bnlm.bn[i] <- fit.bnlm.sim[[i]]$CIs[1]
  CI.Sp.bnlm.bn[i] <- fit.bnlm.sim[[i]]$CIs[2]
  CI.Se.bnn.bn[i] <- fit.bnn.sim[[i]]$CIs[1]
  CI.Sp.bnn.bn[i] <- fit.bnn.sim[[i]]$CIs[2]
  CI.Se.bbn.bn[i] <- fit.bbn.sim[[i]]$CIs[1]
  CI.Sp.bbn.bn[i] <- fit.bbn.sim[[i]]$CIs[2]
}

i=25 # 1, 5, 9, 13, 17, 21, 25
plot(Mat.Scen[(i:(i+3)),13], CI.Se.bnlm.bn[(i:(i+3))], col="blue", type = "l", ylim=c(0,1))
lines(Mat.Scen[(i:(i+3)),13], CI.Se.bnn.bn[(i:(i+3))], col="red", type="l")
lines(Mat.Scen[(i:(i+3)),13], CI.Se.bbn.bn[(i:(i+3))], col="black", type="l")

