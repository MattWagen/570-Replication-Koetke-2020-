#this one

#install.packages(
  "rjags",
 type = "source",
 repos = "https://cloud.r-project.org",
 configure.args = c(
    "--with-jags-include=/usr/local/include",
    "--with-jags-lib=/usr/local/lib"
  )
)

####Chat GPT fix to no package called jags
#-------------------------------------------------#
#Sys.setenv(PKG_CONFIG_PATH = paste(
  "/usr/local/opt/jags/lib/pkgconfig",
  "/usr/local/lib/pkgconfig",
  Sys.getenv("PKG_CONFIG_PATH"),
  sep=":"
))

#install.packages("rjags", type = "source", repos = "https://cloud.r-project.org")
#------------------------------------------------#
#####

library(rjags)
library(R2jags)
library(jagsUI)
library(R2WinBUGS)

hacount=c(35,48,54,69,89,93,89,95,115,140,211,268,303,399,500,590,684) #cows, calves, spikes, harvest added
hacount=c(35,48,54,69,89,93,89,95,115,140,211,268,303,399,500,590,684,443) #cows, calves, spikes, harvest added, including 2000 - final
bhcount=c(4,NA,NA,NA,NA,NA,NA,NA,NA,60,NA,NA,NA,NA,NA,NA,NA,NA,115,128,NA,NA,NA,NA,190,212,250,245,257,284,256,245,265,231,252,238,240,265,180,247,276,322) #harvest added
bhcount2=bhcount[which(is.na(bhcount)==FALSE)]
bhgaps=c(9,9,1,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
prcount=c(18,22,NA,NA,NA,NA,NA,NA,98,116,123,NA,159,176,259,295,343,304,320,304,277,288,265,336,396,429,362)
prcount2=prcount[which(is.na(prcount)==FALSE)]
prgaps=c(1,7,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
dmcount=c(52,47,49,41,39,35,35,36,29,17,20,27,32,30,30,32,33,37,47,50,52,62,62)
slcount=c(87,66,63,49,NA,52,52,55,49,53,51,60,65,66,62,63,53,46,54,59)
slcount2=slcount[which(is.na(slcount)==FALSE)]
slgaps=c(1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
gbcount=c(40,27,32,33,39,24,18,17,13,13,19,21,19,25,33,34,23,17,15,19,21,25)
lscount=c(50,57,62,54,53,47,34,43,46,49,41,36,45,44,48,50,47,59,60,55,56,57)
nyrha=length(hacount);nyrbh=length(bhcount2);nyrpr=length(prcount2);nyrdm=length(dmcount);nyrsl=length(slcount2);nyrgb=length(gbcount);nyrls=length(lscount)
haR=rep(NA,nyrha-1);bhR=rep(NA,nyrbh-1);prR=rep(NA,nyrpr-1);dmR=rep(NA,nyrdm-1);slR=rep(NA,nyrsl-1);gbR=rep(NA,nyrgb-1);lsR=rep(NA,nyrls-1)
for(i in 1:nyrha-1){
  haR[i]=log(hacount[i+1]/hacount[i])
}
for(i in 1:nyrbh-1){
  bhR[i]=log(bhcount2[i+1]/bhcount2[i])/bhgaps[i]
}
for(i in 1:nyrpr-1){
  prR[i]=log(prcount2[i+1]/prcount2[i])/prgaps[i]
}
for(i in 1:nyrdm-1){
  dmR[i]=log(dmcount[i+1]/dmcount[i])
}
for(i in 1:nyrsl-1){
  slR[i]=log(slcount2[i+1]/slcount2[i])/slgaps[i]
}
for(i in 1:nyrgb-1){
  gbR[i]=log(gbcount[i+1]/gbcount[i])
}
for(i in 1:nyrls-1){
  lsR[i]=log(lscount[i+1]/lscount[i])
}

Regdata<-list(hacount=hacount,haR=haR,nyrha=nyrha,
              bhcount=bhcount2,bhR=bhR,nyrbh=nyrbh,
              prcount=prcount2,prR=prR,nyrpr=nyrpr,
              dmcount=dmcount,dmR=dmR,nyrdm=nyrdm,
              slcount=slcount2,slR=slR,nyrsl=nyrsl,
              gbcount=gbcount,gbR=gbR,nyrgb=nyrgb,
              lscount=lscount,lsR=lsR,nyrls=nyrls)

#Both model
sink(file="Regression")
cat(
    "model {

    #ADDITIVE
    
    for(ii in 1:7){
    alpha[ii]~dnorm(mu.alpha,tau.alpha) 
    }
    mu.alpha~dnorm(0,0.001) #uninformative prior for mean rmax across herds
    tau.alpha<-1/(sigma.alpha*sigma.alpha)
    sigma.alpha~dunif(0,100)
    #mu.alpha~dnorm(0.27,0.01) #informative prior for mean rmax across herds, mean drawn from estimates of rmax from literature
    #tau.alpha<-1/(sigma.alpha*sigma.alpha)
    #sigma.alpha~dunif(0.01,0.1) #standard deviation representative of variation in estimates of rmax from 
    #alpha1~dnorm(0,0.001) #NOT random effect
    #alpha2~dnorm(0,0.001)
    #alpha3~dnorm(0,0.001)
    #alpha4~dnorm(0,0.001)
    #alpha5~dnorm(0,0.001)
    #alpha6~dnorm(0,0.001)
    #alpha7~dnorm(0,0.001)

    tau.add1<-1/(sigma.add1*sigma.add1) #uninformative prior for additive variance across time for each herd
    sigma.add1~dunif(0,100)
    tau.add2<-1/(sigma.add2*sigma.add2)
    sigma.add2~dunif(0,100)
    tau.add3<-1/(sigma.add3*sigma.add3)
    sigma.add3~dunif(0,100)
    tau.add4<-1/(sigma.add4*sigma.add4)
    sigma.add4~dunif(0,100)
    tau.add5<-1/(sigma.add5*sigma.add5)
    sigma.add5~dunif(0,100)
    tau.add6<-1/(sigma.add6*sigma.add6)
    sigma.add6~dunif(0,100)
    tau.add7<-1/(sigma.add7*sigma.add7)
    sigma.add7~dunif(0,100)
    
    #MULTIPLICATIVE

    #beta1~dnorm(0,0.001) #uninformative prior for SDD for each herd
    beta2~dnorm(0,0.001)
    beta3~dnorm(0,0.001)
    beta4~dnorm(0,0.001)
    beta5~dnorm(0,0.001)
    beta6~dnorm(0,0.001)
    beta7~dnorm(0,0.001)

    mu.beta1~dnorm(0,0.001)
    tau.mult1<-1/(sigma.mult1*sigma.mult1)
    sigma.mult1~dunif(0,100)
    mu.beta2~dnorm(0,0.001) #uninformative prior for mean SDD for each herd
    tau.mult2<-1/(sigma.mult2*sigma.mult2) #uninformative prior for multiplicative variance across time for each herd
    sigma.mult2~dunif(0,100)
    mu.beta3~dnorm(0,0.001)
    tau.mult3<-1/(sigma.mult3*sigma.mult3)
    sigma.mult3~dunif(0,100)
    mu.beta4~dnorm(0,0.001)
    tau.mult4<-1/(sigma.mult4*sigma.mult4)
    sigma.mult4~dunif(0,100)
    mu.beta5~dnorm(0,0.001)
    tau.mult5<-1/(sigma.mult5*sigma.mult5)
    sigma.mult5~dunif(0,100)
   mu.beta6~dnorm(0,0.001)
    tau.mult6<-1/(sigma.mult6*sigma.mult6)
    sigma.mult6~dunif(0,100)
    mu.beta7~dnorm(0,0.001)
    tau.mult7<-1/(sigma.mult7*sigma.mult7)
    sigma.mult7~dunif(0,100)
    
    #VARIATION NOT CAPTURED BY MODEL

    tau.ha<-1/(sigma.ha*sigma.ha)
    sigma.ha~dunif(0,100)
    tau.bh<-1/(sigma.bh*sigma.bh)
    sigma.bh~dunif(0,100)
    tau.pr<-1/(sigma.pr*sigma.pr)
    sigma.pr~dunif(0,100)
    tau.dm<-1/(sigma.dm*sigma.dm)
    sigma.dm~dunif(0,100)
    tau.sl<-1/(sigma.sl*sigma.sl)
    sigma.sl~dunif(0,100)
    tau.gb<-1/(sigma.gb*sigma.gb)
    sigma.gb~dunif(0,100)
    tau.ls<-1/(sigma.ls*sigma.ls)
    sigma.ls~dunif(0,100)
    
    #MODELS

    for(i in 1:(nyrha-1)){
    haR[i]~dnorm(mu.ha[i],tau.ha)
    #mu.ha[i]<-alpha[1]+beta1*hacount[i]
    mu.ha[i]<-alpha[1]+beta1[i]*hacount[i]+eps.add1[i]
    eps.add1[i]~dnorm(0,tau.add1)
    beta1[i]~dnorm(mu.beta1,tau.mult1)
    }
    
    for(i in 1:(nyrpr-1)){
    prR[i]~dnorm(mu.pr[i],tau.pr)
    mu.pr[i]<-alpha[2]+beta2*prcount[i]
    #mu.pr[i]<-alpha[2]+beta2[i]*prcount[i]#+eps.add2[i]
    #eps.add2[i]~dnorm(0,tau.add2)
    #beta2[i]~dnorm(mu.beta2,tau.mult2)
    }
    
    for(i in 1:(nyrbh-1)){
    bhR[i]~dnorm(mu.bh[i],tau.bh)
    mu.bh[i]<-alpha[3]+beta3*bhcount[i]
    #mu.bh[i]<-alpha[3]+beta3[i]*bhcount[i]+eps.add3[i]
    #eps.add3[i]~dnorm(0,tau.add3)
    #beta3[i]~dnorm(mu.beta3,tau.mult3)
    }
    
    for(i in 1:(nyrsl-1)){
    slR[i]~dnorm(mu.sl[i],tau.sl)
    mu.sl[i]<-alpha[4]+beta4*slcount[i]
    #mu.sl[i]<-alpha[4]+beta4[i]*slcount[i]+eps.add4[i]
    #eps.add4[i]~dnorm(0,tau.add4)
    #beta4[i]~dnorm(mu.beta4,tau.mult4)
    }
    
    for(i in 1:(nyrls-1)){
    lsR[i]~dnorm(mu.ls[i],tau.ls)
    mu.ls[i]<-alpha[5]+beta5*lscount[i]
    #mu.ls[i]<-alpha[5]+beta5[i]*lscount[i]+eps.add5[i]
    #eps.add5[i]~dnorm(0,tau.add5)
    #beta5[i]~dnorm(mu.beta5,tau.mult5)
    }
    
    for(i in 1:(nyrdm-1)){
    dmR[i]~dnorm(mu.dm[i],tau.dm)
    mu.dm[i]<-alpha[6]+beta6*dmcount[i] 
    #mu.dm[i]<-alpha[6]+beta6[i]*dmcount[i]+eps.add6[i]
    #eps.add6[i]~dnorm(0,tau.add6)
    #beta6[i]~dnorm(mu.beta6,tau.mult6)
    }
    
    for(i in 1:(nyrgb-1)){
    gbR[i]~dnorm(mu.gb[i],tau.gb)
    mu.gb[i]<-alpha[7]+beta7*gbcount[i]
    #mu.gb[i]<-alpha[7]+beta7[i]*gbcount[i]+eps.add7[i]
    #eps.add7[i]~dnorm(0,tau.add7)
    #beta7[i]~dnorm(mu.beta7,tau.mult7)
    }
    
    }"
    
    ,fill=TRUE)
sink()

params<-c("mu.alpha","sigma.alpha",
          "alpha",
          "sigma.add1",
          "sigma.add2",
          "sigma.add3",
          "sigma.add4",
          "sigma.add5",
          "sigma.add6",
          "sigma.add7",
         # "beta1",
        #  "beta2",
         # "beta3",
          #"beta4",
        #  "beta5",
        #  "beta6",
         # "beta7",
          "mu.beta1","sigma.mult1",
          "mu.beta2","sigma.mult2",
          "mu.beta3","sigma.mult3",
          "mu.beta4","sigma.mult4",
          "mu.beta5","sigma.mult5",
          "mu.beta6","sigma.mult6",
          "mu.beta7","sigma.mult7",
          "sigma.ha","sigma.bh","sigma.pr","sigma.sl","sigma.ls","sigma.dm","sigma.gb")
inits<-function(){list(mu.alpha=0,sigma.alpha=0.1,
                       sigma.add1=1,
                       sigma.add2=1,
                       sigma.add3=1,
                       sigma.add4=1,
                       sigma.add5=1,
                       sigma.add6=1,
                       sigma.add7=1,
                       #beta1=0,
                       #beta2=0,
                       #beta3=0,
                       #beta4=0,
                       #beta5=0,
                       #beta6=0,
                       #beta7=0,
                       mu.beta1=0,sigma.mult1=1,
                       mu.beta2=0,sigma.mult2=1,
                       mu.beta3=0,sigma.mult3=1,
                       mu.beta4=0,sigma.mult4=1,
                       mu.beta5=0,sigma.mult5=1,
                       mu.beta6=0,sigma.mult6=1,
                       mu.beta7=0,sigma.mult7=1,
                       sigma.ha=1,sigma.bh=1,sigma.pr=1,sigma.dm=1,sigma.sl=1,sigma.gb=1,sigma.ls=1)}

fitreg<-jags(data=Regdata,inits=inits,parameters.to.save=params,model.file="Regression",n.thin=1,
             #n.chains=2,n.burnin=10,n.iter=40,parallel=FALSE, DIC = TRUE) #use this when testing model
             n.chains=3,n.burnin=75000,n.iter=150000,n.adapt=75000,parallel=TRUE)
print(fitreg,5)

apgraphs <- plot(fitreg)


Kha=(-1*fitreg$sims.list$alpha[,1])/fitreg$sims.list$mu.beta1
median(Kha)
quantile(Kha,probs=c(0.025,0.975))
sd(Kha)

Kpr=(-1*fitreg$sims.list$alpha[,2])/fitreg$sims.list$mu.beta2
median(Kpr)
quantile(Kpr,probs=c(0.025,0.975))
sd(Kpr)

Kbh=(-1*fitreg$sims.list$alpha[,3])/fitreg$sims.list$mu.beta3
median(Kbh)
quantile(Kbh,probs=c(0.025,0.975))
sd(Kbh)

Ksl=(-1*fitreg$sims.list$alpha[,4])/fitreg$sims.list$mu.beta4
median(Ksl)
quantile(Ksl,probs=c(0.025,0.975))
sd(Ksl)

Kls=(-1*fitreg$sims.list$alpha[,5])/fitreg$sims.list$mu.beta5
median(Kls)
quantile(Kls,probs=c(0.025,0.975))
sd(Kls)

Kdm=(-1*fitreg$sims.list$alpha[,6])/fitreg$sims.list$mu.beta6
median(Kdm)
quantile(Kdm,probs=c(0.025,0.975))
sd(Kdm)

Kgb=(-1*fitreg$sims.list$alpha[,7])/fitreg$sims.list$mu.beta7
median(Kgb)
quantile(Kgb,probs=c(0.025,0.975))
sd(Kgb)

