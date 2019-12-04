## Author: A.D. Wright
## Project: NCRN Amphibians - Multi-region, Community Occupancy Model
## Code: Analysis code to fit the data (MRCM_data.R) the JaGS model (MRCM.txt) in R

#Empty memory (if necessary)
  #rm(list=ls())

#########
## Part - Working Directory, Data, Packages
#########

## Set Working Directory (if necessary)
  #setwd("")

## Load Data
load("MRCM_data.R") 
  #Files: X (data), 
        # Park_area (cov), Forest_cov (cov), Isol (cov),
        # Site_area (cov), Hydro_state (cov), Conn (cov), Precip (cov), Cond (cov),
        # JDay (cov),
        # J (nSites), minJ (index of first site of each region), maxJ (index of last site of each region),
        # K (nReps for site by year), Y (nYears), minY (first year a Park was surveyed),
        # I (nSpp), M (nZeroes), R (nRegions)

# PARKS: 1)CATO,2)CHOH,3)GWMP, 4)HAFE,5)MANA,6)MONO,7)NACE,8)PRWI,9)ROCR
# SPP:   1)ACRE,2)AMAC,3)AOPA,4)BUFO,5)HSCU,6)HYLA,7)NVIR,
#        8)PCRU,9)PFER,10)RCAT,11)RCLA,12)RPAL,13)RSPH,14)RSYL,15)SHOL

## Install Packages
  #jagsUI
if(!require(jagsUI)) {install.packages("jagsUI");require(jagsUI)}

#########
## Part - jagsUI Code 
#########

##
#### Create necessary arguments to run jags() command
##

# Data
jagsDat <- list(X=X, JDay=JDay, 
                Site_area=Site_area, # mean_SA=mean_SA,
                Hydro_state=Hydro_state, Precip=Precip, Conn=Conn, Cond=Cond,
                Park_area=Park_area, Forest_cov=Forest_cov, Isol=Isol,
                minJ=minJ, maxJ=maxJ, K=K, Y=Y, minY=minY, I=I, M=M, R=R)

# Initial Values
#W
WInit <- array(1,dim=c(I+M,R))
#Z - Can only be 1 in cells that model loops over, otherwise there is an error
ZInit <- array(NA,dim=c(J,Y,I+M,R))
for(r in 1:dim(ZInit)[4]){
  for(i in 1:dim(ZInit)[3]){
    for(j in minJ[r]:maxJ[r]){
      for(y in minY[r]:dim(ZInit)[2]){
        ZInit[j,y,i,r] <- 1
            }
          }
        }
      }
# Compile inits
jagsIni <- function(){
  list(Z=ZInit, W=WInit, mu.b0.global=rnorm(1,0,0.5), mu.omega=rnorm(1,-1,0.5))
}

# Save Parameters
jagsPar <- c('mu.omega','alpha.omega','beta.omega','gamma.omega','omega','Npark',
             'mean.a0.global','mu.a0.global','mu.a0_S.global','mean.a0_S.global','mu.a0_P.global','mean.a0_P.global',
             'mu.a1.global','mu.a3.global', 'tau.a2.time', 'mu.a4.global','mu.a5.global',
             'mu.a0','mu.a0_S','mu.a0_P','mu.a1','mu.a3', 'mu.a4','mu.a5',
             'mean.a0','mean.a0_S','mean.a0_P',           
             'a0.obs','a0_S.obs','a0_P.obs','a1.avg','a2.t.avg','a3.avg','a4.avg','a5.avg',
             'mu.b0.global','mu.b1.global','mu.b2.global','tau.b.time','eta',
             'mu.b0','mu.b1','mu.b2','b0','b1','b2',
             'nSite','Nsite.avg','psi.avg','W.real'
            )

##
#### Run Model using jags() command
##

# #Test run
# jagsFit <- jags(data = jagsDat,
#                 inits = jagsIni,
#                 parameters.to.save = jagsPar,
#                 model.file = "MRCM.txt",
#                 n.chains=1,
#                 n.iter=100, #for jags
#                 parallel=F
#                )

#Set JAGS numbers
n.iter <- 350000
n.burnin <- 325000
n.thin <- 10
n.adapt <- 10000

#Run
jagsFit <- jags(data = jagsDat,
                inits = jagsIni,
                parameters.to.save = jagsPar,
                model.file = "MRCM.txt",
                n.chains=3,
                n.iter=n.iter,
                n.adapt=n.adapt,
                n.burnin=n.burnin,
                n.thin=n.thin,
                parallel=T
               )

# Create results file name
date <- Sys.Date()
iter<-n.iter/1000
file_str <- paste("jagsFit_","MRCM_",iter,"k_",date,".R",sep="")
#Save
save(jagsFit,file=file_str)





