## Author: A.D. Wright
## Project: NCRN Amphibians - Multi-region, Community Occupancy Model
## Code: Results and plots code for the results file

#Empty memory
  #rm(list=ls())

#########
## Part - Working Directory, Data, Packages
#########

## Set Working Directory
  #setwd()

## Load Data
load("MRCM_data.R") 

## Load data/results
  #load(file="".R)
results <- jagsFit

#Describe Park and Spp dataframes
# PARKS: CATO CHOH GWMP HAFE MANA MONO NACE PRWI ROCR
Parks <- data.frame(park=seq(1:R), Park=c("CATO","CHOH","GWMP","HAFE","MANA","MONO","NACE","PRWI","ROCR"), firstYear = minY)
# SPP:   1)ACRE,2)AMAC,3)AOPA,4)BUFO,5)HSCU,6)HYLA,7)NVIR,
#        8)PCRU,9)PFER,10)RCAT,11)RCLA,12)RPAL,13)RSPH,14)RSYL,15)SHOL
Spp <- data.frame(spp=seq(1:I), Spp=c('ACRE','AMAC','AOPA','BUFO','HSCU','HYLA','NVIR','PCRU','PFER','RCAT','RCLA','RPAL','RSPH','RSYL','SHOL'))


## Install Packages
  #jagsUI
if(!require(jagsUI)) {install.packages("jagsUI");require(jagsUI)}
  #tidyverse
if(!require(tidyverse)) {install.packages("tidyverse");require(tidyverse)}
  #ggpubr
if(!require(ggpubr)) {install.packages("ggpubr");require(ggpubr)}


#########
## Part - View JAGS Results/Diagnostics
#########

options(max.print=500)
results

# #View traceplots
# traceplot(results, parameters=c('mu.omega','mu.a0.global', 'mu.b0.global'))
#                                
# traceplot(results, parameters=c('mu.omega','alpha.omega','beta.omega','gamma.omega'))
# traceplot(results, parameters=c('mu.a0.global','mu.a0_S.global','mu.a0_P.global','mu.a1.global', 'mu.a3.global','mu.a4.global','mu.a5.global'))
# traceplot(results, parameters=c('tau.b.time','tau.a2.time'))
# traceplot(results, parameters=c('mu.b0.global','mu.b1.global','mu.b2.global'))


#########
## Part - General Data Management 
#########

#Observed species at each Park
sppPark <- matrix(NA, nrow = dim(Spp)[1], ncol = dim(Parks)[1])
for(j in 1:dim(Parks)[1]){
  for(i in 1:dim(Spp)[1])
    sppPark[i,j] <- max(X[,,,i,j],na.rm=T)
}
rownames(sppPark) <- Spp$Spp
colnames(sppPark) <- Parks$Park

obsN <- as.vector(colSums(sppPark))


#########
## Part - Plotting Metacommunity size & Effects (Figure 1)
#########

#Organize the data
metaC <- data.frame(Park=seq(1:R),   
                    Mean=results$mean$Npark,
                    Obs=obsN,
                    Lower=results$q2.5$Npark,
                    Lower_25=results$q25$Npark,
                    Upper_75=results$q75$Npark,
                    Upper=results$q97.5$Npark)


#Plot
aa <- ggplot(data=metaC, aes(x=Park, y=Mean)) + 
        geom_crossbar(aes(ymin=Lower_25, ymax=Upper_75), width=.5, fill='black', alpha=0.25) +
        geom_errorbar(aes(ymin=Upper, ymax=Upper), width=.5) +
        geom_errorbar(aes(ymin=Lower, ymax=Lower), width=.5) +
        geom_linerange(aes(ymin=Upper_75, ymax=Upper)) +
        geom_linerange(aes(ymax=Lower_25, ymin=Lower)) +
        geom_point(aes(x=Park,y=Obs),size=2,color='red') +
        ylab('Metacommunity Size') + xlab('') + #ylim(5,20) + 
        theme_bw() +
        theme(axis.text.y=element_text(color='black', size=8), 
              axis.text.x=element_text(color='black', size=8,angle=45,hjust=0.5,vjust=0.5), 
              axis.title.y=element_text(size=10)) +
        scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9),labels=Parks[,2]) 

## Correlation
correl <- matrix(NA, ncol = 3, nrow = results$mcmc.info$n.samples)
for (i in 1:results$mcmc.info$n.samples){
  correl[i,1] <- cor(Isol, results$sims.list$Npark[i,])
  correl[i,2] <- cor(Park_area, results$sims.list$Npark[i,])
  correl[i,3] <- cor(Forest_cov, results$sims.list$Npark[i,])
}
quantile(correl[,1], c(0.025, 0.5, 0.975), na.rm = TRUE)
quantile(correl[,2], c(0.025, 0.5, 0.975), na.rm = TRUE)
quantile(correl[,3], c(0.025, 0.5, 0.975), na.rm = TRUE)


#Plotting by covariate
  #ISOL
metaC <- data.frame(Park=Isol_unscaled,
                    Mean=results$mean$Npark,
                    Obs=obsN,
                    Lower=results$q2.5$Npark,
                    Lower_25=results$q25$Npark,
                    Upper_75=results$q75$Npark,
                    Upper=results$q97.5$Npark)
#Plot
isol_rich <- ggplot(data=metaC, aes(x=Park, y=Mean)) +
  geom_crossbar(aes(ymin=Lower_25, ymax=Upper_75), width=.1, fill='black', alpha=0.25) +
  geom_errorbar(aes(ymin=Upper, ymax=Upper), width=.1) +
  geom_errorbar(aes(ymin=Lower, ymax=Lower), width=.1) +
  geom_linerange(aes(ymin=Upper_75, ymax=Upper)) +
  geom_linerange(aes(ymax=Lower_25, ymin=Lower)) +
  geom_point(aes(x=Park,y=Obs), size=2,color='red') +
  ylab('Metacommunity Size') + xlab('% Isolation') + 
  theme_bw() +
  theme(axis.text.y=element_text(color='black', size=8),
        axis.text.x=element_text(color='black', size=8,angle=90,hjust=0.5,vjust=0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))
#AREA
metaC <- data.frame(Park=Park_area_unscaled/1000000,
                    Mean=results$mean$Npark,
                    Obs=c(10,14,9,9,13,10,7,11,6),
                    Lower=results$q2.5$Npark,
                    Lower_25=results$q25$Npark,
                    Upper_75=results$q75$Npark,
                    Upper=results$q97.5$Npark)
area_rich <- ggplot(data=metaC, aes(x=Park, y=Mean)) +
  geom_crossbar(aes(ymin=Lower_25, ymax=Upper_75), width=10, fill='black', alpha=0.25) +
  geom_errorbar(aes(ymin=Upper, ymax=Upper), width=10) + 
  geom_errorbar(aes(ymin=Lower, ymax=Lower), width=10) + 
  geom_linerange(aes(ymin=Upper_75, ymax=Upper)) +
  geom_linerange(aes(ymax=Lower_25, ymin=Lower)) +
  geom_point(aes(x=Park,y=Obs), size=2,color='red') +
  ylab('') + xlab(expression("Area (in millions of m"^{2}*")")) + xlim(0,100) + 
  theme_bw() +
  theme(axis.text.y=element_text(color='black', size=8),
        axis.text.x=element_text(color='black', size=8,angle=90,hjust=0.5,vjust=0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))
#FOREST
metaC <- data.frame(Park=Forest_cov_unscaled,
                    Mean=results$mean$Npark,
                    Obs=c(10,14,9,9,13,10,7,11,6),
                    Lower=results$q2.5$Npark,
                    Lower_25=results$q25$Npark,
                    Upper_75=results$q75$Npark,
                    Upper=results$q97.5$Npark)
fore_rich <- ggplot(data=metaC, aes(x=Park, y=Mean)) +
  geom_crossbar(aes(ymin=Lower_25, ymax=Upper_75), width=.1, fill='black', alpha=0.25) +
  geom_errorbar(aes(ymin=Upper, ymax=Upper), width=.1) +
  geom_errorbar(aes(ymin=Lower, ymax=Lower), width=.1) +
  geom_linerange(aes(ymin=Upper_75, ymax=Upper)) +
  geom_linerange(aes(ymax=Lower_25, ymin=Lower)) +
  geom_point(aes(x=Park,y=Obs), size=2,color='red') + 
  ylab('') + xlab('% Forest Cover') + 
  theme_bw() +
  theme(axis.text.y=element_text(color='black', size=8),
        axis.text.x=element_text(color='black', size=8,angle=90,hjust=0.5,vjust=0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10)) 

covs_rich <- ggarrange(isol_rich,area_rich,fore_rich,ncol=3,nrow=1,widths=c(1,1,1),labels=c('(c)','(d)','(e)'), font.label = list(size = 10), vjust = 1)
  


##
#### Plotting effects of covariates on metacommunity size
##

#Organize the data
metaC_effects <- data.frame(Covariate = as.factor(c("Area", "% Forest",'% Isolated')),
                            Mean = c(results$mean$alpha.omega, results$mean$beta.omega,results$mean$gamma.omega),
                            Lower = c(results$q2.5$alpha.omega, results$q2.5$beta.omega,results$q2.5$gamma.omega),
                            Lower_25 = c(results$q25$alpha.omega, results$q25$beta.omega,results$q25$gamma.omega),
                            Upper_75 = c(results$q75$alpha.omega, results$q75$beta.omega,results$q75$gamma.omega),
                            Upper = c(results$q97.5$alpha.omega, results$q97.5$beta.omega,results$q97.5$gamma.omega)
                            )
#plot
bb <- ggplot(data=metaC_effects, aes(x=Covariate, y=Mean)) + 
        coord_flip()+
        geom_hline(yintercept=0, linetype="dashed") + 
        geom_point(size=5, shape=18) +
        geom_errorbar(aes(ymin=Upper, ymax=Upper), width=.25) +
        geom_errorbar(aes(ymin=Lower, ymax=Lower), width=.25) +
        geom_linerange(aes(ymin=Mean, ymax=Upper_75),size=2,color='black') +
        geom_linerange(aes(ymax=Mean, ymin=Lower_25),size=2,color='black') +
        geom_linerange(aes(ymin=Upper_75, ymax=Upper),color='black') +
        geom_linerange(aes(ymax=Lower_25, ymin=Lower),color='black') +
        ylab('')  + xlab('') + # ylim(-0.75,0.75)+
        theme_bw() +
        theme(axis.text.y=element_text(color='black', size=8,angle=270, vjust=0.5,hjust=0.5), 
              axis.text.x=element_text(color='black', size=8)) 

#Determining the credible interval on either side of 0
mean((results$sims.list$alpha.omega)>0)
mean((results$sims.list$gamma.omega)<0)
mean((results$sims.list$beta.omega)<0)

## Putting aa and bb together
bbaa <- ggarrange(bb,aa,ncol=2,nrow=1,widths=c(1,2),labels=c('(a)','(b)'), font.label = list(size = 10), vjust = 1)


## ALL TOGETHER TO MAKE PUBLICATION QUALITY FIGURE
  #Set image specifications
png('Figure1.png', width = 6.5, height = 4.5, units = 'in', res = 450)
  #Make plot
ggarrange(bbaa,covs_rich,nrow=2,heights=c(1,1)) 
  #Save plot
dev.off()



#########
## Part - Plotting Occupancy Effects (Figure 2, ESM_2)
#########

##
#### Organize data
##

#Intercepts
occ_intercept <- data.frame(Covariate = c("Temp", "Semi", "Perm"),
                            Mean = c(results$mean$mean.a0.global, results$mean$mean.a0_S.global, results$mean$mean.a0_P.global),
                            Lower = c(results$q2.5$mean.a0.global, results$q2.5$mean.a0_S.global, results$q2.5$mean.a0_P.global),
                            Lower_25 = c(results$q25$mean.a0.global, results$q25$mean.a0_S.global, results$q25$mean.a0_P.global),      
                            Upper_75 = c(results$q75$mean.a0.global, results$q75$mean.a0_S.global, results$q75$mean.a0_P.global), 
                            Upper = c(results$q97.5$mean.a0.global, results$q97.5$mean.a0_S.global, results$q97.5$mean.a0_P.global),
                            Overlap = c(results$overlap0$mean.a0.global, results$overlap0$mean.a0_S.global, results$overlap0$mean.a0_P.global)
                            )
occ_intercept$Covariate <- factor(occ_intercept$Covariate, levels=unique(occ_intercept$Covariate))
occ_intercept$park <- 10; occ_intercept$Park <- 'NCRN'; occ_intercept <- occ_intercept<-occ_intercept[,c('Covariate','park','Park','Mean','Lower','Lower_25','Upper_75','Upper','Overlap')]

#Slopes
occ_effects <- data.frame(Covariate = c("Area", "Prec","Conn",'Cond'),
                          Mean = c(results$mean$mu.a1.global,  results$mean$mu.a3.global , results$mean$mu.a4.global , results$mean$mu.a5.global),
                          Lower = c(results$q2.5$mu.a1.global,  results$q2.5$mu.a3.global, results$q2.5$mu.a4.global, results$q2.5$mu.a5.global),
                          Lower_25 = c(results$q25$mu.a1.global, results$q25$mu.a3.global, results$q25$mu.a4.global, results$q25$mu.a5.global),
                          Upper_75 = c(results$q75$mu.a1.global, results$q75$mu.a3.global, results$q75$mu.a4.global, results$q75$mu.a5.global),
                          Upper = c(results$q97.5$mu.a1.global, results$q97.5$mu.a3.global, results$q97.5$mu.a4.global, results$q97.5$mu.a5.global),
                          Overlap = c(results$overlap0$mu.a1.global,  results$overlap0$mu.a3.global, results$overlap0$mu.a4.global, results$overlap0$mu.a5.global)
                          )
occ_effects$Covariate <- factor(occ_effects$Covariate, levels=unique(occ_effects$Covariate))
occ_effects$park <- 10; occ_effects$Park <- 'NCRN'; occ_effects <- occ_effects<-occ_effects[,c('Covariate','park','Park','Mean','Lower','Lower_25','Upper_75','Upper','Overlap')]

## Park-specific estimates
#Intercepts
occ_int_park <- data.frame(Covariate = rep(c("Temp", "Semi", "Perm"),each=dim(Parks)[1]),
                           park=rep(Parks[,1],times=3),
                           Park=rep(Parks[,2],times=3),
                           Mean = c(results$mean$mean.a0, results$mean$mean.a0_S, results$mean$mean.a0_P),
                           Lower = c(results$q2.5$mean.a0, results$q2.5$mean.a0_S, results$q2.5$mean.a0_P),
                           Lower_25 = c(results$q25$mean.a0, results$q25$mean.a0_S, results$q25$mean.a0_P),      
                           Upper_75 = c(results$q75$mean.a0, results$q75$mean.a0_S, results$q75$mean.a0_P), 
                           Upper = c(results$q97.5$mean.a0, results$q97.5$mean.a0_S, results$q97.5$mean.a0_P),
                           Overlap = c(results$overlap0$mean.a0, results$overlap0$mean.a0_S, results$overlap0$mean.a0_P)
                           )
occ_int_park$Covariate <- factor(occ_int_park$Covariate, levels=unique(occ_int_park$Covariate))
occ_int_park <- rbind(occ_int_park,occ_intercept)
occ_int_park$propDir <- NA

#Slopes
occ_effects_park <- data.frame(Covariate = rep(c("Area", "Prec","Conn",'Cond'),each=dim(Parks)[1]),
                              park=rep(Parks[,1],times=4),
                              Park=rep(Parks[,2],times=4),
                              Mean = c(results$mean$mu.a1,  results$mean$mu.a3, results$mean$mu.a4, results$mean$mu.a5),
                              Lower = c(results$q2.5$mu.a1, results$q2.5$mu.a3, results$q2.5$mu.a4, results$q2.5$mu.a5),
                              Lower_25 = c(results$q25$mu.a1,  results$q25$mu.a3, results$q25$mu.a4, results$q25$mu.a5),
                              Upper_75 = c(results$q75$mu.a1,  results$q75$mu.a3, results$q75$mu.a4, results$q75$mu.a5),
                              Upper = c(results$q97.5$mu.a1,  results$q97.5$mu.a3, results$q97.5$mu.a4, results$q97.5$mu.a5),
                              Overlap = c(results$overlap0$mu.a1,  results$overlap0$mu.a3, results$overlap0$mu.a4, results$overlap0$mu.a5)
                              )
occ_effects_park$Covariate <- factor(occ_effects_park$Covariate, levels=unique(occ_effects_park$Covariate))
occ_effects_park <- rbind(occ_effects_park,occ_effects)

##
#### Table
##

##Add probability of direction to slope object
propDir <- rep(NA,40)
for(i in 1:9){
  propDir[i] <- mean(results$sims.list$mu.a1[,i]>0)
}
propDir[10] <- mean(results$sims.list$mu.a1.global>0)
for(i in 1:9){
  propDir[i+10] <- mean(results$sims.list$mu.a3[,i]>0)
}
propDir[10+10] <- mean(results$sims.list$mu.a3.global>0)
for(i in 1:9){
  propDir[i+20] <- mean(results$sims.list$mu.a4[,i]>0)
}
propDir[10+20] <- mean(results$sims.list$mu.a4.global>0)
for(i in 1:9){
  propDir[i+30] <- mean(results$sims.list$mu.a5[,i]>0)
}
propDir[10+30] <- mean(results$sims.list$mu.a5.global>0)
occ_effects_park$propDir <- propDir


## Create table 
occ_effects_park_table <- rbind(occ_int_park,occ_effects_park) %>% select(Covariate, Park, Mean, Lower, Lower_25,Upper_75,Upper,propDir) 
      #https://stackoverflow.com/questions/23217520/limiting-the-number-of-decimals-in-a-dataframe-r
is.num <- sapply(occ_effects_park_table, is.numeric)
occ_effects_park_table[is.num] <- lapply(occ_effects_park_table[is.num], round, 3)
# Edit table
dummy <- occ_effects_park_table
occ_effects_park_table <- occ_effects_park_table %>% mutate(fifty = paste(Lower_25," - ",Upper_75), ninetyfive = paste(Lower," - ",Upper))

#Export table for word
  ##https://sejdemyr.github.io/r-tutorials/basics/tables-in-r/
#write.table(occ_effects_park_table,'park_effects.txt',sep=",",quote=F,row.names=F)

        
##
#### Plots
##


# Intercepts
ee <- ggplot(data=occ_int_park, aes(x=Covariate, y=Mean,color=Park)) + 
        geom_point(aes(color=Park),position=position_dodge(0.8))+
        geom_crossbar(aes(ymin=Lower_25, ymax=Upper_75,color=Park), width=0.5, position=position_dodge(0.8))+
        geom_linerange(aes(ymin=Upper_75, ymax=Upper,color=Park),size=0.1, position=position_dodge(0.8)) +
        geom_linerange(aes(ymax=Lower_25, ymin=Lower,color=Park),size=0.1, position=position_dodge(0.8)) +
        ylab('Intercept') + ylim(0,1) + xlab('') + 
        theme_bw() +
        theme(axis.text.x=element_text(color='black', size=10), axis.title.x=element_text(size=10) ,
              axis.text.y=element_text(color='black', size=10), axis.title.y=element_text(size=10),
              legend.text=element_text( size=10), legend.title=element_text(size=10))  +
        theme(plot.title=element_text(size=16, hjust=0.5))+
        scale_colour_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#1a9850','#a65628','#f781bf','#878787','black'))  

#Slopes
ff <- ggplot(data=occ_effects_park, aes(x=Covariate, y=Mean)) + 
        geom_point(aes(color=Park),position=position_dodge(0.8))+
        geom_crossbar(aes(ymin=Lower_25, ymax=Upper_75,color=Park), width=0.5, position=position_dodge(0.8))+
        geom_linerange(aes(ymin=Upper_75, ymax=Upper,color=Park),size=0.25, position=position_dodge(0.8)) +
        geom_linerange(aes(ymax=Lower_25, ymin=Lower,color=Park),size=0.25, position=position_dodge(0.8)) +
        geom_hline(yintercept=0, linetype="dashed") + 
        ylab('Slope') + ylim(-2,3) + xlab('') + 
        theme_bw() +
        theme(axis.text.x=element_text(color='black', size=10), 
              axis.text.y=element_text(color='black', size=10), 
              axis.title.y=element_text(size=10),
              legend.text=element_text( size=8), legend.title=element_text(size=10)) +
        theme(legend.position="none") +
        scale_colour_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#1a9850','#a65628','#f781bf','#878787','black'))  


## ALL TOGETHER TO MAKE PUBLICATION QUALITY FIGURE
#Set image specifications
png('Figure2.png', width = 5, height = 5.5, units = 'in', res = 450)
#Make plot
ggarrange(ff,ee,ncol=1,nrow=2,widths=c(1,1),labels=c('(a)','(b)'), font.label = list(size = 10), vjust = 1)
#Save plot
dev.off()


#########
## Part - Wetland-specific Species Richness (Figure 3)
######### 

##
#### Organize Data
##

#Alter Hyrdro-period title
Hydr <- rep(NA,length=dim(Hydro_state)[1])
Hydr[Hydro_state[,1] == 1] <- 'Temporary'
Hydr[Hydro_state[,2] == 1] <- 'Semi-permanent'
Hydr[Hydro_state[,3] == 1] <- 'Permanent'

wetRich_area <- data.frame(
                           Area = as.vector(Site_area_unscaled),
                           Hydroperiod = as.factor(rep(Hydr,times=Y)),
                           Mean = as.vector(results$mean$nSite),
                           Lower= as.vector(results$q2.5$nSite),
                           Upper= as.vector(results$q97.5$nSite)
                          )
wetRich_conn  <- data.frame(
                            Conn = as.vector(rep(Conn_unscaled,times=Y)),
                            Hydroperiod = as.factor(rep(Hydr,times=Y)),
                            Mean = as.vector(results$mean$nSite),
                            Lower= as.vector(results$q2.5$nSite),
                            Upper= as.vector(results$q97.5$nSite)
                          )
wetRich_cond  <- data.frame(
                            Cond = as.vector(Cond_unscaled),
                            Hydroperiod = as.factor(rep(Hydr,times=Y)),
                            Mean = as.vector(results$mean$nSite),
                            Lower= as.vector(results$q2.5$nSite),
                            Upper= as.vector(results$q97.5$nSite)
                          )

#Remove NAs & Extreme values
  #Area
wetRich_area <- wetRich_area[complete.cases(wetRich_area),]
wetRich_area_95 <- subset(wetRich_area, Area < quantile(Area, 0.975) & Area > quantile(Area, 0.025)) 
  #Conn
wetRich_conn <- wetRich_conn[complete.cases(wetRich_conn),]
wetRich_conn_95 <- subset(wetRich_conn, Conn < quantile(Conn, 0.975) & Conn > quantile(Conn, 0.025)) 
wetRich_conn_95$Conn <- wetRich_conn_95$Conn/1000
  #Cond
wetRich_cond <- wetRich_cond[complete.cases(wetRich_cond),]
wetRich_cond_95 <- subset(wetRich_cond, Cond < quantile(Cond, 0.975) & Cond > quantile(Cond, 0.025)) 


##
#### Correlations
##


## Calcule correlations across posterior samples 
correl_wet <- matrix(NA, ncol = 3, nrow = results$mcmc.info$n.samples)
#For loop to calculaute correlation
for (i in 1:results$mcmc.info$n.samples){
  correl_wet[i,1] <- cor(rep(Conn_unscaled,times=13), as.vector(results$sims.list$nSite[i,,]), use='pairwise.complete.obs')
  correl_wet[i,2] <- cor(as.vector(Site_area_unscaled), as.vector(results$sims.list$nSite[i,,]), use='pairwise.complete.obs')
  correl_wet[i,3] <- cor(as.vector(Cond_unscaled), as.vector(results$sims.list$nSite[i,,]), use='pairwise.complete.obs')
}
quantile(correl_wet[,1], c(0.025, 0.5, 0.975), na.rm = TRUE)
quantile(correl_wet[,2], c(0.025, 0.5, 0.975), na.rm = TRUE)
quantile(correl_wet[,3], c(0.025, 0.5, 0.975), na.rm = TRUE)


##
#### Plots
##

#plot ## for plotting multiple regression lines on a ggplot (https://stackoverflow.com/questions/40600824/how-to-apply-geom-smooth-for-every-group)
  #Area
zz <- ggplot(data=wetRich_area_95, aes(x=Area, y=Mean)) + 
        geom_point(aes(color=Hydroperiod, shape=Hydroperiod), size = 1.5, alpha=0.55) +
        geom_smooth(method='lm',aes(color=Hydroperiod,fill=Hydroperiod)) +
        theme_classic2() + ylab('Community Size') + xlab('Area') + 
        theme(axis.text.x=element_text(color='black', size=8), axis.title.x=element_text(size=10),
              axis.text.y=element_text(color='black', size=8), axis.title.y=element_text(size=10)) + #, +
        theme(legend.position = 'none') + scale_x_continuous(breaks = c(0,500,1000,1500), labels= c(0,500,1000,1500)) +
        scale_colour_manual(values=c('#225ea8','#1d91c0','#7fcdbb')) +
        scale_fill_manual(values=c('#225ea8','#1d91c0','#7fcdbb'))        

#Conn
yy <- ggplot(data=wetRich_conn_95, aes(x=Conn, y=Mean)) + 
        geom_point(aes(color=Hydroperiod,shape=Hydroperiod), size = 1.5, alpha=0.55) +
        geom_smooth(method='lm',aes(color=Hydroperiod,fill=Hydroperiod)) +
        theme_classic2() + ylab('') + xlab('Connectivity') +
        theme(axis.text.x=element_text(color='black', size=8), axis.title.x=element_text(size=10),
              axis.text.y=element_text(color='black', size=8), axis.title.y=element_text(size=10)) + 
        theme(legend.position = 'none') + 
        scale_colour_manual(values=c('#225ea8','#1d91c0','#7fcdbb')) +
        scale_fill_manual(values=c('#225ea8','#1d91c0','#7fcdbb'))       

#Cond
xx <- ggplot(data=wetRich_cond_95, aes(x=Cond, y=Mean)) + 
        geom_point(aes(color=Hydroperiod,shape=Hydroperiod), size = 1.5, alpha=0.55) +
        geom_smooth(method='lm',aes(color=Hydroperiod,fill=Hydroperiod)) +
        theme_classic2() + ylab('Community Size') + xlab('Conductivity') +
        theme(axis.text.x=element_text(color='black', size=8), axis.title.x=element_text(size=10),
              axis.text.y=element_text(color='black', size=8), axis.title.y=element_text(size=10),
              legend.text=element_text( size=10), legend.title=element_text(size=12)) +
        scale_colour_manual(values=c('#225ea8','#1d91c0','#7fcdbb')) +
        scale_fill_manual(values=c('#225ea8','#1d91c0','#7fcdbb'))


##Create the Legend
  #Code found via https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 
leg <- g_legend(xx)

## Recreate xx without legend
xx <- ggplot(data=wetRich_cond_95, aes(x=Cond, y=Mean)) + 
  geom_point(aes(color=Hydroperiod,shape=Hydroperiod), size = 1.5, alpha=0.55) +
  geom_smooth(method='lm',aes(color=Hydroperiod,fill=Hydroperiod)) +
  theme_classic2() + ylab('Community Size') + xlab('Conductivity') +
  theme(axis.text.x=element_text(color='black', size=8), axis.title.x=element_text(size=10),
        axis.text.y=element_text(color='black', size=8), axis.title.y=element_text(size=10)) + #, +
  theme(legend.position = 'none') +
  scale_colour_manual(values=c('#225ea8','#1d91c0','#7fcdbb')) +
  scale_fill_manual(values=c('#225ea8','#1d91c0','#7fcdbb'))
  
## Put the figures together for each row
rr <- ggarrange(zz,yy,labels=c('(a)','(b)'), font.label = list(size = 10), vjust = 1)
ss <- ggarrange(xx, leg,labels=c('(c)'), font.label = list(size = 10), vjust = 1)


## ALL TOGETHER TO MAKE PUBLICATION QUALITY FIGURE
#Set image specifications
png('Figure3.png', width = 6.5, height = 5, units = 'in', res = 450)
#Make plot
ggarrange (rr,ss, ncol =1, nrow=2)
#Save plot
dev.off()



#########
## Part - Detection 
######### 

## Used Kery & Schaub (Page 435) for guidance

##
####Data management
##

mcmc.sample <- results$mcmc.info$n.samples
mean.date <- mean(JDay_unscaled, na.rm=T)
sd.date <- sd(JDay_unscaled, na.rm=T)
min.date <- min(JDay_unscaled, na.rm=T)
max.date <- max(JDay_unscaled, na.rm=T)
unstd.date.pred <- min.date:max.date
date.pred <- (unstd.date.pred - mean.date)/sd.date
p.pred.date <- plogis(results$mean$mu.b0.global + results$mean$mu.b1.global*date.pred + results$mean$mu.b2.global*date.pred^2)

#For multiple posterior samples
array.p.pred.date <- array(NA, dim=c(length(date.pred), mcmc.sample))
for(i in 1:mcmc.sample){
  array.p.pred.date[,i] <- plogis(results$sims.list$mu.b0.global[i] + results$sims.list$mu.b1.global[i]*date.pred + results$sims.list$mu.b2.global[i]*date.pred^2)
}
sub.set <- sort(sample(1:mcmc.sample, size=2000))

#For CIs
LPB <- apply(array.p.pred.date, 1, quantile, probs=0.025)
UPB <- apply(array.p.pred.date, 1, quantile, probs=0.975)
LPB50 <- apply(array.p.pred.date, 1, quantile, probs=0.25)
UPB50 <- apply(array.p.pred.date, 1, quantile, probs=0.75)  

### For CHOH
CHOH.p.pred.date <- plogis(results$mean$mu.b0[2] + results$mean$mu.b1[2]*date.pred + results$mean$mu.b2[2]*date.pred^2)
array.CHOH.p.pred.date <- array(NA, dim=c(length(date.pred), mcmc.sample))
for(i in 1:mcmc.sample){
  array.CHOH.p.pred.date[,i] <- plogis(results$sims.list$mu.b0[i,2] + results$sims.list$mu.b1[i,2]*date.pred + results$sims.list$mu.b2[i,2]*date.pred^2)
}
LPB_CHOH <- apply(array.CHOH.p.pred.date, 1, quantile, probs=0.025)
UPB_CHOH <- apply(array.CHOH.p.pred.date, 1, quantile, probs=0.975)
LPB50_CHOH <- apply(array.CHOH.p.pred.date, 1, quantile, probs=0.25)
UPB50_CHOH <- apply(array.CHOH.p.pred.date, 1, quantile, probs=0.75)  

#for park-level estimates
array.park.p.pred.date <- array(NA, dim=c(length(date.pred), R) )
for(i in 1:R){
  array.park.p.pred.date[,i] <- plogis(results$mean$mu.b0[i] + results$mean$mu.b1[i]*date.pred + results$mean$mu.b2[i]*date.pred^2)
}

#for species-level estimates
det.int <- results$mean$b0[1:15,2]
det.sl1 <- results$mean$b1[1:15,2]
det.sl2 <- results$mean$b2[1:15,2]

array.spp.p.pred.date <- array(NA, dim=c(length(date.pred), I) )
for(i in 1:(I)){
  array.spp.p.pred.date[,i] <- plogis(det.int[i] + det.sl1[i]*date.pred + det.sl2[i]*date.pred^2)
}


##
#### Plots sepcies detection for each park
##


globalPs <- data.frame(day = unstd.date.pred, 
                       pred = p.pred.date, 
                       L = LPB, 
                       U = UPB,
                       L50 = LPB50,
                       U50 = UPB50)
parkPs <- data.frame(day = rep(unstd.date.pred, times = R),
                     pred = as.vector(array.park.p.pred.date),
                     park = rep(Parks$park, each = length(unstd.date.pred)),
                     Park = rep(Parks$Park, each = length(unstd.date.pred))
)
d1 <- parkPs[parkPs$park == 1,]
d2 <- parkPs[parkPs$park == 2,]
d3 <- parkPs[parkPs$park == 3,]
d4 <- parkPs[parkPs$park == 4,]
d5 <- parkPs[parkPs$park == 5,]
d6 <- parkPs[parkPs$park == 6,]
d7 <- parkPs[parkPs$park == 7,]
d8 <- parkPs[parkPs$park == 8,]
d9 <- parkPs[parkPs$park == 9,]


#https://stackoverflow.com/questions/17148679/construct-a-manual-legend-for-a-complicated-plot
#http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
pppp <- ggplot() +
  scale_x_continuous(breaks = c(91, 121, 152, 182) ,labels=c('Apr 1','May 1','Jun 1','Jul 1')) +
  ylab('Detection Probability') + xlab('') + ylim(c(0,1)) + 
  theme_bw() +
  theme(axis.text.x=element_text(color='black', size=8) , axis.title.x=element_text(size=10),
        axis.text.y=element_text(color='black', size=8), axis.title.y=element_text(size=10),
        legend.text=element_text(color='black', size=8)) +
  geom_ribbon(data = globalPs, aes(x = day, y = pred, ymin = L, ymax = U), fill = 'grey', alpha=0.3) +
  geom_ribbon(data = globalPs, aes(x = day, y = pred, ymin = L50, ymax = U50), fill = 'grey', alpha=0.5) +
  geom_smooth(data = globalPs, aes(x = day, y = pred, color = 'NCRN', linetype = 'NCRN'), se = F) +
  geom_smooth(aes(x = day, y = pred, color = 'CATO', linetype = 'CATO'), se = F, d1) +
  geom_smooth(aes(x = day, y = pred, color = 'CHOH', linetype = 'CHOH'), se = F, d2) +
  geom_smooth(aes(x = day, y = pred, color = 'GWMP', linetype = 'GWMP'), se = F, d3) +
  geom_smooth(aes(x = day, y = pred, color = 'HAFE', linetype = 'HAFE'), se = F, d4) +
  geom_smooth(aes(x = day, y = pred, color = 'MANA', linetype = 'MANA'), se = F, d5) +
  geom_smooth(aes(x = day, y = pred, color = 'MONO', linetype = 'MONO'), se = F, d6) +
  geom_smooth(aes(x = day, y = pred, color = 'NACE', linetype = 'NACE'), se = F, d7) +
  geom_smooth(aes(x = day, y = pred, color = 'PRWI', linetype = 'PRWI'), se = F, d8) +
  geom_smooth(aes(x = day, y = pred, color = 'ROCR', linetype = 'ROCR'), se = F, d9) +
  scale_colour_manual(name='',
                      values=c('CATO' = '#e41a1c','CHOH' = '#377eb8','GWMP' = '#4daf4a','HAFE' = '#984ea3','MANA' = '#ff7f00','MONO' = '#1a9850','NACE' = '#a65628','PRWI' = '#f781bf','ROCR' = '#878787','NCRN' = 'black'),
                      breaks=c('CATO','CHOH','GWMP','HAFE','MANA','MONO','NACE','PRWI','ROCR','NCRN')
  ) +
  scale_linetype_manual(name='',
                        values=c('CATO' = 'dotdash','CHOH' = 'dotdash','GWMP' = 'dotdash','HAFE' = 'dotdash','MANA' = 'dotdash','MONO' = 'dotdash','NACE' = 'dotdash','PRWI' = 'dotdash','ROCR' = 'dotdash','NCRN' = 'solid'),
                        breaks=c('CATO','CHOH','GWMP','HAFE','MANA','MONO','NACE','PRWI','ROCR','NCRN')
  )


##
#### Plotting species detection rates at CHOH
##

chohPs <- data.frame(day = unstd.date.pred, 
                     pred = CHOH.p.pred.date, 
                     L = LPB_CHOH, 
                     U = UPB_CHOH,
                     L50 = LPB50_CHOH,
                     U50 = UPB50_CHOH)
sppPs <- data.frame(day = rep(unstd.date.pred, times = I),
                    pred = as.vector(array.spp.p.pred.date),
                    spp = rep(Spp$spp, each = length(unstd.date.pred)),
                    Spp = rep(Spp$Spp, each = length(unstd.date.pred))
)
d1 <- sppPs[sppPs$spp == 1,]
d2 <- sppPs[sppPs$spp == 2,]
d3 <- sppPs[sppPs$spp == 3,]
d4 <- sppPs[sppPs$spp == 4,]
d5 <- sppPs[sppPs$spp == 5,]
d6 <- sppPs[sppPs$spp == 6,]
d7 <- sppPs[sppPs$spp == 7,]
d8 <- sppPs[sppPs$spp == 8,]
d9 <- sppPs[sppPs$spp == 9,]
d10 <- sppPs[sppPs$spp == 10,]
d11 <- sppPs[sppPs$spp == 11,]
d12 <- sppPs[sppPs$spp == 12,]
d13 <- sppPs[sppPs$spp == 13,]
d14 <- sppPs[sppPs$spp == 14,]


#https://stackoverflow.com/questions/17148679/construct-a-manual-legend-for-a-complicated-plot
#http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
ppsp <- ggplot() +
  scale_x_continuous(breaks = c(91, 121, 152, 182) ,labels=c('Apr 1','May 1','Jun 1','Jul 1')) +
  ylab('Detection Probability') + xlab('') + ylim(c(0,1)) + 
  theme_bw() +
  theme(axis.text.x=element_text(color='black', size=8) , axis.title.x=element_text(size=10),
        axis.text.y=element_text(color='black', size=8), axis.title.y=element_text(size=10),
        legend.text=element_text(color='black', size=8)) +
  geom_ribbon(data = chohPs, aes(x = day, y = pred, ymin = L, ymax = U), fill = 'grey', alpha=0.3) +
  geom_ribbon(data = chohPs, aes(x = day, y = pred, ymin = L50, ymax = U50), fill = 'grey', alpha=0.5) +
  geom_smooth(data = chohPs, aes(x = day, y = pred, color = 'CHOH', linetype = 'CHOH'), se = F) +
  geom_smooth(aes(x = day, y = pred, color = 'ACRE', linetype = 'ACRE'), se = F, d1) +   
  geom_smooth(aes(x = day, y = pred, color = 'AMAC', linetype = 'AMAC'), se = F, d2) +
  geom_smooth(aes(x = day, y = pred, color = 'AOPA', linetype = 'AOPA'), se = F, d3) +
  geom_smooth(aes(x = day, y = pred, color = 'BUFO', linetype = 'BUFO'), se = F, d4) +
  geom_smooth(aes(x = day, y = pred, color = 'HSCU', linetype = 'HSCU'), se = F, d5) +
  geom_smooth(aes(x = day, y = pred, color = 'HYLA', linetype = 'HYLA'), se = F, d6) +
  geom_smooth(aes(x = day, y = pred, color = 'NVIR', linetype = 'NVIR'), se = F, d7) +
  geom_smooth(aes(x = day, y = pred, color = 'PCRU', linetype = 'PCRU'), se = F, d8) +
  geom_smooth(aes(x = day, y = pred, color = 'PFER', linetype = 'PFER'), se = F, d9) +
  geom_smooth(aes(x = day, y = pred, color = 'RCAT', linetype = 'RCAT'), se = F, d10) +
  geom_smooth(aes(x = day, y = pred, color = 'RCLA', linetype = 'RCLA'), se = F, d11) +
  geom_smooth(aes(x = day, y = pred, color = 'RPAL', linetype = 'RPAL'), se = F, d12) +
  geom_smooth(aes(x = day, y = pred, color = 'RSPH', linetype = 'RSPH'), se = F, d13) +
  geom_smooth(aes(x = day, y = pred, color = 'RSYL', linetype = 'RSYL'), se = F, d14) +
  scale_colour_manual(name='',
                      values=c('ACRE' = '#e41a1c','AMAC' = '#377eb8','AOPA' = '#4daf4a','BUFO' = '#984ea3','HSCU' = '#ff7f00',
                               'HYLA' = '#1a9850','NVIR' = '#a65628','PCRU' = '#f781bf','PFER' = '#878787','RCAT' = '#e41a1c',
                               'RCLA' = '#377eb8','RPAL' = '#4daf4a','RSPH' = '#984ea3','RSYL' = '#ff7f00','CHOH' = 'black'),
                      breaks=c('ACRE','AMAC','AOPA','BUFO','HSCU','HYLA','PCRU','PFER','RCAT','RCLA','RPAL','RSPH','RSYL','MANA','CHOH')
  ) +
  scale_linetype_manual(name='',
                        values=c('ACRE' = 'dashed','AMAC' = 'dashed','AOPA' = 'dashed','BUFO' = 'dashed','HSCU' = 'dashed',
                                 'HYLA' = 'dashed','NVIR' = 'dashed','PCRU' = 'dashed','PFER' = 'dashed','RCAT' = 'dotted',
                                 'RCLA' = 'dotted','RPAL' = 'dotted','RSPH' = 'dotted','RSYL' = 'dotted','CHOH' = 'solid'),
                        breaks=c('ACRE','AMAC','AOPA','BUFO','HSCU','HYLA','PCRU','PFER','RCAT','RCLA','RPAL','RSPH','RSYL','MANA','CHOH')
  )



## ALL TOGETHER TO MAKE PUBLICATION QUALITY FIGURE
#Set image specifications
png('Figure4.png', width = 5, height = 7, units = 'in', res = 450)
#Make plot
ggarrange(pppp, ppsp, ncol = 1, nrow = 2, labels=c('(a)','(b)'), font.label = list(size = 10), vjust = 1)  
#Save plot
dev.off()

