#results for table 1
#linear vs quadratic model



#hodg from the package KMsurv, sample size 43
data(hodg,package="KMsurv")
set.seed(12379)
regr_ind(hodg[,c(1:2,5:6)],hodg[,3],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 379.211400   6.183534 445.683189  11.160090 415.589452   6.480354

#including quadratic terms in the model

set.seed(12379)
regr_ind(quadr(hodg[,c(1:2,5:6)]),hodg[,3],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 354.332000   3.411531 445.683189  11.160090 444.782556   8.424078

#kidrecurr from the package KMsurv, sample size 38
data(kidrecurr,package="KMsurv")
set.seed(12383);regr_ind(kidrecurr[,6:10],kidrecurr[,2],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1]  82.566500   4.248058  89.520824   4.918722 168.381579  10.774345

#including quadratic terms in the model
set.seed(12383);
regr_ind(quadr(as.matrix(kidrecurr[,6:10])),kidrecurr[,2],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1]  80.848250   3.560264  89.520824   4.918722 169.896930  10.645383


#kidrecurr from the package KMsurv, sample size 38
data(kidrecurr,package="KMsurv")
set.seed(12383);regr_ind(kidrecurr[,6:10],kidrecurr[,4],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1]  92.706500   3.273108 100.566721   3.743902 104.856589   4.796471


#including quadratic terms in the model
set.seed(12383);
regr_ind(quadr(as.matrix(kidrecurr[,6:10])),kidrecurr[,4],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1]  85.975500   1.645075 100.566721   3.743902 105.826923   4.854650


#hodg from the package KMsurv, sample size 43
data(hodg,package="KMsurv")
set.seed(12379)
regr_ind(hodg[,c(1:2,5:6)],hodg[,3],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 379.211400   6.183534 445.683189  11.160090 415.589452   6.480354


set.seed(12379)
regr_ind(quadr(hodg[,c(1:2,5:6)]),hodg[,3],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 354.332000   3.411531 445.683189  11.160090 444.782556   8.424078

#myeloid from package survival

#removing NA from the dataset
datai<-myeloid[,5:7]
col=dim(datai)[2]
row=dim(datai)[1]
ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}


set.seed(1234)
regr_ind(cbind(ifelse(myeloid[ind,2]=="A",1,0),myeloid[ind,5:6]),myeloid[ind,7],crv=1000,mode='linear')
#[1] 137.7483571   0.5939945 193.8927681   0.9112561 142.6912143   0.6187424

#including quadratic terms in the model
set.seed(1234)
regr_ind(quadr(cbind(ifelse(myeloid[ind,2]=="A",1,0),myeloid[ind,5:6])),myeloid[ind,7],crv=1000,mode='linear')
#[1] 128.7018571   0.5349943 193.8927681   0.9112561 180.4718571   0.6851034


#snails from package MASS, sample size 96

data(snails, package="MASS")
set.seed(100)
regr_ind(snails[,1:4],snails[,5],crv=1000,mode='linear')

#[1] 1.5423000 0.5506663 2.9862163 0.6340696 1.6117000 0.5934123

#including quadratic terms in the model

set.seed(100)
regr_ind(cbind(snails[,1],quadr(snails[,2:4])),snails[,5],crv=1000,mode='linear')

#[1] 1.2275000 0.3503925 2.9862163 0.6340696 1.2423000 0.4235512

#environmental from the package lattice, sample size 111

data(environmental,package="lattice")

set.seed(1200)
regr_ind(environmental[,2:4],environmental[,1],1000,mode='linear')

#first line of the output
#[1] 16.7366667  0.6984487 27.0621128  1.5942608

#including quadratic terms in the model
set.seed(1200)
regr_ind(quadr(environmental[,2:4]),environmental[,1],1000,mode='linear')

#first line of the output
#[1] 14.027500  0.469646 27.062113  1.594261

#Phenobarb from the package nlme, sample size 744
data(Phenobarb,package="nlme")

#removing the NA values, reducing the sample size to 589
datai<-Phenobarb[,2:6]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

set.seed(168)
regr_ind(Phenobarb[ind,2:5],Phenobarb[ind,6],1000,mode='linear')

#first line of the output
#[1] 4.7692434 0.7259111 5.1629820 0.9969036

#including the quadratic terms
set.seed(168)
regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,6],1000,mode='linear')

#first line of the output
#[1] 3.9180856 0.6195895 5.1629820 0.9969036

#Wheat from the package nlme, sample size 48
data(Wheat,package="nlme")

set.seed(175)
regr_ind(cbind(as.numeric(Wheat[,1]),Wheat[,2:3]),Wheat[,4],1000,mode='linear')

#first line of the output
#[1] 2.0589174 0.2859348 3.2210669 0.5015642

#including quadratic terms
set.seed(175)
regr_ind(quadr(cbind(as.numeric(levels(Wheat[,1])[Wheat[,1]]),Wheat[,2:3])),Wheat[,4],1000,mode='linear')

#first line of the output
#[1] 1.1023036 0.1572396 3.2210669 0.5015642


#Wheat2 from the package nlme, sample size 224
data(Wheat2,package="nlme")

set.seed(177)
regr_ind(cbind(as.numeric(Wheat2[,1]),Wheat2[,c(2,4:5)]),Wheat2[,3],1000,mode='linear')

#first line of the output
#[1] 4.5363964 0.4078296 5.4164956 0.5704332

#including quadratic terms
set.seed(177)
regr_ind(cbind(quadr(cbind(as.numeric(levels(Wheat2[,1])[Wheat2[,1]]),Wheat2[,4:5])),Wheat2[,2]),Wheat2[,3],1000,mode='linear',parallel=TRUE,cores=7)

#first line of the output
#[1] 4.0709993 0.3322718 5.3790160 0.5729086

#rock from the package datasets, sample size 48
set.seed(109)
regr_ind(rock[,1:3],rock[,4],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 183.968329   2.151264 407.502100   9.241256

#the same dataset including quadratic terms
set.seed(109)
regr_ind(quadr(rock[,1:3]),rock[,4],1000,cutoff=0.5,mode='linear')

#first line of output
#[1] 168.512198   1.319686 407.502100   9.241256

#channing from the package boot, sample size 462


data(channing,package="boot")

#maximising accuracy
set.seed(201)
regr_ind(channing[,1:3],channing[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.6425319 0.6212979

#maximising AUROC

set.seed(202)
regr_ind(channing[,1:3],channing[,5],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.6696496

#including the quadratic terms
#maximising accuracy
set.seed(201)
regr_ind(cbind(channing[,1],quadr(channing[,2:3])),channing[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7066383 0.6212979

#maximising AUROC
set.seed(202)
regr_ind(cbind(channing[,1],quadr(channing[,2:3])),channing[,5],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.7853932


#nodal from the package boot, sample size 53


data(nodal,package="boot")

#maximising accuracy
set.seed(205)
regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1)
#first line of the output
#[[1] 0.7375 0.6190

#maximising AUROC

set.seed(206)
regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',objfun='roc',maxx=1)
#first line of the output
#[1] 0.6910533

#including the quadratic terms
#maximising accuracy
set.seed(205)
regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1)
#first line of the output
#[1] 0.777 0.619

#maximising AUROC
set.seed(206)
regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',objfun='roc',maxx=1)
#first line of the output
#[1] 0.7261041

#bladder from the package survival, sample size 81


data(bladder,package="survival")

#maximising accuracy
set.seed(232)
regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7227941 0.6706471 

#maximising AUROC

set.seed(233)
regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='roc')
#[1] 0.7714052 

#including the quadratic terms
#maximising accuracy
set.seed(232)
regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7662853 0.6706471

#maximising AUROC
set.seed(233)
regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.8127092


data(aSAH,package="pROC")

#maximising accuracy
set.seed(213)
regr_ind(cbind(as.numeric(aSAH[,3]),as.numeric(aSAH[,5]),aSAH[,c(4,6:7)]),aSAH[,2],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.8525641 0.6330000
#maximising AUROC

set.seed(214)
regr_ind(cbind(as.numeric(aSAH[,3]),as.numeric(aSAH[,5]),aSAH[,c(4,6:7)]),aSAH[,2],1000,cutoff=0.5,mode='binary',objfun='roc')
#[1] 0.8477025

#including the quadratic terms
#maximising accuracy
set.seed(213)
regr_ind(quadr(cbind(as.numeric(aSAH[,3]),as.numeric(aSAH[,5]),aSAH[,c(4,6:7)])),aSAH[,2],1000,cutoff=0.5,mode='binary')#first line of the output
#[1] 0.9166667 0.6330000

#maximising AUROC
set.seed(214)
regr_ind(cbind(as.numeric(aSAH[,3]),as.numeric(aSAH[,5]),aSAH[,c(4,6:7)]),aSAH[,2],1000,cutoff=0.5,mode='binary',objfun='roc')#first line of the output
#


#painters from the package MASS, sample size 54

data(painters, package="MASS")

#merging outcomes in order to be able to fit regressions 

p<-array(0,length(painters[,5]))

p[painters[,5]=="A"]=0
p[painters[,5]=="B"]=0
p[painters[,5]=="C"]=0
p[painters[,5]=="D"]=1
p[painters[,5]=="E"]=1
p[painters[,5]=="F"]=2
p[painters[,5]=="G"]=2
p[painters[,5]=="H"]=2

set.seed(259)
regr_ind(painters[,1:4],p,1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.5468333 0.4015000

#quadratic terms

set.seed(259)
regr_ind(quadr(painters[,1:4]),p,1000,cutoff=0.5,mode='multin')

#first line of the output
#[1] 0.5670 0.4015



