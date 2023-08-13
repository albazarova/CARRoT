
path1="carrot_functions_mod_si_las.R"
path2="carrot_functions_rsq.r"

  
source("carrot_functions_mod_si_las_ada.R")




library(car)
library(afex)

#Note that the results are subject to a partition which is different every time. 
#Therefore the values obtained by running this code might give values which are slightly different fro those givent in Tables 2-3. 
#However we anticipate that that difference will have an order of magnitude 10^{-2}
#Also the ratio between predictive powers obtained by different methods would stay the same up to the second-third decimal place

#To ensure reproducibility we did not use parallel toolbox, however it would normally significantly speed up the process

#snails from package MASS, sample size 96
name<-"snails"
name_p<-"MASS"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(snails[,1:4])

data(snails, package="MASS")



set.seed(100)
c<-regr_ind(snails[,1:4],snails[,5],crv=1000,mode='linear')


#[1] 1.5423000 0.5506663 2.9862163 0.6340696 1.6117000 0.5934123


#uis from the package quantreg, sample size 575

name<-"uis"
name_p<-"quantreg"
name_s<-"lin"
ou<-10
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(uis[,2:9])

data(uis,package="quantreg")

source("carrot_functions_mod_si_las.R")

set.seed(12399)
c<-regr_ind(uis[,2:9],uis[,10],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 53.974224 1.577012 57.734791 1.624488 55.402586 1.602884



#PBG from the package nlme, sample size 60


data(PBG,package="nlme")


name<-"PBG"
name_p<-"nlme"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(PBG[,2:5])


source("carrot_functions_mod_si_las.R")

set.seed(12399)
c<-regr_ind(PBG[,2:5],PBG[,1],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 4.528927 1.048533 10.163057 3.559833 4.854390 1.095415



#Rabbit from the package MASS, sample size 60
data(Rabbit,package="MASS")

name<-"Rabbit"
name_p<-"MASS"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Rabbit[,2:5])

source("carrot_functions_mod_si_las.R")


set.seed(12389)
c<-regr_ind(Rabbit[,2:5],Rabbit[,1],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 4.397715 1.065038 10.197562 3.654520 4.782833 1.110297



#kidrecurr from the package KMsurv, sample size 38
data(kidrecurr,package="KMsurv")

name<-"kidrecurr"
name_p<-"KMsurv"
name_s<-"lin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(kidrecurr[,6:10])

source("carrot_functions_mod_si_las.R")



set.seed(12383);
c<-regr_ind(kidrecurr[,6:10],kidrecurr[,2],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 82.566500 4.248058 89.520824 4.918722 168.381579 10.774345



#kidrecurr from the package KMsurv, sample size 38, different dependent variable
data(kidrecurr,package="KMsurv")

name<-"kidrecurr"
name_p<-"KMsurv"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(kidrecurr[,6:10])

source("carrot_functions_mod_si_las.R")


set.seed(12383);

c<-regr_ind(kidrecurr[,6:10],kidrecurr[,4],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 92.706500 3.273108 100.566721 3.743902 104.856589 4.796471



#hodg from the package KMsurv, sample size 43
data(hodg,package="KMsurv")

name<-"hodg"
name_p<-"KMsurv"
name_s<-"lin"
ou<-3
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(hodg[,c(1:2,5:6)])

source("carrot_functions_mod_si_las.R")

set.seed(12379)
c<-regr_ind(hodg[,c(1:2,5:6)],hodg[,3],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 379.211400   6.183534 445.683189  11.160090 415.589452   6.480354




#bmt from the package KMsurv, sample size 137
data(bmt,package="KMsurv")

name<-"bmt"
name_p<-"KMsurv"
name_s<-"lin"
ou<-2
name_o<-paste0("names(",nameƒ,")[",ou,"]")
name_d<-dim(bmt[,c(8,10,12:22)])

source("carrot_functions_mod_si_las.R")


set.seed(12369)
c<-regr_ind(bmt[,c(8,10,12:22)],bmt[,2],crv=10,mode='linear',cutoff=0.5)

#first line of the output
#[1] 550.016571 2.837227 638.521065 11.963174 606.219000 4.473144



#glioma from the package exactRankTests, sample size 37, using 23 patients who died
data(glioma,package="exactRankTests")
ind<-which(glioma$Cens==1)

name<-"glioma"
name_p<-"exactRankTests"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(glioma[ind,c(2:4,7)])

#source("carrot_functions_mod_si_las.R")
source(path1)

set.seed(2345)
c<-regr_ind(glioma[ind,c(2:4,7)],glioma[ind,5],crv=1000,mode='linear',cutoff=0.5)

#first line of the output
#[1] 7.7243333 0.4729086 9.9333000 0.7271526 8.3730000 0.4861305

#source("/Users/alinabazarova/Dropbox/carrot_functions_new.r")

#bfeed from the package KMsurv, sample size 892, using 927 patients who completed breastfeeding
data(bfeed,package="KMsurv")

ind<-which(bfeed$delta==1)

name<-"bfeed"
name_p<-"KMsurv"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(bfeed[ind,3:9])

source(path1)
set.seed(12359)
c<-regr_ind(bfeed[ind,3:9],bfeed[ind,1],crv=1000,mode='linear',cutoff=0.5)

#first line of the output
#[1] 12.721856 2.736685 13.226956 2.893800 12.833922 2.783218



#esoph from the package datasets, sample size 88


name<-"esoph"
name_p<-"datasets"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(esoph[,c(1:3,5)])

source(path1)

set.seed(99)
c<-regr_ind(esoph[,c(1:3,5)],esoph[,4],crv=1000,mode='linear')



#first line of the output
#[1] 1.3468889 0.5516158 2.0594430 0.6161480 1.3716667 0.7364119

#WeightLoss from the package carData, sample size 34

data(WeightLoss,package="carData")

name<-"WeightLoss"
name_p<-"carData"
name_s<-"lin"
ou<-7
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(WeightLoss[,1:6])


source(path1)

set.seed(200)
c<-regr_ind(WeightLoss[,1:6],WeightLoss[,7],crv=1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 0.95175000 0.06323496 1.94850000 0.12999509 1.03550000 0.06888105



#myeloid from package survival

#removing NA from the dataset

data(myeloid,package="survival")

datai<-myeloid[,5:7]
col=dim(datai)[2]
row=dim(datai)[1]
ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}


name<-"myeloid"
name_p<-"survival"
name_s<-"lin"
ou<-7
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(ifelse(myeloid[ind,2]=="A",1,0),myeloid[ind,5:6]))


source(path1)


set.seed(1234)
c<-regr_ind(cbind(ifelse(myeloid[ind,2]=="A",1,0),myeloid[ind,5:6]),myeloid[ind,7],crv=1000,mode='linear')


#Leinhardt from package carData

data(Leinhardt,package="carData")

#removing NA from the dataset

datai<-Leinhardt[,1:4]
col=dim(datai)[2]
row=dim(datai)[1]
ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"Leinhardt"
name_p<-"carData"
name_s<-"lin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Leinhardt[ind,c(1,3:4)])

source(path1)


set.seed(12349)
c<-regr_ind(Leinhardt[ind,c(1,3:4)],Leinhardt[ind,2],crv=1000,mode='linear',cutoff=0.5)
#[1] 43.8677890 0.8345628 64.5493072 1.6867396 46.5616143 0.8350713


name<-"CO2"
name_p<-"carData"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(CO2[,1:4])

source(path1)

set.seed(100)
c<-regr_ind(CO2[,1:4],CO2[,5],1000,mode='linear')





#OrchardSprays from the package datasets, sample size 64
name<-"OrchardSprays"
name_p<-"datasets"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(OrchardSprays[,2:3])

source(path1)


set.seed(101)
c<-regr_ind(OrchardSprays[,2:3],OrchardSprays[,1],1000,mode='linear')
#first line of output
#[1] 31.314286 2.673523 31.013088 2.720411 37.264808 3.258941




#Seatbelts from the package datasets, sample size 192
name<-"Seatbelts"
name_p<-"datasets"
name_s<-"lin"
ou<-8
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Seatbelts[,2:6])

source(path1)



set.seed(102)
c<-regr_ind(cbind(Seatbelts[,2:6],Seatbelts[,8]),Seatbelts[,1],1000,mode='linear')

#first line of output
#[1] 9.18730000 0.07834161 20.40541047 0.17300658 9.32910000 0.07952680





#Theoph from the package datasets, sample size 132
name<-"Theoph"
name_p<-"datasets"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Theoph[,2:4])

source(path1)

set.seed(103)
c<-regr_ind(Theoph[,2:4],Theoph[,5],1000,mode='linear')


#airquality from the package datasets, sample size of those do not have NA in the outcome variable 'Ozone' 116
name<-"airquality"
name_p<-"datasets"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(airquality[which((is.na(airquality[,1])==FALSE)&(is.na(airquality[,2])==FALSE)),2:5])

source(path1)

set.seed(104)
c<-regr_ind(airquality[which((is.na(airquality[,1])==FALSE)&(is.na(airquality[,2])==FALSE)),2:5],airquality[which((is.na(airquality[,1])==FALSE)&(is.na(airquality[,2])==FALSE)),1],1000,mode='linear')



#mtcars from the package datasets, sample size 32

name<-"mtcars"
name_p<-"datasets"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(mtcars[,2:11])

source(path1)

set.seed(106)
c<-regr_ind(mtcars[,2:11],mtcars[,1],1000,cutoff=0.5,mode='linear')


#npk from the package datasets, sample size 24

#treating numeric binary factor variables as numbers
name<-"npk"
name_p<-"datasets"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(npk[,1],apply(as.matrix.noquote(npk),2,as.numeric)[,2:4]))

source(path1)

set.seed(107)
c<-regr_ind(cbind(npk[,1],apply(as.matrix.noquote(npk),2,as.numeric)[,2:4]),npk[,5],1000,cutoff=0.5,mode='linear')






#rock from the package datasets, sample size 48
name<-"rock"
name_p<-"datasets"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(rock[,1:3])

source(path1)

set.seed(109)
c<-regr_ind(rock[,1:3],rock[,4],1000,cutoff=0.5,mode='linear')



#the same dataset including quadratic terms
name<-"quadr(rock)"
name_p<-"datasets"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quadr(rock[,1:3]))

source(path1)

set.seed(110)
c<-regr_ind(quadr(rock[,1:3]),rock[,4],1000,cutoff=0.5,mode='linear')

#first line of output
#[1] 165.266168 1.308128 405.466431 9.417221 194.435099 2.335413



#stackloss from the package datasets, sample size 21
name<-"stackloss"
name_p<-"datasets"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(stackloss[,1:3])

source(path1)

set.seed(111)
c<-regr_ind(stackloss[,1:3],stackloss[,4],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 3.0470000 0.1816492 7.9975556 0.5291671 2.9973333 0.1682876


#swiss from the package datasets, sample size 47
name<-"swiss"
name_p<-"datasets"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(swiss[,2:6])

source(path1)

set.seed(113)
c<-regr_ind(swiss[,2:6],swiss[,1],1000,cutoff=0.5,mode='linear')


#amis from the package boots, sample size 8437
data(amis,package="boot")

name<-"amis"
name_p<-"boot"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(amis[,2:4])

source(path1)

set.seed(114)
c<-regr_ind(amis[,2:4],amis[,1],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 4.8559976 0.1329417 5.1433365 0.1409071 4.8658353 0.1331930


#cane from the package boots, sample size 180
data(cane,package="boot")

name<-"cane"
name_p<-"boot"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cane[,1:3])

source(path1)

set.seed(115)
c<-regr_ind(cbind(cane[,1:3],cane[,5]),cane[,4],1000,cutoff=0.5,mode='linear')




#Tablerock from the package BSDA, sample size 719
data(Tablrock,package="BSDA")

#removing NA values from the relevant variables and reducing the sample size to 620
datai<-cbind(Tablrock$ozone,Tablrock$tmp,Tablrock$wd,Tablrock$ws,Tablrock$amb,Tablrock$dew,Tablrock$so2,Tablrock$no,Tablrock$no2,Tablrock$nox,Tablrock$co)

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"Tablrock"
name_p<-"BSDA"
name_s<-"lin"
ou<-which(names(Tablrock)=="ozone")
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(Tablrock$tmp[ind],Tablrock$wd[ind],Tablrock$ws[ind],Tablrock$amb[ind],Tablrock$dew[ind],Tablrock$so2[ind],Tablrock$no[ind],Tablrock$no2[ind],Tablrock$nox[ind],Tablrock$co[ind]))

source(path1)

set.seed(117)
c<-regr_ind(cbind(Tablrock$tmp[ind],Tablrock$wd[ind],Tablrock$ws[ind],Tablrock$amb[ind],Tablrock$dew[ind],Tablrock$so2[ind],Tablrock$no[ind],Tablrock$no2[ind],Tablrock$nox[ind],Tablrock$co[ind]),Tablrock$ozone[ind],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 5.3896613 0.4448215 11.7665488 1.1620248 5.4149032 0.4467527



#Track from the package BSDA, sample size 55
data(Track,package="BSDA")

name<-"Track"
name_p<-"BSDA"
name_s<-"lin"
ou<-which(names(Track)=="marathon")
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(Track$'100m',Track$'200m',Track$'400m',Track$'800m',Track$'1500m',Track$'3000m'))

source(path1)

set.seed(118)
c<-regr_ind(cbind(Track$'100m',Track$'200m',Track$'400m',Track$'800m',Track$'1500m',Track$'3000m'),Track$marathon,1000,mode='linear')

#first line of the output
#[1] 10.42125235 0.05822114 21.47014354 0.11674153 11.88004305 0.06570432


#Undergrad from the package BSDA, sample size 100
data(Undergrad,package="BSDA")

name<-"Undergrad"
name_p<-"BSDA"
name_s<-"lin"
ou<-which(names(Undergrad)=="drops")
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(Undergrad$gender,Undergrad$class,Undergrad$gpa,Undergrad$sat))

source(path1)

set.seed(119)
c<-regr_ind(cbind(Undergrad$gender,Undergrad$class,Undergrad$gpa,Undergrad$sat),Undergrad$drops,1000,mode='linear')

#first line of the output
#[1] 0.9173000 0.4306075 1.0789600 0.4538941 0.9185000 0.4311526



#Wins from the package BSDA, sample size 12
data(Wins,package="BSDA")

name<-"Wins"
name_p<-"BSDA"
name_s<-"lin"
ou<-which(names(Wins)=="wins")
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(Wins$batavg,Wins$rbi,Wins$stole,Wins$strkout,Wins$caught,Wins$errors,Wins$era))

source(path1)

set.seed(120)
c<-regr_ind(cbind(Wins$batavg,Wins$rbi,Wins$stole,Wins$strkout,Wins$caught,Wins$errors,Wins$era),Wins$wins,1000,mode='linear')

#first line of the output
#[1] 6.17750000 0.07962313 8.52790000 0.10816740 6.22368421 0.08066405


#plantTraits from the package cluster, sample size 136
data(plantTraits,package="cluster")

#getting rid of all NA values reducing the sample size to 78

datai<-plantTraits[,1:10]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"plantTraits"
name_p<-"cluster"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(plantTraits[ind,2:10])

source(path1)

set.seed(121)
c<-regr_ind(plantTraits[ind,2:10],plantTraits[ind,1],1000,mode='linear')

#first line of the output
#[1] 56.85221 15.11273 97.24468 142.92252 91.38387 144.36665



#using different predictors from the same dataset to predict the same outcome

#remove NA values reducing the initial sample size to 97

datai<-plantTraits[,c(1,11:20)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}


name<-"plantTraits"
name_p<-"datasets"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(plantTraits[ind,11:20])

source(path1)

set.seed(122)
c<-regr_ind(plantTraits[ind,11:20],plantTraits[ind,1],1000,mode='linear')

#first line of the output
#[1] 77.98142 104.19454 112.25366 155.15367 85.18822 115.52875


#algae from the package DMwR, sample size 200
data(algae,package="DMwR2")

datai<-algae[,1:11]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"algae"
name_p<-"DMwR2"
name_s<-"lin"
ou<-12
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(algae[ind,1:11])

source(path1)

set.seed(123)
c<-regr_ind(cbind(algae[ind,1:3],unlist(algae[ind,4]),unlist(algae[ind,5]),unlist(algae[ind,6]),unlist(algae[ind,7]),unlist(algae[ind,8]),unlist(algae[ind,9]),unlist(algae[ind,10]),unlist(algae[ind,11])),unlist(algae[ind,12]),1000,mode='linear')

#first line of the output
#[1] 6.17750000 0.07962313 8.52790000 0.10816740 6.22368421 0.08066405


#the same dataset with a different predictor

name<-"algae"
name_p<-"DMwR2"
name_s<-"lin"
ou<-13
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(algae[ind,1:11])

source(path1)

set.seed(124)
c<-regr_ind(cbind(algae[ind,1:3],unlist(algae[ind,4]),unlist(algae[ind,5]),unlist(algae[ind,6]),unlist(algae[ind,7]),unlist(algae[ind,8]),unlist(algae[ind,9]),unlist(algae[ind,10]),unlist(algae[ind,11])),unlist(algae[,13]),1000,mode='linear')



#the same dataset with a different predictor
name<-"algae"
name_p<-"DMwR2"
name_s<-"lin"
ou<-14
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(algae[ind,1:11])

source(path1)

set.seed(125)
c<-regr_ind(cbind(algae[ind,1:3],unlist(algae[ind,4]),unlist(algae[ind,5]),unlist(algae[ind,6]),unlist(algae[ind,7]),unlist(algae[ind,8]),unlist(algae[ind,9]),unlist(algae[ind,10]),unlist(algae[ind,11])),unlist(algae[,14]),1000,mode='linear')

#first line of the output
#[1] 4.473589  Inf 4.754987  NaN 4.495800  NaN



#the same dataset with a different predictor
name<-"algae"
name_p<-"DMwR2"
name_s<-"lin"
ou<-15
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(algae[ind,1:11])

source(path1)

set.seed(126)
c<-regr_ind(cbind(algae[ind,1:3],unlist(algae[ind,4]),unlist(algae[ind,5]),unlist(algae[ind,6]),unlist(algae[ind,7]),unlist(algae[ind,8]),unlist(algae[ind,9]),unlist(algae[ind,10]),unlist(algae[ind,11])),unlist(algae[,15]),1000,mode='linear')

#first line of the output
#[1] 1.807220  Inf 2.350031  NaN 2.267672  NaN



#the same dataset with a different predictor
name<-"algae"
name_p<-"DMwR2"
name_s<-"lin"
ou<-16
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(algae[ind,1:11])

source(path1)

set.seed(127)
c<-regr_ind(cbind(algae[ind,1:3],unlist(algae[ind,4]),unlist(algae[ind,5]),unlist(algae[ind,6]),unlist(algae[ind,7]),unlist(algae[ind,8]),unlist(algae[ind,9]),unlist(algae[ind,10]),unlist(algae[ind,11])),unlist(algae[,16]),1000,mode='linear')

#first line of the output
#4.696981  Inf 5.312394  NaN 4.926308  NaN



#economics from the package ggplot2, sample size 478
data(economics,package="ggplot2")

name<-"economics"
name_p<-"ggplot2"
name_s<-"lin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(as.data.frame(economics[,3:6]))

source(path1)

set.seed(128)
c<-regr_ind(as.data.frame(economics[,3:6]),as.data.frame(economics)[,2],1000,mode='linear')

#first line of the output
#[1] 308.6363932 0.1229381 3100.4678496 1.6394895 308.6363932 0.1292587



#mpg from the package ggplot2, sample size 234
data(mpg,package="ggplot2")

name<-"mpg"
name_p<-"ggplot2"
name_s<-"lin"
ou<-8
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]))

source(path1)

set.seed(129)
c<-regr_ind(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]),as.data.frame(mpg)[,8],1000,mode='linear')

#first line of the output
#[1] 1.2280417 0.0702405 3.3483869 0.2119952 5.8805833 0.3936189



#the same dataset with a different predictor
name<-"mpg"
name_p<-"ggplot2"
name_s<-"lin"
ou<-9
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]))

source(path1)

set.seed(130)
c<-regr_ind(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]),as.data.frame(mpg)[,9],1000,mode='linear')

#first line of the output
#[1] 1.52541667 0.06559844 4.96845238 0.23618654 5.73625000 0.26474140

source(path2)

set.seed(130)


set.seed(130)
c2<-regr_ind(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]),as.data.frame(mpg)[,9],1000,mode='linear',Rsq=T)


set.seed(130)
c3<-regr_ind(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]),as.data.frame(mpg)[,9],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#txhousing from the package ggplot2, sample size 8602
data(txhousing,package="ggplot2")





#removing NA values, reducing the sample size to 7134
datai<-cbind(as.data.frame(txhousing)[,1:4],as.data.frame(txhousing)[,7:8])

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}


name<-"txhousing"
name_p<-"ggplot2"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(as.data.frame(txhousing)[ind,1:3],as.data.frame(txhousing)[ind,7:8]))

source(path1)

set.seed(131)
c<-regr_ind(cbind(as.data.frame(txhousing)[ind,1:3],as.data.frame(txhousing)[ind,7:8]),as.data.frame(txhousing)[ind,4],1000,mode='linear')

#first line of the output
#[1] 125.3734972 0.2444171 649.4811667 4.3310534 129.1523894 0.3886428


source(path2)

set.seed(131)


set.seed(131)
c2<-regr_ind(cbind(as.data.frame(txhousing)[ind,1:3],as.data.frame(txhousing)[ind,7:8]),as.data.frame(txhousing)[ind,4],1000,mode='linear',Rsq=T)


set.seed(131)
c3<-regr_ind(cbind(as.data.frame(txhousing)[ind,1:3],as.data.frame(txhousing)[ind,7:8]),as.data.frame(txhousing)[ind,4],1000,mode='linear', Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()




#midwest from the package ggplot2, sample size 400
data(midwest,package="ggplot2")

name<-"midwest"
name_p<-"ggplot2"
name_s<-"lin"
ou<-22
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(as.data.frame(midwest[,4:20]))

source(path1)

set.seed(134)
c<-regr_ind(as.data.frame(midwest[,4:20]),as.data.frame(midwest)[,22],1000,mode='linear')

source(path2)

set.seed(134)

set.seed(134)
c2<-regr_ind(as.data.frame(midwest[,4:20]),as.data.frame(midwest)[,22],1000,mode='linear',Rsq=T)

set.seed(134)
c3<-regr_ind(as.data.frame(midwest[,4:20]),as.data.frame(midwest)[,22],1000,mode='linear',Rsq=T,marg=0.1)

#first line of the output
#[1] 1.50688997 0.01607700 1.81372543 0.01929520 1.60741481 0.01705375

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#diamonds from the package ggplot2, sample size 54000
data(diamonds,package="ggplot2")

name<-"diamonds"
name_p<-"ggplots2"
name_s<-"lin"
ou<-7
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(as.data.frame(diamonds[,1:6]),as.data.frame(diamonds[,8:10])))

source(path1)

set.seed(135)
c<-regr_ind(cbind(as.data.frame(diamonds[,1:6]),as.data.frame(diamonds[,8:10])),as.data.frame(diamonds)[,7],1000,mode='linear')

#first line of the output
#[1] 740.0139716 0.2710318 3031.2540127 1.8809806 740.5193003 0.3904769

source(path2)

set.seed(135)


set.seed(135)
c2<-regr_ind(cbind(as.data.frame(diamonds[,1:6]),as.data.frame(diamonds[,8:10])),as.data.frame(diamonds)[,7],1000,mode='linear',Rsq=T)


set.seed(135)
c3<-regr_ind(cbind(as.data.frame(diamonds[,1:6]),as.data.frame(diamonds[,8:10])),as.data.frame(diamonds)[,7],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#environmental from the package lattice, sample size 111

data(environmental,package="lattice")

name<-"environmental"
name_p<-"lattice"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(environmental[,2:4])

source(path1)

set.seed(137)
c<-regr_ind(environmental[,2:4],environmental[,1],1000,mode='linear')

#first line of the output
#[1] 16.0594167 0.6832754 26.7794815 1.4994344 16.0594167 0.6832754

source(path2)

set.seed(137)


set.seed(137)
c2<-regr_ind(environmental[,2:4],environmental[,1],1000,mode='linear',Rsq=T)


set.seed(137)
c3<-regr_ind(environmental[,2:4],environmental[,1],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#including quadratic terms in the model
name<-"qu_environmental"
name_p<-"lattice"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quadr(environmental[,2:4]))

qu_environmental<-quadr(environmental[,2:4])

source(path1)

set.seed(138)
c<-regr_ind(quadr(environmental[,2:4]),environmental[,1],1000,mode='linear')

#first line of the output
#[1] 13.7926667 0.4695287 26.9547306 1.5016453 14.0320833 0.6760230

source(path2)

set.seed(138)


set.seed(138)
c2<-regr_ind(quadr(environmental[,2:4]),environmental[,1],1000,mode='linear',Rsq=T)


set.seed(138)
c3<-regr_ind(quadr(environmental[,2:4]),environmental[,1],1000,mode='linear',Rsq=T,marg=0.1)



# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Boston from the package MASS, sample size 506

data(Boston,package="MASS")

name<-"Boston"
name_p<-"MASS"
name_s<-"lin"
ou<-14
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Boston[,1:13])

source(path1)

set.seed(139)
c<-regr_ind(Boston[,1:13],Boston[,14],1000,mode='linear')

#first line of the output
#[1] 3.3550508 0.1690855 6.6236054 0.3634686 3.3684651 0.1696334

source(path2)

set.seed(139)

set.seed(139)
c2<-regr_ind(Boston[,1:13],Boston[,14],1000,mode='linear',Rsq=T)

set.seed(139)
c3<-regr_ind(Boston[,1:13],Boston[,14],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#OME from the package MASS, sample size 1129

data(OME,package="MASS")

name<-"OME"
name_p<-"MASS"
name_s<-"lin"
ou<-6
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(OME[,2:5])

source(path1)

set.seed(140)
c<-regr_ind(OME[,2:5],OME[,6]/OME[,7]+0.0000001,1000,mode='linear')

#first line of the output
#[1] 1.808355e-01 3.422046e+05 2.565896e-01 4.328624e+05 1.816264e-01 3.442949e+05

source(path2)

set.seed(140)

set.seed(140)
c2<-regr_ind(OME[,2:5],OME[,6]/OME[,7]+0.0000001,1000,mode='linear',Rsq=T)

set.seed(140)
c3<-regr_ind(OME[,2:5],OME[,6]/OME[,7]+0.0000001,1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Rabbit from the package MASS, sample size 60

data(Rabbit,package="MASS")

name<-"Rabbit"
name_p<-"MASS"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Rabbit[,2:5])

source(path1)

set.seed(141)
c<-regr_ind(Rabbit[,2:5],Rabbit[,1],1000,mode='linear')

#first line of the output
#[1] 4.428361 1.066375 10.111532 3.639913 4.749548 1.103338

source(path2)

set.seed(141)


set.seed(141)
c2<-regr_ind(Rabbit[,2:5],Rabbit[,1],1000,mode='linear',Rsq=T)


set.seed(141)
c3<-regr_ind(Rabbit[,2:5],Rabbit[,1],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Traffic from the package MASS, sample size 184
data(Traffic,package="MASS")

name<-"Traffic"
name_p<-"MASS"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Traffic[,1:3])

source(path1)

set.seed(142)
c<-regr_ind(Traffic[,1:3],Traffic[,4],1000,mode='linear')

#first line of the output
#[1] 6.8250526 0.3722014 6.8997965 0.3832491 6.9378421 0.3792912

source(path2)

set.seed(142)


set.seed(142)
c2<-regr_ind(Traffic[,1:3],Traffic[,4],1000,mode='linear',Rsq=T)


set.seed(142)
c3<-regr_ind(Traffic[,1:3],Traffic[,4],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#UScereal from the package MASS, sample size 65
data(UScereal,package="MASS")

name<-"UScereal"
name_p<-"MASS"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(UScereal[,2:11])

source(path1)

set.seed(143)
c<-regr_ind(UScereal[,2:11],UScereal[,1],1000,mode='linear')

#first line of the output
#[1] 0.9421429 0.2911630 1.1847635 0.4101309 1.0279971 0.4564103

source(path2)

set.seed(143)

set.seed(143)
c2<-regr_ind(UScereal[,2:11],UScereal[,1],1000,mode='linear',Rsq=T)

set.seed(143)
c3<-regr_ind(UScereal[,2:11],UScereal[,1],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#UScrime from the package MASS, sample size 47
data(UScrime,package="MASS")

name<-"UScrime"
name_p<-"MASS"
name_s<-"lin"
ou<-16
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(UScrime[,1:15])

source(path1)

set.seed(144)
c<-regr_ind(UScrime[,1:15],UScrime[,16],1000,mode='linear')

#first line of the output
#[1] 177.9804000 0.2081134 303.7863333 0.3776392 241.0920000 0.3081034

source(path2)

set.seed(144)


set.seed(144)
c2<-regr_ind(UScrime[,1:15],UScrime[,16],1000,mode='linear',Rsq=T)


set.seed(144)
c3<-regr_ind(UScrime[,1:15],UScrime[,16],1000,mode='linear',Rsq=T,marg=0.1)



# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#birthwt from the package MASS, sample size 189

data(birthwt,package="MASS")

name<-"birthwt"
name_p<-"MASS"
name_s<-"lin"
ou<-10
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(birthwt[,1:9])

source(path1)

set.seed(145)
c<-regr_ind(birthwt[,1:9],birthwt[,10],1000,mode='linear')
#first line of the output
#[1] 361.5716842 0.1368641 591.8646232 0.2411088 364.9017368 0.1394650

source(path2)

set.seed(145)

set.seed(145)
c2<-regr_ind(birthwt[,1:9],birthwt[,10],1000,mode='linear',Rsq=T)

set.seed(145)
c3<-regr_ind(birthwt[,1:9],birthwt[,10],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



#the same dataset for a different set of predictors
name<-"birthwt"
name_p<-"MASS"
name_s<-"lin"
ou<-10
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(birthwt[,2:9])

source(path1)

set.seed(150)
c<-regr_ind(birthwt[,2:9],birthwt[,10],1000,mode='linear')
#first line of the output
#[1] 539.9145263 0.2168056 592.9032269 0.2426130 561.5234737 0.2269350

source(path2)

set.seed(150)


set.seed(150)
c2<-regr_ind(birthwt[,2:9],birthwt[,10],1000,mode='linear',Rsq=T)


set.seed(150)
c3<-regr_ind(birthwt[,2:9],birthwt[,10],1000,mode='linear',Rsq=T,marg=0.1)



# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#cabbages from the package MASS, sample size 60

data(cabbages,package="MASS")

name<-"cabbages"
name_p<-"MASS"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cabbages[,1:3])

source(path1)

set.seed(147)
c<-regr_ind(cabbages[,1:3],cabbages[,4],1000,mode='linear')
#first line of the output
#[1] 4.92483333 0.08694499 8.55504938 0.15202000 4.99566667 0.08812669

source(path2)

set.seed(147)


set.seed(147)
c2<-regr_ind(cabbages[,1:3],cabbages[,4],1000,mode='linear',Rsq=T)


set.seed(147)
c3<-regr_ind(cabbages[,1:3],cabbages[,4],1000,mode='linear',Rsq=T,marg=0.1)



# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



#coop from the package MASS, sample size 252

data(coop,package="MASS")

name<-"coop"
name_p<-"MASS"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(coop[,1:3])

source(path1)

set.seed(148)
c<-regr_ind(coop[,1:3],coop[,4],1000,mode='linear')
#first line of the output
#[1] 0.2443722 0.3180667 1.7051254 2.3567321 0.3152603 0.3451214

source(path2)

set.seed(148)
#

set.seed(148)
c2<-regr_ind(coop[,1:3],coop[,4],1000,mode='linear',Rsq=T)
#

set.seed(148)
c3<-regr_ind(coop[,1:3],coop[,4],1000,mode='linear',Rsq=T,marg=0.1)
#

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#cpus from the package MASS, sample size 209

data(cpus,package="MASS")

name<-"cpus"
name_p<-"MASS"
name_s<-"lin"
ou<-8
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cpus[,2:7])

source(path1)

set.seed(149)
c<-regr_ind(cpus[,2:7],cpus[,8],1000,mode='linear')
#first line of the output
#[1] 40.5054762 0.5105281 96.0344352 2.1915047 41.1369524 0.7803790

source(path2)

set.seed(149)

set.seed(149)
c2<-regr_ind(cpus[,2:7],cpus[,8],1000,mode='linear',Rsq=T)

set.seed(149)
c3<-regr_ind(cpus[,2:7],cpus[,8],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



#epil from the package MASS, sample size 236
data(epil,package="MASS")

name<-"epil"
name_p<-"MASS"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(epil[,2:9])

source(path1)

set.seed(151)
c<-regr_ind(epil[,2:9],epil[,1],1000,mode='linear')
#first line of the output
#[1] 4.311750 0.721548 7.130709 1.517481 4.312125 0.721548

source(path2)

set.seed(151)


set.seed(151)
c2<-regr_ind(epil[,2:9],epil[,1],1000,mode='linear',Rsq=T)


set.seed(151)
c3<-regr_ind(epil[,2:9],epil[,1],1000,mode='linear',Rsq=T,marg=0.1)



# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


# #fgl from the package MASS, sample size 214
# data(fgl,package="MASS")
# 
# set.seed(152)
# regr_ind(fgl[,1:9],fgl[,10],1000,mode='linear')
# #first line of the output
# #[1] 0.8565455  Inf 1.1145000  NaN 0.9047273  NaN

#housing from the package MASS, sample size 72
data(housing,package="MASS")

name<-"housing"
name_p<-"MASS"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(housing[,2:5])

source(path1)

set.seed(153)
c<-regr_ind(housing[,2:5],housing[,1],1000,mode='linear')
#first line of the output
#[1] 0.6647500 0.2321429 0.6826602 0.2753906 1.0000000  NaN

source(path2)

set.seed(153)


set.seed(153)
c2<-regr_ind(housing[,2:5],housing[,1],1000,mode='linear',Rsq=T)


set.seed(153)
c3<-regr_ind(housing[,2:5],housing[,1],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#nlschools from the package MASS, sample size 2287

data(nlschools,package="MASS")

name<-"nlschools"
name_p<-"MASS"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(nlschools[,2:6])

source(path1)

set.seed(155)
c<-regr_ind(nlschools[,2:6],nlschools[,1],1000,mode='linear')

#first line of the output
#[1] 5.2521921 0.1463301 7.3639437 0.2123014 5.2521921 0.1463301

source(path2)

set.seed(155)


set.seed(155)
c2<-regr_ind(nlschools[,2:6],nlschools[,1],1000,mode='linear',Rsq=T)


set.seed(155)
c3<-regr_ind(nlschools[,2:6],nlschools[,1],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#nlschools from the package MASS, sample size 30

data(immer,package="MASS")

name<-"immer"
name_p<-"MASS"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(immer[,1:3])

source(path1)

set.seed(154)
c<-regr_ind(immer[,1:3],immer[,4],1000,mode='linear')

#first line of the output
#[1] 17.3855404 0.1991453 19.9225309 0.2285893 13.9538488 0.1607105

source(path2)

set.seed(154)

set.seed(154)
c2<-regr_ind(immer[,1:3],immer[,4],1000,mode='linear',Rsq=T)

set.seed(154)
c3<-regr_ind(immer[,1:3],immer[,4],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#oats from the package MASS, sample size 72
data(oats,package="MASS")

name<-"oats"
name_p<-"MASS"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(oats[,1:3])

source(path1)

set.seed(156)
c<-regr_ind(oats[,1:3],oats[,4],1000,mode='linear')

#first line of the output
#[1] 17.7446250 0.1776077 22.0894336 0.2352328 14.3116250 0.1467353

source(path2)

set.seed(156)


set.seed(156)
c2<-regr_ind(oats[,1:3],oats[,4],1000,mode='linear',Rsq=T)


set.seed(156)
c3<-regr_ind(oats[,1:3],oats[,4],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


# #painters from the package MASS, sample size 72
# data(painters,package="MASS")
# 
# set.seed(157)
# regr_ind(painters[,1:4],painters[,5],1000,mode='linear')
# 
# #first line of the output
# #[1] 1.4485000 0.6647151 1.9160972 0.6850662 1.5590000 0.7868087


#petrol from the package MASS, sample size 32
data(petrol,package="MASS")

name<-"petrol"
name_p<-"MASS"
name_s<-"lin"
ou<-6
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(petrol[,1:5])

source(path1)

set.seed(158)
c<-regr_ind(petrol[,1:5],petrol[,6],1000,mode='linear')

#first line of the output
#[1] 2.096233 0.167669 9.335995 0.844543 5.001481 0.381677

source(path2)

set.seed(158)

set.seed(158)
c2<-regr_ind(petrol[,1:5],petrol[,6],1000,mode='linear',Rsq=T)

set.seed(158)
c3<-regr_ind(petrol[,1:5],petrol[,6],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#quine from the package MASS, sample size 146
data(quine,package="MASS")

name<-"quine"
name_p<-"MASS"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quine[,1:4])

source(path1)

set.seed(159)
c<-regr_ind(quine[,1:4],quine[,5],1000,mode='linear')

#first line of the output
#[1] 11.591067 1.515183 12.496293 1.691373 11.936733 1.621413

source(path2)

set.seed(159)

set.seed(159)
c2<-regr_ind(quine[,1:4],quine[,5],1000,mode='linear',Rsq=T)

set.seed(159)
c3<-regr_ind(quine[,1:4],quine[,5],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#road from the package MASS, sample size 26
data(road,package="MASS")

name<-"road"
name_p<-"MASS"
name_s<-"lin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(road[,3:6])

source(path1)

set.seed(160)
c<-regr_ind(road[,3:6],road[,2],1000,mode='linear')

#first line of the output
#[1] 67.4353333 0.5127998 129.5784348 1.8690579 91.7740000 0.6430287

source(path2)

set.seed(160)

set.seed(160)
c2<-regr_ind(road[,3:6],road[,2],1000,mode='linear',Rsq=T)

set.seed(160)
c3<-regr_ind(road[,3:6],road[,2],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


# #including quadratic terms
# 
# set.seed(161)
# regr_ind(quadr(road[,3:6]),road[,2],1000,mode='linear')
# 
# #first line of the output
# #[1] 68.4713333 0.3395833 128.3834783 1.7649064 128.8740000 1.0107998


#Gasoline from the package nlme, sample size 32
data(Gasoline,package="nlme")

name<-"Gasoline"
name_p<-"nlme"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Gasoline[,2:6])

source(path1)

set.seed(162)
c<-regr_ind(Gasoline[,2:6],Gasoline[,1],1000,mode='linear')

#first line of the output
#[1] 2.1174769 0.1653246 9.4245196 0.8366709 5.0017443 0.3817855

source(path2)

set.seed(162)

set.seed(162)
c2<-regr_ind(Gasoline[,2:6],Gasoline[,1],1000,mode='linear',Rsq=T)

set.seed(162)
c3<-regr_ind(Gasoline[,2:6],Gasoline[,1],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


# #including quadratic terms
# 
# set.seed(163)
# regr_ind(quadr(apply(as.matrix.noquote(Gasoline),2,as.numeric)[,2:6]),Gasoline[,1],1000,mode='linear')
# 
# #first line of the output
# #[1] 2.0699387 0.1324884 9.3715661 0.8228121 2.5968704 0.1801361

#MathAchieve from the package nlme, sample size 7185
data(MathAchieve,package="nlme")

name<-"MathAchieve"
name_p<-"nlme"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(MathAchieve[,c(2:4,6)])

source(path1)

set.seed(164)
c<-regr_ind(MathAchieve[,c(2:4,6)],MathAchieve[,5],1000,mode='linear')

#first line of the output
#[1] 5.123259 2.340082 5.823388 2.873735 5.123259 2.340082

source(path2)

set.seed(164)

set.seed(164)
c2<-regr_ind(MathAchieve[,c(2:4,6)],MathAchieve[,5],1000,mode='linear',Rsq=T)

set.seed(164)
c3<-regr_ind(MathAchieve[,c(2:4,6)],MathAchieve[,5],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



#MathAchieve from the package nlme, sample size 160
data(MathAchSchool,package="nlme")

name<-"MathAchSchool"
name_p<-"nlme"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(MathAchSchool[,c(2:4,6:7)])

source(path1)

set.seed(165)
c<-regr_ind(MathAchSchool[,c(2:4,6:7)],MathAchSchool[,5],1000,mode='linear')

#first line of the output
#[1] 0.7069805 1.0289295 0.7801860 1.1483394 0.7137737 6.3802130

source(path2)

set.seed(165)

set.seed(165)
c2<-regr_ind(MathAchSchool[,c(2:4,6:7)],MathAchSchool[,5],1000,mode='linear',Rsq=T)

set.seed(165)
c3<-regr_ind(MathAchSchool[,c(2:4,6:7)],MathAchSchool[,5],1000,mode='linear',Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



#Milk from the package nlme, sample size 1337
data(Milk,package="nlme")

name<-"Milk"
name_p<-"nlme"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Milk[,2:4])

source(path1)

set.seed(167)
c<-regr_ind(Milk[,2:4],Milk[,1],1000,mode='linear')

#first line of the output
#[1] 0.21983135 0.06456367 0.26308696 0.07763502 0.22031652 0.06472208

source(path2)

set.seed(167)

set.seed(167)
c2<-regr_ind(Milk[,2:4],Milk[,1],1000,mode='linear', Rsq=T)

set.seed(167)
c3<-regr_ind(Milk[,2:4],Milk[,1],1000,mode='linear', Rsq=T,marg=0.1)

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Phenobarb from the package nlme, sample size 744
data(Phenobarb,package="nlme")



source(path1)

#removing the NA values, reducing the sample size to 589
datai<-Phenobarb[,2:6]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"Phenobarb"
name_p<-"nlme"
name_s<-"lin"
ou<-6
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Phenobarb[ind,2:5])

set.seed(168)
c<-regr_ind(Phenobarb[ind,2:5],Phenobarb[ind,6],1000,mode='linear')

#first line of the output
#[1] 4.7692434 0.7259111 5.1629820 0.9969036 7.1565476 1.6221474

set.seed(168)


set.seed(168)
c2<-regr_ind(Phenobarb[ind,2:5],Phenobarb[ind,6],1000,mode='linear',Rsq=T)


set.seed(168)
c3<-regr_ind(Phenobarb[ind,2:5],Phenobarb[ind,6],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#including the quadratic terms
name<-"qu_Phenobarb"
name_p<-"nlme"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]))

qu_Phenobarb<-cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4])

source(path1)

set.seed(169)
c<-regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,6],1000,mode='linear')

#first line of the output
#[1] 3.9110456 0.6058816 5.2573102 0.9895885 3.9444009 0.7747142

source(path2)

set.seed(169)


set.seed(169)
c2<-regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,6],1000,mode='linear',Rsq=T)


set.seed(169)
c3<-regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,6],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{q(Phenobarb)} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#same dataset, different outcome

#removing the NA values, reducing the sample size to 155
datai<-Phenobarb[,c(2:5,7)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"Phenobarb"
name_p<-"nlme"
name_s<-"lin"
ou<-7
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Phenobarb[ind,2:5])

source(path1)

set.seed(171)
c<-regr_ind(Phenobarb[ind,2:5],Phenobarb[ind,7],1000,mode='linear')
#first line of the output
#[1] 6.3691385 0.3040755 6.6053284 0.3048933 6.7003913 0.3503109

source(path2)

set.seed(171)


set.seed(171)
c2<-regr_ind(Phenobarb[ind,2:5],Phenobarb[ind,7],1000,mode='linear',Rsq=T)


set.seed(171)
c3<-regr_ind(Phenobarb[ind,2:5],Phenobarb[ind,7],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#including quadratic terms
name<-"qu_Phenobarb"
name_p<-"nlme"
name_s<-"lin"
ou<-7
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]))

qu_Phenobarb<-cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4])

source(path1)

set.seed(172)
c<-regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,7],1000,mode='linear')

#first line of the output
#[1] 4.4508046 0.1951310 6.6272059 0.3035403 6.8693454 0.3585367

source(path2)

set.seed(172)


set.seed(172)
c2<-regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,7],1000,mode='linear',Rsq=T)


set.seed(172)
c3<-regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,7],1000,mode='linear', Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Quinidine from the package nlme, sample size 1471
data(Quinidine,package="nlme")



#removing the NA values, reducing the sample size to 1028

datai<-Quinidine[,c(2,4,6:14)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}


name<-"Quinidine"
name_p<-"nlme"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Quinidine[ind,c(2,6:14)])

source(path1)

set.seed(170)
c<-regr_ind(Quinidine[ind,c(2,6:14)],Quinidine[ind,4],1000,mode='linear')

#first line of the output
#[1] 51.5763301 0.2306375 52.6001954 0.2350661 52.1702524 0.2342444

source(path2)

set.seed(170)

set.seed(170)
c2<-regr_ind(Quinidine[ind,c(2,6:14)],Quinidine[ind,4],1000,mode='linear',Rsq=T)

set.seed(170)
c3<-regr_ind(Quinidine[ind,c(2,6:14)],Quinidine[ind,4],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Remifentanil from the package nlme, sample size 2107
data(Remifentanil,package="nlme")



#removing the NA values, reducing the sample size to 1992

datai<-Remifentanil[,3:12]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"Remifentanil"
name_p<-"nlme"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Remifentanil[ind,c(3,5:12)])

source(path1)

set.seed(173)
c<-regr_ind(Remifentanil[ind,c(3,5:12)],Remifentanil[ind,4],1000,mode='linear')

#first line of the output
#[1] 12.296073 4.561360 23.266024 18.717709 12.318301 4.693894

source(path2)

set.seed(173)


set.seed(173)
c2<-regr_ind(Remifentanil[ind,c(3,5:12)],Remifentanil[ind,4],1000,mode='linear', Rsq=T)


set.seed(173)
c3<-regr_ind(Remifentanil[ind,c(3,5:12)],Remifentanil[ind,4],1000,mode='linear', Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Soybean from the package nlme, sample size 412
data(Soybean,package="nlme")

name<-"Soybean"
name_p<-"nlme"
name_s<-"lin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Soybean[,2:4])

source(path1)

set.seed(174)
c<-regr_ind(Soybean[,2:4],Soybean[,5],1000,mode='linear')

#first line of the output
#[1] 2.096382 4.459741 5.884695 13.218127 2.112355 5.278725

source(path2)

set.seed(174)


set.seed(174)
c2<-regr_ind(Soybean[,2:4],Soybean[,5],1000,mode='linear',Rsq=T)


set.seed(174)
c3<-regr_ind(Soybean[,2:4],Soybean[,5],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Wheat from the package nlme, sample size 48
data(Wheat,package="nlme")

name<-"Wheat"
name_p<-"nlme"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(as.numeric(Wheat[,1]),Wheat[,2:3]))

source(path1)

set.seed(175)
c<-regr_ind(cbind(as.numeric(Wheat[,1]),Wheat[,2:3]),Wheat[,4],1000,mode='linear')

#first line of the output
#[1] 2.0589174 0.2859348 3.2210669 0.5015642 2.1082381 0.2904173

source(path2)

set.seed(175)


set.seed(175)
c2<-regr_ind(cbind(as.numeric(Wheat[,1]),Wheat[,2:3]),Wheat[,4],1000,mode='linear',Rsq=T)


set.seed(175)
c3<-regr_ind(cbind(as.numeric(Wheat[,1]),Wheat[,2:3]),Wheat[,4],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#including quadratic terms
name<-"qu_Wheat"
name_p<-"nlme"
name_s<-"lin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quadr(cbind(as.numeric(levels(Wheat[,1])[Wheat[,1]]),Wheat[,2:3])))

qu_Wheat<-quadr(cbind(as.numeric(levels(Wheat[,1])[Wheat[,1]]),Wheat[,2:3]))

source(path1)

set.seed(176)
c<-regr_ind(quadr(cbind(as.numeric(levels(Wheat[,1])[Wheat[,1]]),Wheat[,2:3])),Wheat[,4],1000,mode='linear')

#first line of the output
#[1] 1.0809070 0.1567015 3.1841391 0.5110255 1.7519201 0.2620687

source(path2)

set.seed(176)


set.seed(176)
c2<-regr_ind(quadr(cbind(as.numeric(levels(Wheat[,1])[Wheat[,1]]),Wheat[,2:3])),Wheat[,4],1000,mode='linear',Rsq=T)


set.seed(176)
c3<-regr_ind(quadr(cbind(as.numeric(levels(Wheat[,1])[Wheat[,1]]),Wheat[,2:3])),Wheat[,4],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Wheat2 from the package nlme, sample size 224
data(Wheat2,package="nlme")

name<-"Wheat2"
name_p<-"nlme"
name_s<-"lin"
ou<-3
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(as.numeric(Wheat2[,1]),Wheat2[,c(2,4:5)]))

source(path1)

set.seed(177)
c<-regr_ind(cbind(as.numeric(Wheat2[,1]),Wheat2[,c(2,4:5)]),Wheat2[,3],1000,mode='linear')

#first line of the output
#[1] 4.5363964 0.4078296 5.4164956 0.5704332 4.5514874 0.4078296
source(path2)

set.seed(177)


set.seed(177)
c2<-regr_ind(cbind(as.numeric(Wheat2[,1]),Wheat2[,c(2,4:5)]),Wheat2[,3],1000,mode='linear',Rsq=T)


set.seed(177)
c3<-regr_ind(cbind(as.numeric(Wheat2[,1]),Wheat2[,c(2,4:5)]),Wheat2[,3],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#including quadratic terms
name<-"qu_Wheat2"
name_p<-"nlme"
name_s<-"lin"
ou<-3
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(quadr(cbind(as.numeric(levels(Wheat2[,1])[Wheat2[,1]]),Wheat2[,4:5])),Wheat2[,2]))

qu_Wheat2<-cbind(quadr(cbind(as.numeric(levels(Wheat2[,1])[Wheat2[,1]]),Wheat2[,4:5])),Wheat2[,2])

source(path1)

set.seed(178)
c<-regr_ind(cbind(quadr(cbind(as.numeric(levels(Wheat2[,1])[Wheat2[,1]]),Wheat2[,4:5])),Wheat2[,2]),Wheat2[,3],1000,mode='linear',parallel=TRUE,cores=7)

#first line of the output
#[1] 4.0864935 0.3457878 5.4752173 0.5979057 4.1904725 0.3502343
source(path2)

set.seed(178)

set.seed(178)
c2<-regr_ind(cbind(quadr(cbind(as.numeric(levels(Wheat2[,1])[Wheat2[,1]]),Wheat2[,4:5])),Wheat2[,2]),Wheat2[,3],1000,mode='linear',parallel=TRUE,cores=7,Rsq=T)

set.seed(178)
c3<-regr_ind(cbind(quadr(cbind(as.numeric(levels(Wheat2[,1])[Wheat2[,1]]),Wheat2[,4:5])),Wheat2[,2]),Wheat2[,3],1000,mode='linear',parallel=TRUE,cores=7,Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



#tips from the package nlme, sample size 244
data(tips,package="reshape2")

name<-"tips"
name_p<-"reshape2"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(tips[,c(1,3:7)])

source(path1)

set.seed(180)
c<-regr_ind(tips[,c(1,3:7)],tips[,2],1000,mode='linear')

#first line of the output
#[1] 0.7505459 0.2792391 1.0447381 0.4177951 0.7516984 0.2797217
source(path2)

set.seed(180)


set.seed(180)
c2<-regr_ind(tips[,c(1,3:7)],tips[,2],1000,mode='linear',Rsq=T)


set.seed(180)
c3<-regr_ind(tips[,c(1,3:7)],tips[,2],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#car.test.frame from the package rpart, sample size 60
data(car.test.frame,package="rpart")

#removing NA values, reducing the sample size to 60

datai<-car.test.frame[,c(1:2,4:8)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"car.test.frame"
name_p<-"rpart"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(car.test.frame[ind,c(2,4:8)])

source(path1)

set.seed(181)
c<-regr_ind(car.test.frame[ind,c(2,4:8)],car.test.frame[ind,1],1000,mode='linear')
#first line of the output
#[1] 2036.7983333 0.1554235 3184.0909012 0.2843645 4295.5801667 0.3630200
source(path2)

set.seed(181)


set.seed(181)
c2<-regr_ind(car.test.frame[ind,c(2,4:8)],car.test.frame[ind,1],1000,mode='linear',Rsq=T)


set.seed(181)
c3<-regr_ind(car.test.frame[ind,c(2,4:8)],car.test.frame[ind,1],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



#cu.summary from the package rpart, sample size 117
data(cu.summary,package="rpart")

#removing NA values, reducing the sample size to 49

datai<-cu.summary[,1:5]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"cu.summary"
name_p<-"rpart"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cu.summary[ind,2:5])

source(path1)

set.seed(182)
c<-regr_ind(cu.summary[ind,2:5],cu.summary[ind,1],100,mode='linear')
#first line of the output
#[1] 2.432976e+03 1.879221e-01 3.255523e+03 2.888707e-01 1.382275e+04 1.289736e+00
source(path2)

set.seed(182)


set.seed(182)
c2<-regr_ind(cu.summary[ind,2:5],cu.summary[ind,1],100,mode='linear',Rsq=T)


set.seed(182)
c3<-regr_ind(cu.summary[ind,2:5],cu.summary[ind,1],100,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#cancer from the package survival, sample size 228
data(cancer,package="survival")

#removing NA values, reducing the sample size to 167

datai<-cancer[,1:10]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"cancer"
name_p<-"survival"
name_s<-"lin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cancer[ind,c(1,3:10)])

source(path1)

set.seed(184)
c<-regr_ind(cancer[ind,c(1,3:10)],cancer[ind,2],1000,mode='linear')
#first line of the output
#[1] 158.276059 1.659907 162.654458 1.820468 165.347934 1.779803
source(path2)

set.seed(184)


set.seed(184)
c2<-regr_ind(cancer[ind,c(1,3:10)],cancer[ind,2],1000,mode='linear',Rsq=T)


set.seed(184)
c3<-regr_ind(cancer[ind,c(1,3:10)],cancer[ind,2],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#cgd from the package survival, sample size 117
data(cgd,package="survival")


name<-"cgd"
name_p<-"survival"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cgd[,c(13,4:12)])

source(path1)

set.seed(185)
c<-regr_ind(cgd[,c(13,4:12)],cgd[,14],1000,mode='linear')
#first line of the output
#[1] 0.3355238 0.1192488 0.8477666 0.5411852 0.3622381 0.1324598
source(path2)

set.seed(185)


set.seed(185)
c2<-regr_ind(cgd[,c(13,4:12)],cgd[,14],1000,mode='linear',Rsq=T)


set.seed(185)
c3<-regr_ind(cgd[,c(13,4:12)],cgd[,14],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#flchain from the package survival, sample size 7874
data(flchain,package="survival")

datai<-flchain[,1:9]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"flchain"
name_p<-"survival"
name_s<-"lin"
ou<-9
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(flchain[ind,c(1:8)])

source(path1)

set.seed(186)
c<-regr_ind(flchain[ind,c(1:8)],flchain[ind,9]+0.0000000001,1000,mode='linear')
#first line of the output
#[1] 9.029672e+02 5.962283e+09 1.196929e+03 1.715029e+10 9.037316e+02 6.383050e+09
source(path2)

set.seed(186)

set.seed(186)
c2<-regr_ind(flchain[ind,c(1:8)],flchain[ind,9]+0.0000000001,1000,mode='linear',Rsq=T)


set.seed(186)
c3<-regr_ind(flchain[ind,c(1:8)],flchain[ind,9]+0.0000000001,1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)

# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#the same dataset, different predictors

#removing NA values reducing the sample size to 1962
datai<-flchain[,c(1:9,11)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"flchain"
name_p<-"survival"
name_s<-"lin"
ou<-9
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(flchain[ind,c(11,1:8)])

source(path1)

set.seed(187)
c<-regr_ind(flchain[ind,c(11,1:8)],flchain[ind,9]+0.0000000001,1000,mode='linear')

#first line of the output
#[1] 1.074215e+03 1.638743e+10 1.193699e+03 3.333615e+10 1.074619e+03 1.779234e+10

source(path2)

set.seed(187)


set.seed(187)
c2<-regr_ind(flchain[ind,c(11,1:8)],flchain[ind,9]+0.0000000001,1000,mode='linear',Rsq=T)


set.seed(187)
c3<-regr_ind(flchain[ind,c(11,1:8)],flchain[ind,9]+0.0000000001,1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#logan from the package survival, sample size 838
data(logan,package="survival")

name<-"logan"
name_p<-"survival"
name_s<-"lin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(logan[,c(1,2,4)])

source(path1)

set.seed(188)
c<-regr_ind(logan[,c(1,2,4)],logan[,3],1000,mode='linear')
#first line of the output
#[1] 1.5919762 0.1332218 2.2493278 0.1842479 1.5936429 0.1333967

source(path2)

set.seed(188)


set.seed(188)
c2<-regr_ind(logan[,c(1,2,4)],logan[,3],1000,mode='linear',Rsq=T)


set.seed(188)
c3<-regr_ind(logan[,c(1,2,4)],logan[,3],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#retinopathy from the package survival, sample size 394
data(retinopathy,package="survival")


name<-"retinopathy"
name_p<-"survival"
name_s<-"lin"
ou<-9
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(retinopathy[,2:8])

source(path1)

set.seed(189)
c<-regr_ind(retinopathy[,2:8],retinopathy[,9],1000,mode='linear')
#first line of the output
#[1] 1.1269000 0.1243687 1.1874955 0.1312497 1.1564919 0.1282755

source(path2)

set.seed(189)


set.seed(189)
c2<-regr_ind(retinopathy[,2:8],retinopathy[,9],1000,mode='linear',Rsq=T)


set.seed(189)
c3<-regr_ind(retinopathy[,2:8],retinopathy[,9],1000,mode='linear',Rsq=T,marg=0.1)


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c1[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][5],2)),cat("/"),cat(round(c[[1]][6],2)),cat("$ & $"),cat(round(c[[1]][7],2)),cat("/"),cat(round(c[[1]][8],2)),cat("$ & $"),cat(round(c2[[1]][3],2)),cat("/"),cat(round(c2[[1]][4],2)),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c2[[1]][2],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c3[[1]][2],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#best significant lasso emp rsq rsq+marg

############################################################################################

#pneumon

#the following examples are for the mode 'binary'

#bacteria from the package MASS, sample size 220, 43 less likely outcomes

#maximising accuracy
data(bacteria,package="MASS")

name<-"bacteria"
name_p<-"MASS"
name_s<-"bin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(bacteria[,2:6])


source(path1)
set.seed(12398)
c<-regr_ind(bacteria[,2:6],bacteria[,1],crv=1000,mode='binary',cutoff=0.5,maxw=3)
#first line of the output
#[1] 0.8933668 0.8088182 0.7766364

#maximising AUROC

set.seed(12398)
#[1] 0.6674006 0.7513193

source(path2)

set.seed(12398)
#first line of the output
#[1] 0.8933668 0.8088182 0.7766364

#maximising AUROC

set.seed(12398)
#[1] 0.6674006 0.7513193

set.seed(12398)
c2<-regr_ind(bacteria[,2:6],bacteria[,1],crv=1000,mode='binary',cutoff=0.5,Rsq=T,maxw=3)
#first line of the output
#[1] 0.8933668 0.8088182 0.7766364

#maximising AUROC

set.seed(12398)
c20<-regr_ind(bacteria[,2:6],bacteria[,1],crv=1000,mode='binary',cutoff=0.5,objfun='roc',Rsq=T,maxw=3)#first line of the output
#[1] 0.6674006 0.7513193

set.seed(12398)
c3<-regr_ind(bacteria[,2:6],bacteria[,1],crv=1000,mode='binary',cutoff=0.5,Rsq=T,marg=0.05,maxw=3)
#first line of the output
#[1] 0.8933668 0.8088182 0.7766364

#maximising AUROC

set.seed(12398)
c30<-regr_ind(bacteria[,2:6],bacteria[,1],crv=1000,mode='binary',cutoff=0.5,objfun='roc',Rsq=T,marg=0.05,maxw=3)#first line of the output
#[1] 0.6674006 0.7513193

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



#Pima.te from the package MASS, sample size 332, 109 less likely outcomes

#maximising accuracy
data(Pima.te,package="MASS")

name<-"Pima.te"
name_p<-"MASS"
name_s<-"bin"
ou<-8
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Pima.te[,1:7])

source(path1)

set.seed(12388)
c<-regr_ind(Pima.te[,1:7],Pima.te[,8],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.7929706 0.6693824 0.7774412

#maximising AUROC

set.seed(12388)
#first line of the output
#[1] 0.8605648 0.8553927

source(path2)

set.seed(12388)
#first line of the output
#[1] 0.7929706 0.6693824 0.7774412

#maximising AUROC

set.seed(12388)
#first line of the output
#[1] 0.8605648 0.8553927


set.seed(12388)
c2<-regr_ind(Pima.te[,1:7],Pima.te[,8],crv=1000,mode='binary',cutoff=0.5,Rsq=T)
#first line of the output
#[1] 0.7929706 0.6693824 0.7774412

#maximising AUROC

set.seed(12388)
c20<-regr_ind(Pima.te[,1:7],Pima.te[,8],crv=1000,mode='binary',cutoff=0.5,objfun='roc',Rsq=T)
#first line of the output
#[1] 0.8605648 0.8553927


set.seed(12388)
c3<-regr_ind(Pima.te[,1:7],Pima.te[,8],crv=1000,mode='binary',cutoff=0.5,Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7929706 0.6693824 0.7774412

#maximising AUROC

set.seed(12388)
c30<-regr_ind(Pima.te[,1:7],Pima.te[,8],crv=1000,mode='binary',cutoff=0.5,objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.8605648 0.8553927

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

#pneumon from the package KMsurv, sample size 3470, 73 less likely outcomes

#maximising accuracy
data(pneumon,package="KMsurv")

name<-"pneumon"
name_p<-"KMsurv"
name_s<-"bin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(pneumon[,2])

source(path1)

set.seed(12385)
c<-regr_ind(pneumon[,c(1,3:12)],pneumon[,2],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.9922742 0.9788761 0.9781787

#maximising AUROC

set.seed(12386)
#first line of the output
#[1] 0.9210679 0.9040106

source(path2)

set.seed(12385)
#first line of the output
#[1] 0.9922742 0.9788761 0.9781787

#maximising AUROC

set.seed(12386)
#first line of the output
#[1] 0.9210679 0.9040106

set.seed(12385)
c2<-regr_ind(pneumon[,c(1,3:12)],pneumon[,2],crv=1000,mode='binary',cutoff=0.5,Rsq=T)
#first line of the output
#[1] 0.9922742 0.9788761 0.9781787

#maximising AUROC

set.seed(12386)
c20<-regr_ind(pneumon[,c(1,3:12)],pneumon[,2],crv=1000,mode='binary',cutoff=0.5,objfun='roc',Rsq=T)
#first line of the output
#[1] 0.9210679 0.9040106

set.seed(12385)
c3<-regr_ind(pneumon[,c(1,3:12)],pneumon[,2],crv=1000,mode='binary',cutoff=0.5,Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9922742 0.9788761 0.9781787

#maximising AUROC

set.seed(12386)
c30<-regr_ind(pneumon[,c(1,3:12)],pneumon[,2],crv=1000,mode='binary',cutoff=0.5,objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9210679 0.9040106

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#KosteckiDillon from the package carData, sample size 4152, 1486 less likely outcomes

#maximising accuracy
data(KosteckiDillon,package="carData")

name<-"KosteckiDillon"
name_p<-"carData"
name_s<-"bin"
ou<-8
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(KosteckiDillon[,c(2:7,9)])

source(path1)

set.seed(1238)
c<-regr_ind(KosteckiDillon[,c(2:7,9)],KosteckiDillon[,8],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.6804784 0.6415457 0.6795529

#maximising AUROC

set.seed(1248)
#first line of the output
#[1] 0.6665914 0.6624804

source(path2)

set.seed(1238)
#first line of the output
#[1] 0.6804784 0.6415457 0.6795529

#maximising AUROC

set.seed(1248)
#first line of the output
#[1] 0.6665914 0.6624804

set.seed(1238)
c2<-regr_ind(KosteckiDillon[,c(2:7,9)],KosteckiDillon[,8],crv=1000,mode='binary',cutoff=0.5,Rsq=T)
#first line of the output
#[1] 0.6804784 0.6415457 0.6795529

#maximising AUROC

set.seed(1248)
c20<-regr_ind(KosteckiDillon[,c(2:7,9)],KosteckiDillon[,8],crv=1000,mode='binary',cutoff=0.5, objfun='roc',Rsq=T)
#first line of the output
#[1] 0.6665914 0.6624804

set.seed(1238)
c3<-regr_ind(KosteckiDillon[,c(2:7,9)],KosteckiDillon[,8],crv=1000,mode='binary',cutoff=0.5,Rsq=T,marg=0.05)
#first line of the output
#[1] 0.6804784 0.6415457 0.6795529

#maximising AUROC

set.seed(1248)
c30<-regr_ind(KosteckiDillon[,c(2:7,9)],KosteckiDillon[,8],crv=1000,mode='binary',cutoff=0.5, objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.6665914 0.6624804

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#burn from the package KMsurv, sample size 154, 48 less likely outcomes

#maximising accuracy


data(burn,package="KMsurv")

name<-"burn"
name_p<-"KMsurv"
name_s<-"bin"
ou<-18
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(burn[,c(2:12,14:16)])

source(path1)

set.seed(1288)
c<-regr_ind(burn[,c(2:12,14:16)],burn[,18],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.7336929 0.6950000 0.6656875

#maximising AUROC

set.seed(1288)
#first line of the output
#[1] 0.7101131 0.5991186

source(path2)

set.seed(1288)
#first line of the output
#[1] 0.7336929 0.6950000 0.6656875

#maximising AUROC

set.seed(1288)
#first line of the output
#[1] 0.7101131 0.5991186

set.seed(1288)
c2<-regr_ind(burn[,c(2:12,14:16)],burn[,18],crv=1000,mode='binary',cutoff=0.5,Rsq=T)
#first line of the output
#[1] 0.7336929 0.6950000 0.6656875

#maximising AUROC

set.seed(1288)
c20<-regr_ind(burn[,c(2:12,14:16)],burn[,18],crv=1000,mode='binary',cutoff=0.5, objfun='roc',Rsq=T)
#first line of the output
#[1] 0.7101131 0.5991186

set.seed(1288)
c3<-regr_ind(burn[,c(2:12,14:16)],burn[,18],crv=1000,mode='binary',cutoff=0.5,Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7336929 0.6950000 0.6656875

#maximising AUROC

set.seed(1288)
c30<-regr_ind(burn[,c(2:12,14:16)],burn[,18],crv=1000,mode='binary',cutoff=0.5, objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7101131 0.5991186

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#beaver from the package datasets, sample size 100, 38 less likely outcomes

#maximising accuracy
data(beaver,package="boot")


name<-"beaver"
name_p<-"boot"
name_s<-"bin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(beaver[,1:3])

source(path1)

set.seed(193)
c<-regr_ind(beaver[,1:3],beaver[,4],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.9643 0.3856 0.9613

#maximising AUROC

set.seed(194)
#first line of the output
#[1] 0.9955691 0.9881696

source(path2)

set.seed(193)
#first line of the output
#[1] 0.9643 0.3856 0.9613

#maximising AUROC

set.seed(194)
#first line of the output
#[1] 0.9955691 0.9881696

set.seed(193)
c2<-regr_ind(beaver[,1:3],beaver[,4],1000,cutoff=0.5,mode='binary',Rsq=T)
#first line of the output
#[1] 0.9643 0.3856 0.9613

#maximising AUROC

set.seed(194)
c20<-regr_ind(beaver[,1:3],beaver[,4],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.9955691 0.9881696

set.seed(193)
c3<-regr_ind(beaver[,1:3],beaver[,4],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9643 0.3856 0.9613

#maximising AUROC

set.seed(194)
c30<-regr_ind(beaver[,1:3],beaver[,4],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9955691 0.9881696

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#Aids2 from the package MASS, sample size 2843, 1082 least likely outcomes

#maximising accuracy
data(Aids2,package="MASS")

name<-"Aids2"
name_p<-"MASS"
name_s<-"bin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Aids2[,c(1,3:4,7)])

source(path1)

set.seed(195)
c<-regr_ind(Aids2[,c(1,3:4,7)],Aids2[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.9659404 0.6196702 0.9654140

#maximising AUROC

set.seed(196)
#first line of the output
#[1] 0.9990844 0.9986687

source(path2)

set.seed(195)
#first line of the output
#[1] 0.9659404 0.6196702 0.9654140

#maximising AUROC

set.seed(196)
#first line of the output
#[1] 0.9990844 0.9986687

set.seed(195)
c2<-regr_ind(Aids2[,c(1,3:4,7)],Aids2[,5],1000,cutoff=0.5,mode='binary',Rsq=T)
#first line of the output
#[1] 0.9659404 0.6196702 0.9654140

#maximising AUROC

set.seed(196)
c20<-regr_ind(Aids2[,c(1,3:4,7)],Aids2[,5],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.9990844 0.9986687

set.seed(195)
c3<-regr_ind(Aids2[,c(1,3:4,7)],Aids2[,5],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9659404 0.6196702 0.9654140

#maximising AUROC

set.seed(196)
c30<-regr_ind(Aids2[,c(1,3:4,7)],Aids2[,5],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9990844 0.9986687

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

#Pima.tr from the package MASS, sample size 200, 68 least likely outcomes

#maximising accuracy
data(Pima.tr,package="MASS")

name<-"Pima.tr"
name_p<-"MASS"
name_s<-"bin"
ou<-8
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Pima.tr[,1:7])

source(path1)

set.seed(197)
c<-regr_ind(Pima.tr[,1:7],Pima.tr[,8],1000,cutoff=0.5,mode='binary',maxw=5)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(198)
#first line of the output
#[1] 0.8377254 0.8263776

source(path2)

set.seed(197)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(198)
#first line of the output
#[1] 0.8377254 0.8263776


set.seed(197)
c2<-regr_ind(Pima.tr[,1:7],Pima.tr[,8],1000,cutoff=0.5,mode='binary',Rsq=T,maxw=5)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(198)
c20<-regr_ind(Pima.tr[,1:7],Pima.tr[,8],1000,cutoff=0.5,mode='binary',Rsq=T,maxw=5)
#first line of the output
#[1] 0.8377254 0.8263776


set.seed(197)
c3<-regr_ind(Pima.tr[,1:7],Pima.tr[,8],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05,maxw=5)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(198)
c30<-regr_ind(Pima.tr[,1:7],Pima.tr[,8],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05,maxw=5)
#first line of the output
#[1] 0.8377254 0.8263776


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#biopsy from the package MASS, sample size 699


data(biopsy,package="MASS")
#removing NA values, reducing sample size to 683, 239 least likely outcomes



datai<-biopsy[,2:11]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"biopsy"
name_p<-"MASS"
name_s<-"bin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(biopsy[ind,2:10])

source(path1)
#maximising accuracy
set.seed(199)
c<-regr_ind(biopsy[ind,2:10],biopsy[ind,11],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(200)
#first line of the output
#[1] 0.8377254 0.8263776

source(path2)

set.seed(199)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(200)
#first line of the output
#[1] 0.8377254 0.8263776

set.seed(199)
c2<-regr_ind(biopsy[ind,2:10],biopsy[ind,11],1000,cutoff=0.5,mode='binary',Rsq=T)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(200)
c20<-regr_ind(biopsy[ind,2:10],biopsy[ind,11],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.8377254 0.8263776

set.seed(199)
c3<-regr_ind(biopsy[ind,2:10],biopsy[ind,11],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(200)
c30<-regr_ind(biopsy[ind,2:10],biopsy[ind,11],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.8377254 0.8263776

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#channing from the package boot, sample size 462, 176 least likely outcomes


data(channing,package="boot")

name<-"channing"
name_p<-"boot"
name_s<-"bin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(channing[,1:4])


source(path1)
#maximising accuracy
set.seed(201)
c<-regr_ind(channing[,1:4],channing[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.6425319 0.6212979 0.6113404

#maximising AUROC

set.seed(202)
#first line of the output
#[1] 0.6696496 0.6570937

source(path2)

set.seed(199)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(200)
#first line of the output
#[1] 0.8377254 0.8263776

set.seed(199)
c2<-regr_ind(channing[,1:4],channing[,5],1000,cutoff=0.5,mode='binary',Rsq=T)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(200)
c20<-regr_ind(channing[,1:4],channing[,5],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.8377254 0.8263776

set.seed(199)
c3<-regr_ind(channing[,1:4],channing[,5],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(200)
c30<-regr_ind(channing[,1:4],channing[,5],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.8377254 0.8263776

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()



name<-"qu_channing"
name_p<-"boot"
name_s<-"bin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cbind(channing[,1],quadr(channing[,2:4])))

qu_channing<-cbind(channing[,1],quadr(channing[,2:4]))
#including the quadratic terms
#maximising accuracy
source(path1)

set.seed(203)
c<-regr_ind(cbind(channing[,1],quadr(channing[,2:4])),channing[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7064468 0.6166596 0.6927021 0.7061277

#maximising AUROC
set.seed(204)
#first line of the output
#[1]  0.7857487 0.7688049 0.7805803

source(path2)

set.seed(203)
#first line of the output
#[1]0.7063830 0.6164468

#maximising AUROC
set.seed(204)
#first line of the output
#[1]0.7858406

set.seed(203)
c2<-regr_ind(cbind(channing[,1],quadr(channing[,2:4])),channing[,5],1000,cutoff=0.5,mode='binary',Rsq=T)
#first line of the output
#[1] 0.7063830 0.6164468

#maximising AUROC
set.seed(204)
c20<-regr_ind(cbind(channing[,1],quadr(channing[,2:4])),channing[,5],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.7858406

set.seed(203)
c3<-regr_ind(cbind(channing[,1],quadr(channing[,2:4])),channing[,5],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7063830 0.6164468

#maximising AUROC
set.seed(204)
c30<-regr_ind(cbind(channing[,1],quadr(channing[,2:4])),channing[,5],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7858406

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#nodal from the package boot, sample size 53, 20 least likely outcomes


data(nodal,package="boot")

name<-"nodal"
name_p<-"MASS"
name_s<-"bin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(nodal[,3:7])


source(path1)
#maximising accuracy
set.seed(205)
c<-regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1)
#first line of the output
#[[1] 0.7375 0.6190 0.7410

#maximising AUROC

set.seed(206)
#first line of the output
#[1] 0.6910533 0.7781677

source(path2)

#maximising accuracy
set.seed(205)
#first line of the output
#[[1] 0.7375 0.6190 0.7410

#maximising AUROC

set.seed(206)
#first line of the output
#[1] 0.6910533 0.7781677

#maximising accuracy
set.seed(205)
c2<-regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1,Rsq=T)
#first line of the output
#[[1] 0.7375 0.6190 0.7410

#maximising AUROC

set.seed(206)
c20<-regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',objfun='roc',maxx=1,Rsq=T)
#first line of the output
#[1] 0.6910533 0.7781677

#maximising accuracy
set.seed(205)
c3<-regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1,Rsq=T,marg=0.05)
#first line of the output
#[[1] 0.7375 0.6190 0.7410

#maximising AUROC

set.seed(206)
c30<-regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',objfun='roc',maxx=1,Rsq=T,marg=0.05)
#first line of the output
#[1] 0.6910533 0.7781677

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


name<-"qu_nodal"
name_p<-"MASS"
name_s<-"bin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quadr(nodal[,3:7]))

qu_nodal<-quadr(nodal[,3:7])

source(path1)

#including the quadratic terms
#maximising accuracy
set.seed(207)
c<-regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1)
#first line of the output
#[1] 0.7758333 0.6286667 0.7241667 0.7178333

#maximising AUROC
set.seed(208)
#first line of the output
#[1] 0.7323181 0.7096399 0.7436603

source(path2)

#maximising accuracy
set.seed(207)
#first line of the output
#[1]0.7756667 0.6283333

#maximising AUROC
set.seed(208)
#first line of the output
#[1] 0.7324064


#maximising accuracy
set.seed(207)
c2<-regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1,Rsq=T)
#first line of the output
#[1]0.7815159 0.6283333

#maximising AUROC
set.seed(208)
c20<-regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',objfun='roc',maxx=1,Rsq=T)
#first line of the output
#[1] 0.7324064


#maximising accuracy
set.seed(207)
c3<-regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1,Rsq=T,marg=0.05)
#first line of the output
#[1] NA 0.6283333

#maximising AUROC
set.seed(208)
c30<-regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',objfun='roc',maxx=1,Rsq=T,marg=0.05)
#first line of the output
#[1] NA


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#urine from the package boot, sample size 79, we use 77, 33 least likely outcomes
data(urine,package="boot")


name<-"urine"
name_p<-"boot"
name_s<-"bin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(urine[setdiff(1:79,c(1,55)),2:7])

source(path1)
#maximising accuracy
set.seed(209)
c<-regr_ind(urine[setdiff(1:79,c(1,55)),2:7],urine[setdiff(1:79,c(1,55)),1],1000,cutoff=0.5,mode='binary',maxw=2)
#first line of the output
#[1] 0.7752381 0.5700000 0.7536250

#maximising AUROC

set.seed(210)
#[1] 0.8468434 0.8437506

source(path2)

set.seed(209)
#first line of the output
#[1] 0.7752381 0.5700000 0.7536250

#maximising AUROC

set.seed(210)
#[1] 0.8468434 0.8437506


set.seed(209)
c2<-regr_ind(urine[setdiff(1:79,c(1,55)),2:7],urine[setdiff(1:79,c(1,55)),1],1000,cutoff=0.5,mode='binary',Rsq=T,maxw=2)
#first line of the output
#[1] 0.7752381 0.5700000 0.7536250

#maximising AUROC

set.seed(210)
#[1] 0.8468434 0.8437506


set.seed(209)
c<-regr_ind(urine[setdiff(1:79,c(1,55)),2:7],urine[setdiff(1:79,c(1,55)),1],1000,cutoff=0.5,mode='binary',maxw=2)
#first line of the output
#[1] 0.7752381 0.5700000 0.7536250

#maximising AUROC

set.seed(210)
#[1] 0.8468434 0.8437506


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#infert from the package datasets, sample size 248, 83 least likely outcomes

name<-"infert"
name_p<-"datasets"
name_s<-"bin"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(infert[,c(1:4,6:8)])

source(path1)

#maximising accuracy
set.seed(211)
c<-regr_ind(infert[,c(1:4,6:8)],infert[,5],1000,cutoff=0.5,mode='binary',maxw=7)
#first line of the output
#[1] 0.7771429 0.6630000 0.7130800

#maximising AUROC

set.seed(212)
#first line of the output
#[1] 0.7674035 0.6975847

source(path2)

#maximising accuracy
set.seed(211)
#first line of the output
#[1] 0.7771429 0.6630000 0.7130800

#maximising AUROC

set.seed(212)
#first line of the output
#[1] 0.7674035 0.6975847


#maximising accuracy
set.seed(211)
c2<-regr_ind(infert[,c(1:4,6:8)],infert[,5],1000,cutoff=0.5,mode='binary',Rsq=T,maxw=7)
#first line of the output
#[1] 0.7771429 0.6630000 0.7130800

#maximising AUROC

set.seed(212)
c20<-regr_ind(infert[,c(1:4,6:8)],infert[,5],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,maxw=7)
#first line of the output
#[1] 0.7674035 0.6975847


#maximising accuracy
set.seed(211)
c3<-regr_ind(infert[,c(1:4,6:8)],infert[,5],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05,maxw=7)
#first line of the output
#[1] 0.7771429 0.6630000 0.7130800

#maximising AUROC

set.seed(212)
c30<-regr_ind(infert[,c(1:4,6:8)],infert[,5],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05,maxw=7)
#first line of the output
#[1] 0.7674035 0.6975847


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#aSAH from the package pROC, sample size 113, 41 less likely outcomes
data(aSAH,package="pROC")

name<-"aSAH"
name_p<-"pROC"
name_s<-"bin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(aSAH[,c(3:7)])

source(path1)
#maximising accuracy
set.seed(213)
c<-regr_ind(aSAH[,c(3:7)],aSAH[,2],1000,cutoff=0.5,mode='binary',maxw=3)
#first line of the output
#[1] 0.8596491 0.6375000 0.7177500

#maximising AUROC

set.seed(214)
#first line of the output
#[1] 0.7832593 0.7782922

source(path2)

set.seed(213)
#first line of the output
#[1] 0.8596491 0.6375000 0.7177500

#maximising AUROC

set.seed(214)
#first line of the output
#[1] 0.7832593 0.7782922

set.seed(213)
c2<-regr_ind(aSAH[,c(3:7)],aSAH[,2],1000,cutoff=0.5,mode='binary',Rsq=T,maxw=3)
#first line of the output
#[1] 0.8596491 0.6375000 0.7177500

#maximising AUROC

set.seed(214)
c20<-regr_ind(aSAH[,c(3:7)],aSAH[,2],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,maxw=3)
#first line of the output
#[1] 0.7832593 0.7782922

set.seed(213)
c3<-regr_ind(aSAH[,c(3:7)],aSAH[,2],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05,maxw=3)
#first line of the output
#[1] 0.8596491 0.6375000 0.7177500

#maximising AUROC

set.seed(214)
c30<-regr_ind(aSAH[,c(3:7)],aSAH[,2],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05,maxw=3)
#first line of the output
#[1] 0.7832593 0.7782922

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#kyphosis from the package rpart, sample size 81, 17 least likely outcomes


data(kyphosis,package="rpart")

name<-"kyphosis"
name_p<-"rpart"
name_s<-"bin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(kyphosis[,2:4])


source(path1)
#maximising accuracy
set.seed(215)
c<-regr_ind(kyphosis[,2:4],kyphosis[,1],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.791 0.791 0.791

#maximising AUROC

set.seed(220)
#[1] 0.8183897 0.8180974

source(path2)

set.seed(215)
#first line of the output
#[1] 0.791 0.791 0.791

#maximising AUROC

set.seed(220)
#[1] 0.8183897 0.8180974


set.seed(215)
c2<-regr_ind(kyphosis[,2:4],kyphosis[,1],1000,cutoff=0.5,mode='binary',Rsq=T)
#first line of the output
#[1] 0.791 0.791 0.791

#maximising AUROC

set.seed(220)
c20<-regr_ind(kyphosis[,2:4],kyphosis[,1],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)#first line of the output
#[1] 0.8183897 0.8180974


set.seed(215)
c3<-regr_ind(kyphosis[,2:4],kyphosis[,1],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.791 0.791 0.791

#maximising AUROC

set.seed(220)
c30<-regr_ind(kyphosis[,2:4],kyphosis[,1],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)#first line of the output
#[1] 0.8183897 0.8180974


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

name<-"qu_kyphosis"
name_p<-"rpart"
name_s<-"bin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quadr(kyphosis[,2:4]))

qu_kyphosis<-quadr(kyphosis[,2:4])

source(path1)
#including the quadratic terms
#maximising accuracy
set.seed(217)
c<-regr_ind(quadr(kyphosis[,2:4]),kyphosis[,1],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1]0.7853333 0.7853333 0.7940000 0.8158889

#maximising AUROC
set.seed(218)
#first line of the output
#[1]  0.8196632 0.7991388 0.8469863

source(path2)

set.seed(217)
#first line of the output
#[1] 0.7854444 0.7854444

#maximising AUROC
set.seed(218)
#first line of the output
#[1] 0.8196632

set.seed(217)
c2<-regr_ind(quadr(kyphosis[,2:4]),kyphosis[,1],1000,cutoff=0.5,mode='binary',Rsq=T)
#first line of the output
#[1] 0.7854444 0.7854444

#maximising AUROC
set.seed(218)
c20<-regr_ind(quadr(kyphosis[,2:4]),kyphosis[,1],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.8196632

set.seed(217)
c3<-regr_ind(quadr(kyphosis[,2:4]),kyphosis[,1],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05)
#first line of the output
#[1] NA 0.7854444

#maximising AUROC
set.seed(218)
c30<-regr_ind(quadr(kyphosis[,2:4]),kyphosis[,1],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] NA

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#stagec from the package rpart, sample size 146


data(stagec,package="rpart")



#removing all the NA values, reducing the sample size to 134, 49 least likely outcomes
datai<-stagec[,2:8]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"stagec"
name_p<-"rpart"
name_s<-"bin"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(stagec[ind,c(1,3:8)])


source(path1)
#maximising accuracy
set.seed(230)
c<-regr_ind(stagec[ind,c(1,3:8)],stagec[ind,2],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.8167857 0.6353571 0.7910714

#maximising AUROC

set.seed(231)
#first line of the output
#[1] 0.8869770 0.8819919

source(path2)

set.seed(230)
#first line of the output
#[1] 0.8167857 0.6353571 0.7910714

#maximising AUROC

set.seed(231)
#first line of the output
#[1] 0.8869770 0.8819919

set.seed(230)
c2<-regr_ind(stagec[ind,c(1,3:8)],stagec[ind,2],1000,cutoff=0.5,mode='binary',Rsq=T)
#first line of the output
#[1] 0.8167857 0.6353571 0.7910714

#maximising AUROC

set.seed(231)
c20<-regr_ind(stagec[ind,c(1,3:8)],stagec[ind,2],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.8869770 0.8819919

set.seed(230)
c3<-regr_ind(stagec[ind,c(1,3:8)],stagec[ind,2],1000,cutoff=0.5,mode='binary',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.8167857 0.6353571 0.7910714

#maximising AUROC

set.seed(231)
c30<-regr_ind(stagec[ind,c(1,3:8)],stagec[ind,2],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.8869770 0.8819919

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#bladder from the package survival, sample size 340, 112 least likely outcomes 


data(bladder,package="survival")

name<-"bladder"
name_p<-"survival"
name_s<-"bin"
ou<-6
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(bladder[,2:5])


source(path1)
#maximising accuracy
set.seed(232)
c<-regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7227941 0.6706471 0.7132353

#maximising AUROC

set.seed(233)
#[1] 0.7714052 0.7630878

source(path2)

#maximising accuracy
set.seed(232)
#first line of the output
#[1] 0.7227941 0.6706471 0.7132353

#maximising AUROC

set.seed(233)
#[1] 0.7714052 0.7630878


#maximising accuracy
set.seed(232)
c2<-regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)
#first line of the output
#[1] 0.7227941 0.6706471 0.7132353

#maximising AUROC

set.seed(233)
c20<-regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#[1] 0.7714052 0.7630878


#maximising accuracy
set.seed(232)
c3<-regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7227941 0.6706471 0.7132353

#maximising AUROC

set.seed(233)
c30<-regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#[1] 0.7714052 0.7630878


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#including the quadratic terms

name<-"qu_bladder"
name_p<-"survival"
name_s<-"bin"
ou<-6
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quadr(bladder[,2:5]))

qu_bladder<-quadr(bladder[,2:5])
#maximising accuracy
source(path1)

set.seed(234)
c<-regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc',maxw=10)
#first line of the output
#[1] 0.9117647 0.6729118 0.7175882

#maximising AUROC
set.seed(235)
#first line of the output
#[1] 0.8093026 0.7703889

source(path2)

set.seed(234)
#first line of the output
#[1] 0.9117647 0.6729118 0.7175882

#maximising AUROC
set.seed(235)
#first line of the output
#[1] 0.8093026 0.7703889

set.seed(234)
c2<-regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,maxw=10)
#first line of the output
#[1] 0.9117647 0.6729118 0.7175882

#maximising AUROC
set.seed(235)
c20<-regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,maxw=10)
#first line of the output
#[1] 0.8093026 0.7703889

set.seed(234)
c3<-regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05,maxw=10)
#first line of the output
#[1] 0.9117647 0.6729118 0.7175882

#maximising AUROC
set.seed(235)
c30<-regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05,maxw=10)
#first line of the output
#[1] 0.8093026 0.7703889

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#bladder2 from the package survival, sample size 178, 66 least likely outcomes


data(bladder,package="survival")

name<-"bladder2"
name_p<-"survival"
name_s<-"bin"
ou<-7
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(bladder2[,2:6])

source(path1)
#maximising accuracy
set.seed(234)
c<-regr_ind(bladder2[,2:6],bladder2[,7],100,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7827778 0.3633333 0.7627778

#maximising AUROC

set.seed(235)
#[1] 0.8026278 0.7786591

source(path2)

set.seed(234)
#first line of the output
#[1] 0.7827778 0.3633333 0.7627778

#maximising AUROC

set.seed(235)
#[1] 0.8026278 0.7786591


set.seed(234)
c2<-regr_ind(bladder2[,2:6],bladder2[,7],100,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)
#first line of the output
#[1] 0.7827778 0.3633333 0.7627778

#maximising AUROC

set.seed(235)
c20<-regr_ind(bladder2[,2:6],bladder2[,7],100,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#[1] 0.8026278 0.7786591


set.seed(234)
c3<-regr_ind(bladder2[,2:6],bladder2[,7],100,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7827778 0.3633333 0.7627778

#maximising AUROC

set.seed(235)
c30<-regr_ind(bladder2[,2:6],bladder2[,7],100,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#[1] 0.8026278 0.7786591



# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

#cancer from the package survival, sample size 228
data(cancer,package="survival")

#removing all the NA values, reducing the sample size to 168, 47 least likely outcomes

datai<-cancer[,3:10]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"cancer"
name_p<-"survival"
name_s<-"bin"
ou<-3
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cancer[ind,4:10])

source(path1)
#maximising accuracy
set.seed(236)
c<-regr_ind(cancer[ind,4:10],cancer[ind,3]-1,1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7818784 0.2757059 0.7563529

#maximising AUROC

set.seed(237)
#first line of the output
#[1] 0.6666318 0.6434980

source(path2)

set.seed(236)
#first line of the output
#[1] 0.7818784 0.2757059 0.7563529

#maximising AUROC

set.seed(237)
#first line of the output
#[1] 0.6666318 0.6434980

set.seed(236)
c2<-regr_ind(cancer[ind,4:10],cancer[ind,3]-1,1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)
#first line of the output
#[1] 0.7818784 0.2757059 0.7563529

#maximising AUROC

set.seed(237)
c20<-regr_ind(cancer[ind,4:10],cancer[ind,3]-1,1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.6666318 0.6434980

set.seed(236)
c3<-regr_ind(cancer[ind,4:10],cancer[ind,3]-1,1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7818784 0.2757059 0.7563529

#maximising AUROC

set.seed(237)
c30<-regr_ind(cancer[ind,4:10],cancer[ind,3]-1,1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.6666318 0.6434980

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#cgd from the package survival, sample size 203, 76 least likely outcomes


data(cgd,package="survival")

name<-"cgd"
name_p<-"survival"
name_s<-"bin"
ou<-16
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(cgd[,c(4:11,13,15)])

source(path1)
#maximising accuracy
set.seed(238)
c<-regr_ind(cgd[,c(4:11,13,15)],cgd[,16],1000,cutoff=0.5,mode='binary',objfun='acc',maxw=6)
#first line of the output
#[1] 0.8139456 0.6245238 0.7419524

#maximising AUROC

set.seed(239)
#[1] 0.8143154 0.7987056

source(path2)

set.seed(238)
#first line of the output
#[1] 0.8139456 0.6245238 0.7419524

#maximising AUROC

set.seed(239)
#[1] 0.8143154 0.7987056


set.seed(238)
c2<-regr_ind(cgd[,c(4:11,13,15)],cgd[,16],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,maxw=6)
#first line of the output
#[1] 0.8139456 0.6245238 0.7419524

#maximising AUROC

set.seed(239)
c20<-regr_ind(cgd[,c(4:11,13,15)],cgd[,16],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,maxw=6)
#[1] 0.8143154 0.7987056


set.seed(238)
c3<-regr_ind(cgd[,c(4:11,13,15)],cgd[,16],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05,maxw=6)
#first line of the output
#[1] 0.8139456 0.6245238 0.7419524

#maximising AUROC

set.seed(239)
c30<-regr_ind(cgd[,c(4:11,13,15)],cgd[,16],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05,maxw=6)
#[1] 0.8143154 0.7987056


# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#colon from the package survival, sample size 1858
data(colon,package="survival")

#removing all the NA values, reducing the sample size to 1822, 911 of each outcomes
datai<-colon[,c(3:9,13:15)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"colon"
name_p<-"survival"
name_s<-"bin"
ou<-16
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(colon[ind,c(3:9,13:15)])

source(path1)
#maximising accuracy
set.seed(240)
c<-regr_ind(colon[ind,c(3:9,13:15)],colon[ind,16]-1,1000,cutoff=0.5,mode='binary',objfun='acc')#first line of the output
#[1] 0.5521858 0.4994481 0.5458689

#maximising AUROC

set.seed(241)
#first line of the output
#[1] 0.5825579 0.5825579

source(path2)

set.seed(240)
#[1] 0.5521858 0.4994481 0.5458689

#maximising AUROC

set.seed(241)
#first line of the output
#[1] 0.5825579 0.5825579

set.seed(240)
c2<-regr_ind(colon[ind,c(3:9,13:15)],colon[ind,16]-1,1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)#first line of the output
#[1] 0.5521858 0.4994481 0.5458689

#maximising AUROC

set.seed(241)
c20<-regr_ind(colon[ind,c(3:9,13:15)],colon[ind,16]-1,1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.5825579 0.5825579

set.seed(240)
c3<-regr_ind(colon[ind,c(3:9,13:15)],colon[ind,16]-1,1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)#first line of the output
#[1] 0.5521858 0.4994481 0.5458689

#maximising AUROC

set.seed(241)
c30<-regr_ind(colon[ind,c(3:9,13:15)],colon[ind,16]-1,1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.5825579 0.5825579

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#crabs from the package MASS, sample size 200, 100 of each outcome


data(crabs,package="MASS")

name<-"crabs"
name_p<-"MASS"
name_s<-"bin"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(crabs[,c(3,5:8)])


source(path1)
#maximising accuracy
set.seed(242)
c<-regr_ind(crabs[,c(3,5:8)],crabs[,1],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.98930 0.49645 0.97920

#maximising AUROC

set.seed(243)
#first line of the output
#[1] 0.9989594 0.9958940

source(path2)

set.seed(242)
#first line of the output
#[1] 0.98930 0.49645 0.97920

#maximising AUROC

set.seed(243)
#first line of the output
#[1] 0.9989594 0.9958940

set.seed(242)
c2<-regr_ind(crabs[,c(3,5:8)],crabs[,1],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)
#first line of the output
#[1] 0.98930 0.49645 0.97920

#maximising AUROC

set.seed(243)
c20<-regr_ind(crabs[,c(3,5:8)],crabs[,1],100,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.9989594 0.9958940

set.seed(242)
c3<-regr_ind(crabs[,c(3,5:8)],crabs[,1],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.98930 0.49645 0.97920

#maximising AUROC

set.seed(243)
c30<-regr_ind(crabs[,c(3,5:8)],crabs[,1],100,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9989594 0.9958940

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#quine from the package MASS, sample size 146, 63 least likely outcomes


data(quine,package="MASS")

name<-"quine"
name_p<-"MASS"
name_s<-"bin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quine[,c(1:3,5)])


source(path1)
#maximising accuracy
set.seed(244)
c<-regr_ind(quine[,c(1:3,5)],quine[,4],1000,cutoff=0.5,mode='binary',objfun='acc',maxw=5)
#first line of the output
#[1] 0.7358025 0.5733333 0.7224000

#maximising AUROC

set.seed(245)
#first line of the output
#[1] 0.8083812 0.7787639

source(path2)

#maximising accuracy
set.seed(244)
#first line of the output
#[1] 0.7358025 0.5733333 0.7224000

#maximising AUROC

set.seed(245)
#first line of the output
#[1] 0.8083812 0.7787639

#maximising accuracy
set.seed(244)
c2<-regr_ind(quine[,c(1:3,5)],quine[,4],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,maxw=5)
#first line of the output
#[1] 0.7358025 0.5733333 0.7224000

#maximising AUROC

set.seed(245)
c20<-regr_ind(quine[,c(1:3,5)],quine[,4],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,maxw=5)
#first line of the output
#[1] 0.8083812 0.7787639

#maximising accuracy
set.seed(244)
c3<-regr_ind(quine[,c(1:3,5)],quine[,4],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05,maxw=5)
#first line of the output
#[1] 0.7358025 0.5733333 0.7224000

#maximising AUROC

set.seed(245)
c30<-regr_ind(quine[,c(1:3,5)],quine[,4],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05,maxw=5)
#first line of the output
#[1] 0.8083812 0.7787639

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#jasa1 from the package survival, sample size 170, 75 least likely outcomes


data(heart,package="survival")

name<-"jasa1"
name_p<-"survival"
name_s<-"bin"
ou<-4
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(jasa1[,c(2:3,5:8)])

source(path1)
#maximising accuracy
set.seed(246)
c<-regr_ind(jasa1[,c(2:3,5:8)],jasa1[,4],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7468235 0.5553529 0.6805882

#maximising AUROC

set.seed(247)
#first line of the output
#[1] 0.8187166 0.7484923

source(path2)

set.seed(246)
#first line of the output
#[1] 0.7468235 0.5553529 0.6805882

#maximising AUROC

set.seed(247)
#first line of the output
#[1] 0.8187166 0.7484923

set.seed(246)
c2<-regr_ind(jasa1[,c(2:3,5:8)],jasa1[,4],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)
#first line of the output
#[1] 0.7468235 0.5553529 0.6805882

#maximising AUROC

set.seed(247)
c20<-regr_ind(jasa1[,c(2:3,5:8)],jasa1[,4],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.8187166 0.7484923

set.seed(246)
c3<-regr_ind(jasa1[,c(2:3,5:8)],jasa1[,4],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7468235 0.5553529 0.6805882

#maximising AUROC

set.seed(247)
c30<-regr_ind(jasa1[,c(2:3,5:8)],jasa1[,4],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.8187166 0.7484923

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#mgus2 from the package survival, sample size 1384

data(mgus2,package="survival")



#removing all the NA values, reducing the sample size to 1338, 400 least likely outcomes
datai<-mgus2[,2:8]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"mgus2"
name_p<-"survival"
name_s<-"bin"
ou<-10
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(mgus2[ind,2:8])

source(path1)
#maximising accuracy
set.seed(248)
c<-regr_ind(mgus2[ind,2:8],mgus2[ind,11],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7952910 0.3013806 0.7897164

#maximising AUROC

set.seed(249)
#first line of the output
#[1] 0.8426302 0.8393732

source(path2)

set.seed(248)
#first line of the output
#[1] 0.7952910 0.3013806 0.7897164

#maximising AUROC

set.seed(249)
#first line of the output
#[1] 0.8426302 0.8393732

set.seed(248)
c2<-regr_ind(mgus2[ind,2:8],mgus2[ind,11],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)
#first line of the output
#[1] 0.7952910 0.3013806 0.7897164

#maximising AUROC

set.seed(249)
c20<-regr_ind(mgus2[ind,2:8],mgus2[ind,11],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.8426302 0.8393732

set.seed(248)
c3<-regr_ind(mgus2[ind,2:8],mgus2[ind,11],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.7952910 0.3013806 0.7897164

#maximising AUROC

set.seed(249)
c30<-regr_ind(mgus2[ind,2:8],mgus2[ind,11],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.8426302 0.8393732

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#nwtco from the package survival, sample size 4028, 571 least likely outcomes
data(nwtco,package="survival")

name<-"nwtco"
name_p<-"survival"
name_s<-"bin"
ou<-6
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(nwtco[,c(2:5,7:8)])


source(path1)
#maximising accuracy
set.seed(250)
c<-regr_ind(nwtco[,c(2:5,7:8)],nwtco[,6],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.9313375 0.8579553 0.9240918

#maximising AUROC

set.seed(251)
#first line of the output
#[1] 0.9664795 0.9584666

source(path2)

#maximising accuracy
set.seed(250)
#first line of the output
#[1] 0.9313375 0.8579553 0.9240918

#maximising AUROC

set.seed(251)
#first line of the output
#[1] 0.9664795 0.9584666

#maximising accuracy
set.seed(250)
c2<-regr_ind(nwtco[,c(2:5,7:8)],nwtco[,6],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)
#first line of the output
#[1] 0.9313375 0.8579553 0.9240918

#maximising AUROC

set.seed(251)
c20<-regr_ind(nwtco[,c(2:5,7:8)],nwtco[,6],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.9664795 0.9584666

#maximising accuracy
set.seed(250)
c3<-regr_ind(nwtco[,c(2:5,7:8)],nwtco[,6],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9313375 0.8579553 0.9240918

#maximising AUROC

set.seed(251)
c30<-regr_ind(nwtco[,c(2:5,7:8)],nwtco[,6],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.9664795 0.9584666

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#retinopathy from the package survival, sample size 394, 155 least likely outcomes
data(retinopathy,package="survival")

name<-"retinopathy"
name_p<-"survival"
name_s<-"bin"
ou<-8
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(retinopathy[,c(2:7,9)])

source(path1)
#maximising accuracy
set.seed(252)
c<-regr_ind(retinopathy[,c(2:7,9)],retinopathy[,8],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.823850 0.606325 0.816175

#maximising AUROC

set.seed(253)
#first line of the output
#[1] 0.8865818 0.8789591

source(path2)
#maximising accuracy
set.seed(252)
#first line of the output
#[1] 0.823850 0.606325 0.816175

#maximising AUROC

set.seed(253)
#first line of the output
#[1] 0.8865818 0.8789591


#maximising accuracy
set.seed(252)
c2<-regr_ind(retinopathy[,c(2:7,9)],retinopathy[,8],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T)
#first line of the output
#[1] 0.823850 0.606325 0.816175

#maximising AUROC

set.seed(253)
c20<-regr_ind(retinopathy[,c(2:7,9)],retinopathy[,8],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T)
#first line of the output
#[1] 0.8865818 0.8789591


#maximising accuracy
set.seed(252)
c3<-regr_ind(retinopathy[,c(2:7,9)],retinopathy[,8],1000,cutoff=0.5,mode='binary',objfun='acc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.823850 0.606325 0.816175

#maximising AUROC

set.seed(253)
c30<-regr_ind(retinopathy[,c(2:7,9)],retinopathy[,8],1000,cutoff=0.5,mode='binary',objfun='roc',Rsq=T,marg=0.05)
#first line of the output
#[1] 0.8865818 0.8789591

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c1[[1]][1],2)),cat("/"),cat(round(c10[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("/"),cat(round(c0[[1]][2],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("/"),cat(round(c0[[1]][3],2)),cat("$ & $"),cat(round(c2[[1]][2],2)),cat("/-"),cat("$ & $"),cat(round(c2[[1]][1],2)),cat("/"),cat(round(c20[[1]][1],2)),cat("$ & $"),cat(round(c3[[1]][1],2)),cat("/"),cat(round(c30[[1]][1],2)),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


############################################################################################

#the following examples are for the mode 'multin'


#iris from the package datasets, sample size 150, 50 of each outcome

name<-"iris"
name_p<-"datasets"
name_s<-"mult"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(iris[,1:4])

source(path1)
set.seed(255)
c<-regr_ind(iris[,1:4],iris[,5],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.9689067 0.2160667 0.9690000

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("$ & $"),cat(round(c[[1]][2],2)),cat("$ & $"),cat("-"),cat("$ & $"),cat("-"),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

#Melanoma from the package MASS, sample size 205, 14 least likely outcomes

data(Melanoma, package="MASS")

name<-"Melanoma"
name_p<-"MASS"
name_s<-"mult"
ou<-2
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(Melanoma[,3:7])

set.seed(258)
c<-regr_ind(Melanoma[,3:7],Melanoma[,2],100,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.2828571 0.6471429 0.1780952

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("$ & $"),cat(round(c[[1]][2],2)),cat("$ & $"),cat("-"),cat("$ & $"),cat("-"),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

#painters from the package MASS, sample size 54, 15 likely outcomes

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

name<-"painters"
name_p<-"MASS"
name_s<-"mult"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(painters[,1:4])

set.seed(259)
c<-regr_ind(painters[,1:4],p,1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.5468333 0.4015000 0.6283333
# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("$ & $"),cat(round(c[[1]][2],2)),cat("$ & $"),cat("-"),cat("$ & $"),cat("-"),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


name<-"q(painters)"
name_p<-"MASS"
name_s<-"mult"
ou<-5
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(quadr(painters[,1:4]))

#quadratic terms
set.seed(301)
c<-regr_ind(quadr(painters[,1:4]),p,1000,cutoff=0.5,mode='multin')

#first line of the output
#[1] 0.5715000 0.3988333 0.5793333
# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("$ & $"),cat(round(c[[1]][2],2)),cat("$ & $"),cat("-"),cat("$ & $"),cat("-"),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()


#solder from the package survival, sample size 720, 240 of each outcome

data(solder, package="rpart")

name<-"solder"
name_p<-"rpart"
name_s<-"mult"
ou<-1
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(solder[,2:6])

set.seed(260)
c<-regr_ind(solder[,2:6],solder[,1],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.5319722 0.2791944 0.5188472

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("$ & $"),cat(round(c[[1]][2],2)),cat("$ & $"),cat("-"),cat("$ & $"),cat("-"),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

#bladder1 from the package survival, sample size 294, 29 least likely outcomes

data(bladder, package="survival")

name<-"bladder1"
name_p<-"survival"
name_s<-"mult"
ou<-8
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(bladder1[,2:7])

bladder1[which(bladder1[,8]==3),8]=2

set.seed(261)
c<-regr_ind(bladder1[,2:7],bladder1[,8],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.8249667 0.6452000 0.7850667

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("$ & $"),cat(round(c[[1]][2],2)),cat("$ & $"),cat("-"),cat("$ & $"),cat("-"),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

#pbc from the package survival, sample size 418

data(pbc, package="survival")



#removing all the NA values, reducing the sample size to 399, 25 least likely outcomes
datai<-pbc[,c(5:6,10:11,13,18:20)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
  ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

name<-"pbc"
name_p<-"survival"
name_s<-"mult"
ou<-3
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(pbc[ind,c(5:6,10:11,13,18:20)])

set.seed(263)
c<-regr_ind(pbc[ind,c(5:6,10:11,13,18:20)],pbc[ind,3],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.7207305 0.5590500 0.7187500

# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("$ & $"),cat(round(c[[1]][2],2)),cat("$ & $"),cat("-"),cat("$ & $"),cat("-"),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()

#transplant from the package survival, sample size 815

data(transplant, package="survival")

#removing all the "withdraw" values, reducing the sample size to 778

ind<-which(transplant[,6]!="withdraw")

#removing all the NA values, reducing the sample size to 760, 66 least likely outcomes
datai<-transplant[ind,1:5]

col=dim(datai)[2]
row=dim(datai)[1]


ind1=1:row
for (i in 1:col){
  ind1=intersect(ind1,which(is.na(datai[,i])==FALSE))
}

name<-"transplant"
name_p<-"survival"
name_s<-"mult"
ou<-6
name_o<-paste0("names(",name,")[",ou,"]")
name_d<-dim(transplant[ind[ind1],1:5])

set.seed(264)
c<-regr_ind(transplant[ind[ind1],1:5],transplant[ind[ind1],6],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.8743307 0.8141316 0.8490000
# sink(paste("Table1.tex"),append=T)
# cat(paste0("\n"))
# cat(paste0(cat("\\texttt{"),cat(name),cat("} (\\bf{"),cat(name_p),cat("}) &\\texttt{"),cat(name_s),cat("} &\\texttt{"),cat(eval(parse(text=name_o))),cat("} &,$"),cat(round(c[[1]][1],2)),cat("$ & $"),cat(round(c[[1]][3],2)),cat("$ & $"),cat(round(c[[1]][4],2)),cat("$ & $"),cat(round(c[[1]][2],2)),cat("$ & $"),cat("-"),cat("$ & $"),cat("-"),cat("$ & $"),cat(name_d[2]),cat("$ &"),cat(name_d[1]),cat("\\"),cat("\\")))
# cat(paste0("\n"))
# sink()
#################

set.seed(258)
c<-regr_ind(USJudgeRatings[,1:11],USJudgeRatings[,12],1000,cutoff=0.5,mode='linear')

set.seed(270)
c<-regr_ind(VerbAgg[,c(2:4,6:9)],VerbAgg[,1],100,cutoff=0.5,mode='linear')
