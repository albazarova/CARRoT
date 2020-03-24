source("carrot_functions_mod_si.r")

library(car)
library(afex)

#Note that the results are subject to a partition which is different every time. 
#Therefore the values obtained by running this code might give values which are slightly different fro those givent in Tables 2-3. 
#However we anticipate that that difference will have an order of magnitude 10^{-2}
#Also the ratio between predictive powers obtained by different methods would stay the same up to the second-third decimal place

#To ensure reproducibility we did not use parallel toolbox, however it would normally significantly speed up the process

#uis from the package quantreg, sample size 575
data(uis,package="quantreg")
set.seed(12399)
regr_ind(uis[,2:9],uis[,10],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 53.974224 1.577012 57.734791 1.624488 55.402586 1.602884

#PBG from the package nlme, sample size 60
data(PBG,package="nlme")
set.seed(12399)
regr_ind(PBG[,2:5],PBG[,1],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 4.528927 1.048533 10.163057 3.559833 4.854390 1.095415

#Rabbit from the package MASS, sample size 60
data(Rabbit,package="MASS")
set.seed(12389)
regr_ind(Rabbit[,2:5],Rabbit[,1],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 4.397715 1.065038 10.197562 3.654520 4.782833 1.110297


#kidrecurr from the package KMsurv, sample size 38
data(kidrecurr,package="KMsurv")
set.seed(12383);regr_ind(kidrecurr[,6:10],kidrecurr[,2],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 82.566500 4.248058 89.520824 4.918722 168.381579 10.774345

#kidrecurr from the package KMsurv, sample size 38, different dependent variable
data(kidrecurr,package="KMsurv")
set.seed(12383);regr_ind(kidrecurr[,6:10],kidrecurr[,4],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 92.706500 3.273108 100.566721 3.743902 104.856589 4.796471

#hodg from the package KMsurv, sample size 43
data(hodg,package="KMsurv")
set.seed(12379)
regr_ind(hodg[,c(1:2,4:5)],hodg[,3],crv=1000,mode='linear',cutoff=0.5)
#first line of the output
#[1] 255.322200 2.497530 445.683189 11.160090 278.132600 4.879435

#bmt from the package KMsurv, sample size 137
data(bmt,package="KMsurv")
set.seed(12369)
regr_ind(bmt[,c(8,10,12:22)],bmt[,2],crv=1000,mode='linear',cutoff=0.5)

#first line of the output
#[1] 550.016571 2.837227 638.521065 11.963174 606.219000 4.473144

#glioma from the package exactRankTests, sample size 37, using 23 patients who died
data(glioma,package="exactRankTests")
ind<-which(glioma$Cens==1)
set.seed(2345)
regr_ind(glioma[ind,c(2:4,7)],glioma[ind,5],crv=1000,mode='linear',cutoff=0.5)

#first line of the output
#[1] 7.7243333 0.4729086 9.9333000 0.7271526 8.3730000 0.4861305

#bfeed from the package KMsurv, sample size 892, using 927 patients who completed breastfeeding
ind<-which(bfeed$delta==1)
set.seed(12359)
regr_ind(bfeed[ind,3:9],bfeed[ind,1],crv=1000,mode='linear',cutoff=0.5)

#first line of the output
#[1] 12.721856 2.736685 13.226956 2.893800 12.833922 2.783218

#esoph from the package datasets, sample size 88
set.seed(99)
regr_ind(esoph[,c(1:3,5)],esoph[,4],crv=1000,mode='linear')

#first line of the output
#[1] 1.3468889 0.5516158 2.0594430 0.6161480 1.3716667 0.7364119

#WeightLoss from the package carData, sample size 34
set.seed(200)
regr_ind(WeightLoss[,1:6],WeightLoss[,7],crv=1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 0.95175000 0.06323496 1.94850000 0.12999509 1.03550000 0.06888105

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
#[1] 137.7483571 0.5939945 193.8927681 0.9112561 142.6912143 0.6187424


#Leinhardt from package carData

#removing NA from the dataset
datai<-Leinhardt[,1:4]
col=dim(datai)[2]
row=dim(datai)[1]
ind=1:row
for (i in 1:col){
 ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}


set.seed(12349)
regr_ind(Leinhardt[ind,c(1,3:4)],Leinhardt[ind,2],crv=1000,mode='linear',cutoff=0.5)
#[1] 43.8677890 0.8345628 64.5493072 1.6867396 46.5616143 0.8350713

#CO2 from the package datasets, sample size 84

set.seed(100)
regr_ind(CO2[,1:4],CO2[,5],1000,mode='linear')

#first line of output
#[1] 4.9474490 0.2432998 9.6012853 0.4852201 10.4823150 0.4325342


#OrchardSprays from the package datasets, sample size 64

set.seed(101)
regr_ind(OrchardSprays[,2:3],OrchardSprays[,1],1000,mode='linear')


#first line of output
#[1] 31.314286 2.673523 31.013088 2.720411 37.264808 3.258941

#Seatbelts from the package datasets, sample size 192
set.seed(102)
regr_ind(cbind(Seatbelts[,2:6],Seatbelts[,8]),Seatbelts[,1],1000,mode='linear')

#first line of output
#[1] 9.18730000 0.07834161 20.40541047 0.17300658 9.32910000 0.07952680


#Theoph from the package datasets, sample size 132
set.seed(103)
regr_ind(Theoph[,2:4],Theoph[,5],1000,mode='linear')


#first line of output
#[1] 2.1906459 0.9973437 2.4213413 1.0937330 2.1906459 0.9973437

#airquality from the package datasets, sample size of those do not have NA in the outcome variable 'Ozone' 116
set.seed(104)
regr_ind(airquality[which(is.na(airquality[,1])==FALSE),2:5],airquality[which(is.na(airquality[,1])==FALSE),1],10000,mode='linear')

#alternatively
#regr_ind(airquality[which(is.na(airquality[,1])==FALSE),2:5],airquality[which(is.na(airquality[,1])==FALSE),1],10000,mode='linear')

#first line of output
#[1] 15.7219764 0.6773550 26.7000994 1.5225084 16.0264012 0.6714827

#airquality from the package datasets, sample size of those do not have NA in the outcome variable 'Ozone' 116
set.seed(105)
regr_ind(airquality[which(is.na(airquality[,1])==FALSE),2:5],airquality[which(is.na(airquality[,1])==FALSE),1],1000,mode='linear')

#first line of output
#[1] 15.7928241 0.6839488 26.2919087 1.5560886 16.0316840 0.6809184

#mtcars from the package datasets, sample size 32

set.seed(106)
regr_ind(mtcars[,2:11],mtcars[,1],1000,cutoff=0.5,mode='linear')


#first line of output
#[1] 2.1552329 0.1066498 4.8406661 0.2577836 2.9056975 0.1496216

#npk from the package datasets, sample size 24

#treating numeric binary factor variables as numbers
set.seed(107)
regr_ind(cbind(npk[,1],apply(as.matrix.noquote(npk),2,as.numeric)[,2:4]),npk[,5],1000,cutoff=0.5,mode='linear')


#first line of output
#[1] 4.62745835 0.08741047 5.21379048 0.09735196 5.21909251 0.09872581

# #quadratic terms for binary variables
# set.seed(108)
# regr_ind(cbind(npk[,1],quadr(apply(as.matrix.noquote(npk),2,as.numeric)[,2:4])),npk[,5],1000,cutoff=0.5,mode='linear')
# 
# #first line of the output
# #[1] 4.55952316 0.08533178 5.10497016 0.09452046 5.14935209 0.09675629



#rock from the package datasets, sample size 48
set.seed(109)
regr_ind(rock[,1:3],rock[,4],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 183.968329 2.151264 407.502100 9.241256 186.370996 2.156599

#the same dataset including quadratic terms
set.seed(110)
regr_ind(quadr(rock[,1:3]),rock[,4],1000,cutoff=0.5,mode='linear')

#first line of output
#[1] 165.266168 1.308128 405.466431 9.417221 194.435099 2.335413

#stackloss from the package datasets, sample size 21
set.seed(111)
regr_ind(stackloss[,1:3],stackloss[,4],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 3.0470000 0.1816492 7.9975556 0.5291671 2.9973333 0.1682876

# #the same dataset including quadratic terms
# set.seed(112)
# regr_ind(quadr(stackloss[,1:3]),stackloss[,4],1000,cutoff=0.5,mode='linear')
# 
# #first line of the output
# #[1] 2.5976667 0.1451716 7.8298704 0.5267106 3.2656667 0.1982156


#swiss from the package datasets, sample size 47
set.seed(113)
regr_ind(swiss[,2:6],swiss[,1],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 6.20163884 0.09184764 9.94272095 0.16130716 6.28664048 0.09420296

#amis from the package boots, sample size 8437
data(amis,package="boot")
set.seed(114)
regr_ind(amis[,2:4],amis[,1],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 4.8559976 0.1329417 5.1433365 0.1409071 4.8658353 0.1331930

#cane from the package boots, sample size 180
data(cane,package="boot")
set.seed(115)
regr_ind(cbind(cane[,1:3],cane[,5]),cane[,4],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 10.691778 1.374301 11.354694 1.517265 10.923201 1.422802

#the same dataset including quadratic terms

# set.seed(116)
# regr_ind(cbind(quadr(cane[,1:3]),cane[,5]),cane[,4],1000,cutoff=0.5,mode='linear')
# 
# #first line of the output
# #[1] 10.338944 1.293314 11.322361 1.495865 10.823490 1.393972



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

set.seed(117)
regr_ind(cbind(Tablrock$tmp[ind],Tablrock$wd[ind],Tablrock$ws[ind],Tablrock$amb[ind],Tablrock$dew[ind],Tablrock$so2[ind],Tablrock$no[ind],Tablrock$no2[ind],Tablrock$nox[ind],Tablrock$co[ind]),Tablrock$ozone[ind],1000,cutoff=0.5,mode='linear')

#first line of the output
#[1] 5.3896613 0.4448215 11.7665488 1.1620248 5.4149032 0.4467527


#Track from the package BSDA, sample size 55
data(Track,package="BSDA")
set.seed(118)
regr_ind(cbind(Track$'100m',Track$'200m',Track$'400m',Track$'800m',Track$'1500m',Track$'3000m'),Track$marathon,1000,mode='linear')

#first line of the output
#[1] 10.42125235 0.05822114 21.47014354 0.11674153 11.88004305 0.06570432

#Undergrad from the package BSDA, sample size 100
data(Undergrad,package="BSDA")
set.seed(119)
regr_ind(cbind(Undergrad$gender,Undergrad$class,Undergrad$gpa,Undergrad$sat),Undergrad$drops,1000,mode='linear')

#first line of the output
#[1] 0.9173000 0.4306075 1.0789600 0.4538941 0.9185000 0.4311526

#Wins from the package BSDA, sample size 12
data(Wins,package="BSDA")
set.seed(120)
regr_ind(cbind(Wins$batavg,Wins$rbi,Wins$stole,Wins$strkout,Wins$caught,Wins$errors,Wins$era),Wins$wins,1000,mode='linear')

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

set.seed(121)
regr_ind(plantTraits[ind,2:10],plantTraits[ind,1],1000,mode='linear')

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

set.seed(122)
regr_ind(plantTraits[ind,11:20],plantTraits[ind,1],1000,mode='linear')

#first line of the output
#[1] 77.98142 104.19454 112.25366 155.15367 85.18822 115.52875


#algae from the package DMwR, sample size 200
data(algae,package="DMwR")
set.seed(123)
regr_ind(algae[,1:11],algae[,12],1000,mode='linear')

#first line of the output
#[1] 6.17750000 0.07962313 8.52790000 0.10816740 6.22368421 0.08066405

#the same dataset with a different predictor
set.seed(124)
regr_ind(algae[,1:11],algae[,13],1000,mode='linear')

#first line of the output
#[1] 6.367486  Inf 7.908480  NaN 6.897259  NaN

#the same dataset with a different predictor
set.seed(125)
regr_ind(algae[,1:11],algae[,14],1000,mode='linear')

#first line of the output
#[1] 4.473589  Inf 4.754987  NaN 4.495800  NaN

#the same dataset with a different predictor
set.seed(126)
regr_ind(algae[,1:11],algae[,15],1000,mode='linear')

#first line of the output
#[1] 1.807220  Inf 2.350031  NaN 2.267672  NaN

#the same dataset with a different predictor
set.seed(127)
regr_ind(algae[,1:11],algae[,16],1000,mode='linear')

#first line of the output
#4.696981  Inf 5.312394  NaN 4.926308  NaN

#economics from the package ggplot2, sample size 478
data(economics,package="ggplot2")
set.seed(128)
regr_ind(as.data.frame(economics[,3:6]),as.data.frame(economics)[,2],1000,mode='linear')

#first line of the output
#[1] 308.6363932 0.1229381 3100.4678496 1.6394895 308.6363932 0.1292587


#mpg from the package ggplot2, sample size 234
data(mpg,package="ggplot2")
set.seed(129)
regr_ind(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]),as.data.frame(mpg)[,8],1000,mode='linear')

#first line of the output
#[1] 1.2280417 0.0702405 3.3483869 0.2119952 5.8805833 0.3936189

#mpg from the package ggplot2, sample size 234
data(mpg,package="ggplot2")
set.seed(129)
regr_ind(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]),as.data.frame(mpg)[,8],1000,mode='linear')

#first line of the output
#[1] 1.2280417 0.0702405 3.3483869 0.2119952 5.8805833 0.3936189

#the same dataset with a different predictor
set.seed(130)
regr_ind(cbind(as.data.frame(mpg)[,1:7],as.data.frame(mpg)[,10:11]),as.data.frame(mpg)[,9],100,mode='linear')

#first line of the output
#[1] 1.52541667 0.06559844 4.96845238 0.23618654 5.73625000 0.26474140

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

set.seed(131)
regr_ind(cbind(as.data.frame(txhousing)[ind,1:3],as.data.frame(txhousing)[ind,7:8]),as.data.frame(txhousing)[ind,4],1000,mode='linear')

#first line of the output
#[1] 125.3734972 0.2444171 649.4811667 4.3310534 129.1523894 0.3886428


# #rtPCR from the package gplots sample size 1672
# data(rtPCR,package="gplots")
# 
# #removing NA values, reducing the sample size to 
# 
# datai<-rtPCR[,c(2,3,6:12)]
# 
# col=dim(datai)[2]
# row=dim(datai)[1]
# 
# 
# ind=1:row
# for (i in 1:col){
# ind=intersect(ind,which(is.na(datai[,i])==FALSE))
# }
# 
# set.seed(132)
# regr_ind(rtPCR[ind,c(2,3,6:11)],rtPCR[ind,12],1000,mode='linear')

## first line of the output
## [1] 0.5099122 1.1373285 0.6383283 2.3024466 377.5877704 1190.2460719




#midwest from the package ggplot2, sample size 400
data(midwest,package="ggplot2")
set.seed(134)
regr_ind(as.data.frame(midwest[,4:20]),as.data.frame(midwest)[,22],1000,mode='linear')

#first line of the output
#[1] 1.50688997 0.01607700 1.81372543 0.01929520 1.60741481 0.01705375

#diamonds from the package ggplot2, sample size 54000
data(diamonds,package="ggplot2")
set.seed(135)
regr_ind(cbind(as.data.frame(diamonds[,1:6]),as.data.frame(diamonds[,8:10])),as.data.frame(diamonds)[,7],1000,mode='linear')

#first line of the output
#[1] 740.0139716 0.2710318 3031.2540127 1.8809806 740.5193003 0.3904769

#environmental from the package lattice, sample size 111

data(environmental,package="lattice")

set.seed(137)
regr_ind(environmental[,2:4],environmental[,1],1000,mode='linear')

#first line of the output
#[1] 16.0594167 0.6832754 26.7794815 1.4994344 16.0594167 0.6832754

#including quadratic terms in the model
set.seed(138)
regr_ind(quadr(environmental[,2:4]),environmental[,1],1000,mode='linear')

#first line of the output
#[1] 13.7926667 0.4695287 26.9547306 1.5016453 14.0320833 0.6760230


#Boston from the package MASS, sample size 506

data(Boston,package="MASS")

set.seed(139)
regr_ind(Boston[,1:13],Boston[,14],1000,mode='linear')

#first line of the output
#[1] 3.3550508 0.1690855 6.6236054 0.3634686 3.3684651 0.1696334

#OME from the package MASS, sample size 1129

data(OME,package="MASS")

set.seed(140)
regr_ind(OME[,2:5],OME[,6]/OME[,7]+0.0000001,1000,mode='linear')

#first line of the output
#[1] 1.808355e-01 3.422046e+05 2.565896e-01 4.328624e+05 1.816264e-01 3.442949e+05

#Rabbit from the package MASS, sample size 60

data(Rabbit,package="MASS")

set.seed(141)
regr_ind(Rabbit[,2:5],Rabbit[,1],1000,mode='linear')

#first line of the output
#[1] 4.428361 1.066375 10.111532 3.639913 4.749548 1.103338

#Traffic from the package MASS, sample size 184
data(Traffic,package="MASS")

set.seed(142)
regr_ind(Traffic[,1:3],Traffic[,4],1000,mode='linear')

#first line of the output
#[1] 6.8250526 0.3722014 6.8997965 0.3832491 6.9378421 0.3792912

#UScereal from the package MASS, sample size 65
data(UScereal,package="MASS")

set.seed(143)
regr_ind(UScereal[,2:11],UScereal[,1],1000,mode='linear')

#first line of the output
#[1] 0.9421429 0.2911630 1.1847635 0.4101309 1.0279971 0.4564103

#UScrime from the package MASS, sample size 47
data(UScrime,package="MASS")

set.seed(144)
regr_ind(UScrime[,1:15],UScrime[,16],1000,mode='linear')

#first line of the output
#[1] 177.9804000 0.2081134 303.7863333 0.3776392 241.0920000 0.3081034


#birthwt from the package MASS, sample size 189

data(birthwt,package="MASS")

set.seed(145)
regr_ind(birthwt[,1:9],birthwt[,10],1000,mode='linear')
#first line of the output
#[1] 361.5716842 0.1368641 591.8646232 0.2411088 364.9017368 0.1394650

#the same dataset for a different set of predictors
set.seed(150)
regr_ind(birthwt[,2:9],birthwt[,10],1000,mode='linear')
#first line of the output
#[1] 539.9145263 0.2168056 592.9032269 0.2426130 561.5234737 0.2269350


#cabbages from the package MASS, sample size 60

data(cabbages,package="MASS")

set.seed(147)
regr_ind(cabbages[,1:3],cabbages[,4],1000,mode='linear')
#first line of the output
#[1] 4.92483333 0.08694499 8.55504938 0.15202000 4.99566667 0.08812669



#coop from the package MASS, sample size 252

data(coop,package="MASS")

set.seed(148)
regr_ind(coop[,1:3],coop[,4],1000,mode='linear')
#first line of the output
#[1] 0.2443722 0.3180667 1.7051254 2.3567321 0.3152603 0.3451214

#cpus from the package MASS, sample size 209

data(cpus,package="MASS")

set.seed(149)
regr_ind(cpus[,2:7],cpus[,8],1000,mode='linear')
#first line of the output
#[1] 40.5054762 0.5105281 96.0344352 2.1915047 41.1369524 0.7803790


#epil from the package MASS, sample size 236
data(epil,package="MASS")

set.seed(151)
regr_ind(epil[,2:9],epil[,1],1000,mode='linear')
#first line of the output
#[1] 4.311750 0.721548 7.130709 1.517481 4.312125 0.721548


# #fgl from the package MASS, sample size 214
# data(fgl,package="MASS")
# 
# set.seed(152)
# regr_ind(fgl[,1:9],fgl[,10],1000,mode='linear')
# #first line of the output
# #[1] 0.8565455  Inf 1.1145000  NaN 0.9047273  NaN

#housing from the package MASS, sample size 72
data(housing,package="MASS")

set.seed(153)
regr_ind(housing[,2:5],housing[,1],1000,mode='linear')
#first line of the output
#[1] 0.6647500 0.2321429 0.6826602 0.2753906 1.0000000  NaN

#nlschools from the package MASS, sample size 2287

data(nlschools,package="MASS")

set.seed(155)
regr_ind(nlschools[,2:6],nlschools[,1],1000,mode='linear')

#first line of the output
#[1] 5.2521921 0.1463301 7.3639437 0.2123014 5.2521921 0.1463301

#nlschools from the package MASS, sample size 30

data(immer,package="MASS")

set.seed(154)
regr_ind(immer[,1:3],immer[,4],1000,mode='linear')

#first line of the output
#[1] 17.3855404 0.1991453 19.9225309 0.2285893 13.9538488 0.1607105

#oats from the package MASS, sample size 72
data(oats,package="MASS")

set.seed(156)
regr_ind(oats[,1:3],oats[,4],1000,mode='linear')

#first line of the output
#[1] 17.7446250 0.1776077 22.0894336 0.2352328 14.3116250 0.1467353

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

set.seed(158)
regr_ind(petrol[,1:5],petrol[,6],1000,mode='linear')

#first line of the output
#[1] 2.096233 0.167669 9.335995 0.844543 5.001481 0.381677

#quine from the package MASS, sample size 146
data(quine,package="MASS")

set.seed(159)
regr_ind(quine[,1:4],quine[,5],1000,mode='linear')

#first line of the output
#[1] 11.591067 1.515183 12.496293 1.691373 11.936733 1.621413

#road from the package MASS, sample size 26
data(road,package="MASS")

set.seed(160)
regr_ind(road[,3:6],road[,2],1000,mode='linear')

#first line of the output
#[1] 67.4353333 0.5127998 129.5784348 1.8690579 91.7740000 0.6430287

# #including quadratic terms
# 
# set.seed(161)
# regr_ind(quadr(road[,3:6]),road[,2],1000,mode='linear')
# 
# #first line of the output
# #[1] 68.4713333 0.3395833 128.3834783 1.7649064 128.8740000 1.0107998


#Gasoline from the package nlme, sample size 32
data(Gasoline,package="nlme")

set.seed(162)
regr_ind(Gasoline[,2:6],Gasoline[,1],1000,mode='linear')

#first line of the output
#[1] 2.1174769 0.1653246 9.4245196 0.8366709 5.0017443 0.3817855


# #including quadratic terms
# 
# set.seed(163)
# regr_ind(quadr(apply(as.matrix.noquote(Gasoline),2,as.numeric)[,2:6]),Gasoline[,1],1000,mode='linear')
# 
# #first line of the output
# #[1] 2.0699387 0.1324884 9.3715661 0.8228121 2.5968704 0.1801361

#MathAchieve from the package nlme, sample size 7185
data(MathAchieve,package="nlme")

set.seed(164)
regr_ind(MathAchieve[,c(2:4,6)],MathAchieve[,5],1000,mode='linear')

#first line of the output
#[1] 5.123259 2.340082 5.823388 2.873735 5.123259 2.340082


#MathAchieve from the package nlme, sample size 160
data(MathAchSchool,package="nlme")

set.seed(165)
regr_ind(MathAchSchool[,c(2:4,6:7)],MathAchSchool[,5],1000,mode='linear')

#first line of the output
#[1] 0.7069805 1.0289295 0.7801860 1.1483394 0.7137737 6.3802130

# #including quadratic terms
# set.seed(166)
# regr_ind(cbind(quadr(cbind(MathAchSchool[,c(2,4,7)],as.numeric(levels(MathAchSchool[,6])[MathAchSchool[,6]]))),MathAchSchool[,3]),MathAchSchool[,5],1000,mode='linear')
# #first line of the output
# #[1] 0.7149502 1.0275090 0.7909486 1.1325293 0.7375928 5.3152996

#Milk from the package nlme, sample size 1337
data(Milk,package="nlme")

set.seed(167)
regr_ind(Milk[,2:4],Milk[,1],1000,mode='linear')

#first line of the output
#[1] 0.21983135 0.06456367 0.26308696 0.07763502 0.22031652 0.06472208

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
#[1] 4.7692434 0.7259111 5.1629820 0.9969036 7.1565476 1.6221474

#including the quadratic terms
set.seed(169)
regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,6],1000,mode='linear')

#first line of the output
#[1] 3.9110456 0.6058816 5.2573102 0.9895885 3.9444009 0.7747142

#same dataset, different outcome

#removing the NA values, reducing the sample size to 155
datai<-Phenobarb[,c(2:5,7)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
 ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

set.seed(171)
regr_ind(Phenobarb[ind,2:5],Phenobarb[ind,7],1000,mode='linear')
#first line of the output
#[1] 6.3691385 0.3040755 6.6053284 0.3048933 6.7003913 0.3503109

#including quadratic terms
set.seed(172)
regr_ind(cbind(quadr(cbind(Phenobarb[ind,c(2,5)],as.numeric(levels(Phenobarb[ind,3])[Phenobarb[ind,3]]))),Phenobarb[ind,4]),Phenobarb[ind,7],1000,mode='linear')

#first line of the output
#[1] 4.4508046 0.1951310 6.6272059 0.3035403 6.8693454 0.3585367

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

set.seed(170)
regr_ind(Quinidine[ind,c(2,6:14)],Quinidine[ind,4],1000,mode='linear')

#first line of the output
#[1] 51.5763301 0.2306375 52.6001954 0.2350661 52.1702524 0.2342444

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

set.seed(173)
regr_ind(Remifentanil[ind,c(3,5:12)],Remifentanil[ind,4],1000,mode='linear')

#first line of the output
#[1] 12.296073 4.561360 23.266024 18.717709 12.318301 4.693894


#Soybean from the package nlme, sample size 412
data(Soybean,package="nlme")

set.seed(174)
regr_ind(Soybean[,2:4],Soybean[,5],1000,mode='linear')

#first line of the output
#[1] 2.096382 4.459741 5.884695 13.218127 2.112355 5.278725

#Wheat from the package nlme, sample size 48
data(Wheat,package="nlme")

set.seed(175)
regr_ind(cbind(as.numeric(Wheat[,1]),Wheat[,2:3]),Wheat[,4],1000,mode='linear')

#first line of the output
#[1] 2.0589174 0.2859348 3.2210669 0.5015642 2.1082381 0.2904173

#including quadratic terms
set.seed(176)
regr_ind(quadr(cbind(as.numeric(levels(Wheat[,1])[Wheat[,1]]),Wheat[,2:3])),Wheat[,4],1000,mode='linear')

#first line of the output
#[1] 1.0809070 0.1567015 3.1841391 0.5110255 1.7519201 0.2620687


#Wheat2 from the package nlme, sample size 224
data(Wheat2,package="nlme")

set.seed(177)
regr_ind(cbind(as.numeric(Wheat2[,1]),Wheat2[,c(2,4:5)]),Wheat2[,3],1000,mode='linear')

#first line of the output
#[1] 4.5363964 0.4078296 5.4164956 0.5704332 4.5514874 0.4078296

#including quadratic terms
set.seed(178)
regr_ind(cbind(quadr(cbind(as.numeric(levels(Wheat2[,1])[Wheat2[,1]]),Wheat2[,4:5])),Wheat2[,2]),Wheat2[,3],1000,mode='linear',parallel=TRUE,cores=7)

#first line of the output
#[1] 4.0864935 0.3457878 5.4752173 0.5979057 4.1904725 0.3502343


#tips from the package nlme, sample size 244
data(tips,package="reshape2")

set.seed(180)
regr_ind(tips[,c(1,3:7)],tips[,2],1000,mode='linear')

#first line of the output
#[1] 0.7505459 0.2792391 1.0447381 0.4177951 0.7516984 0.2797217

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

set.seed(181)
regr_ind(car.test.frame[ind,c(2,4:8)],car.test.frame[ind,1],1000,mode='linear')
#first line of the output
#[1] 2036.7983333 0.1554235 3184.0909012 0.2843645 4295.5801667 0.3630200



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

set.seed(182)
regr_ind(cu.summary[ind,2:5],cu.summary[ind,1],100,mode='linear')
#first line of the output
#[1] 2.432976e+03 1.879221e-01 3.255523e+03 2.888707e-01 1.382275e+04 1.289736e+00


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

set.seed(184)
regr_ind(cancer[ind,c(1,3:10)],cancer[ind,2],1000,mode='linear')
#first line of the output
#[1] 158.276059 1.659907 162.654458 1.820468 165.347934 1.779803

#cgd from the package survival, sample size 117
data(cgd,package="survival")


set.seed(185)
regr_ind(cgd[,c(13,4:12)],cgd[,14],1000,mode='linear')
#first line of the output
#[1] 0.3355238 0.1192488 0.8477666 0.5411852 0.3622381 0.1324598

#flchain from the package survival, sample size 7874
data(flchain,package="survival")

datai<-flchain[,1:9]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
 ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

set.seed(186)
regr_ind(flchain[ind,c(1:8)],flchain[ind,9]+0.0000000001,1000,mode='linear')
#first line of the output
#[1] 9.029672e+02 5.962283e+09 1.196929e+03 1.715029e+10 9.037316e+02 6.383050e+09

#the same dataset, different predictors

#removing NA values reducing the sample size to 1962
datai<-flchain[,c(1:9,11)]

col=dim(datai)[2]
row=dim(datai)[1]


ind=1:row
for (i in 1:col){
 ind=intersect(ind,which(is.na(datai[,i])==FALSE))
}

set.seed(187)
regr_ind(flchain[ind,c(11,1:8)],flchain[ind,9]+0.0000000001,1000,mode='linear')

#first line of the output
#[1] 1.074215e+03 1.638743e+10 1.193699e+03 3.333615e+10 1.074619e+03 1.779234e+10

#logan from the package survival, sample size 838
data(logan,package="survival")


set.seed(188)
regr_ind(logan[,c(1,2,4)],logan[,3],1000,mode='linear')
#first line of the output
#[1] 1.5919762 0.1332218 2.2493278 0.1842479 1.5936429 0.1333967

#retinopathy from the package survival, sample size 394
data(retinopathy,package="survival")


set.seed(189)
regr_ind(retinopathy[,2:8],retinopathy[,9],1000,mode='linear')
#first line of the output
#[1] 1.1269000 0.1243687 1.1874955 0.1312497 1.1564919 0.1282755


set.seed(190)
regr_ind(Leinhardt[which(is.na(Leinhardt[,2])==F),c(1,3:4)],Leinhardt[which(is.na(Leinhardt[,2])==F),2],crv=1000,mode='linear')
#[1] 43.2117870 0.8478157 63.7743998 1.7193571

set.seed(2000)
regr_ind(quadr(cbind(Wong[,c(2:3,5)],as.numeric(Wong[,4])-1)),Wong[,7],crv=1000,mode='linear')
#[1] 11.4053529 0.1230318 11.5805597 0.1248972


# #car90 from the package rpart, sample size 838
# data(car90,package="rpart")
# 
# datai<-car90[,c(1:3,5:7,9:14,17:20,22,24:26,28:34)]
# 
# col=dim(datai)[2]
# row=dim(datai)[1]
# 
# 
# ind=1:row
# for (i in 1:col){
# ind=intersect(ind,which(is.na(datai[,i])==FALSE))
# }
# 
# set.seed(190)
# regr_ind(car90[ind,c(1:3,5:7,9:14,18:20,22,24:26,28:34)],car90[ind,17],1000,mode='linear')
# #first line of the output
# #[1] 1.5919762 0.1332218 2.2493278 0.1842479 1.5936429 0.1333967
# 
# 
# #bdf from the package nlme, sample size 2287
# data(bdf,package="nlme")
# 
# 
# set.seed(191)
# regr_ind(bdf[,5:25],bdf[,3],1000,mode='linear')
# #first line of the output
# #[1] 1.1269000 0.1243687 1.1874955 0.1312497 1.1564919 0.1282755
# 
# set.seed(192)
# regr_ind(bdf[,5:25],bdf[,4],1000,mode='linear')
# #first line of the output
# #[1] 1.1269000 0.1243687 1.1874955 0.1312497 1.1564919 0.1282755



############################################################################################

#the following examples are for the mode 'binary'

#bacteria from the package MASS, sample size 220, 43 less likely outcomes

#maximising accuracy
data(bacteria,package="MASS")

set.seed(12398)
a<-regr_ind(bacteria[,2:6],bacteria[,1],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.8933668 0.8088182 0.7766364

#maximising AUROC

set.seed(12398)
regr_ind(bacteria[,2:6],bacteria[,1],crv=1000,mode='binary',cutoff=0.5,objfun='roc')#first line of the output
#[1] 0.6674006 0.7513193



#Pima.te from the package MASS, sample size 332, 109 less likely outcomes

#maximising accuracy
data(Pima.te,package="MASS")

set.seed(12388)
regr_ind(Pima.te[,1:7],Pima.te[,8],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.7929706 0.6693824 0.7774412

#maximising AUROC

set.seed(12388)
regr_ind(Pima.te[,1:7],Pima.te[,8],crv=1000,mode='binary',cutoff=0.5,objfun='roc')
#first line of the output
#[1] 0.8605648 0.8553927


#pneumon from the package KMsurv, sample size 3470, 73 less likely outcomes

#maximising accuracy
data(pneumon,package="KMsurv")

set.seed(12385)
regr_ind(pneumon[,c(1,3:12)],pneumon[,2],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.9922742 0.9788761 0.9781787

#maximising AUROC

set.seed(12386)
regr_ind(pneumon[,c(1,3:12)],pneumon[,2],crv=1000,mode='binary',cutoff=0.5,objfun='roc')
#first line of the output
#[1] 0.9210679 0.9040106

#KosteckiDillon from the package carData, sample size 4152, 1486 less likely outcomes

#maximising accuracy
data(KosteckiDillon,package="carData")

set.seed(1238)
regr_ind(KosteckiDillon[,c(2:7,9)],KosteckiDillon[,8],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.6804784 0.6415457 0.6795529

#maximising AUROC

set.seed(1248)
regr_ind(KosteckiDillon[,c(2:7,9)],KosteckiDillon[,8],crv=1000,mode='binary',cutoff=0.5, objfun='roc')
#first line of the output
#[1] 0.6665914 0.6624804

#burn from the package KMsurv, sample size 154, 48 less likely outcomes

#maximising accuracy
data(burn,package="KMsurv")

set.seed(1288)
regr_ind(burn[,c(2:12,14:16)],burn[,18],crv=1000,mode='binary',cutoff=0.5)
#first line of the output
#[1] 0.7336929 0.6950000 0.6656875

#maximising AUROC

set.seed(1288)
regr_ind(burn[,c(2:12,14:16)],burn[,18],crv=1000,mode='binary',cutoff=0.5, objfun='roc')
#first line of the output
#[1] 0.7101131 0.5991186

#beaver from the package datasets, sample size 100, 38 less likely outcomes

#maximising accuracy
data(beaver,package="boot")

set.seed(193)
regr_ind(beaver[,1:3],beaver[,4],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.9643 0.3856 0.9613

#maximising AUROC

set.seed(194)
regr_ind(beaver[,1:3],beaver[,4],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.9955691 0.9881696


#Aids2 from the package MASS, sample size 2843, 1082 least likely outcomes

#maximising accuracy
data(Aids2,package="MASS")

set.seed(195)
regr_ind(Aids2[,c(1,3:4,7)],Aids2[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.9659404 0.6196702 0.9654140

#maximising AUROC

set.seed(196)
regr_ind(Aids2[,c(1,3:4,7)],Aids2[,5],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.9990844 0.9986687


#Pima.tr from the package MASS, sample size 200, 68 least likely outcomes

#maximising accuracy
data(Pima.tr,package="MASS")

set.seed(197)
regr_ind(Pima.tr[,1:7],Pima.tr[,8],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(198)
regr_ind(Pima.tr[,1:7],Pima.tr[,8],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.8377254 0.8263776


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

#maximising accuracy
set.seed(199)
regr_ind(biopsy[ind,2:10],biopsy[ind,11],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7804657 0.6538500 0.7530000

#maximising AUROC

set.seed(200)
regr_ind(Pima.tr[,1:7],Pima.tr[,8],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.8377254 0.8263776

#channing from the package boot, sample size 462, 176 least likely outcomes


data(channing,package="boot")

#maximising accuracy
set.seed(201)
regr_ind(channing[,1:4],channing[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.6425319 0.6212979 0.6113404

#maximising AUROC

set.seed(202)
regr_ind(channing[,1:4],channing[,5],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.6696496 0.6570937

#including the quadratic terms
#maximising accuracy
set.seed(203)
regr_ind(cbind(channing[,1],quadr(channing[,2:4])),channing[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7054681 0.6206383 0.6912766

#maximising AUROC
set.seed(204)
regr_ind(cbind(channing[,1],quadr(channing[,2:4])),channing[,5],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.7820982 0.7657714


#nodal from the package boot, sample size 53, 20 least likely outcomes


data(nodal,package="boot")

#maximising accuracy
set.seed(205)
regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1)
#first line of the output
#[[1] 0.7375 0.6190 0.7410

#maximising AUROC

set.seed(206)
regr_ind(nodal[,3:7],nodal[,2],1000,cutoff=0.5,mode='binary',objfun='roc',maxx=1)
#first line of the output
#[1] 0.6910533 0.7781677

#including the quadratic terms
#maximising accuracy
set.seed(207)
regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',maxx=1)
#first line of the output
#[1] 0.7705 0.6210 0.7135

#maximising AUROC
set.seed(208)
regr_ind(quadr(nodal[,3:7]),nodal[,2],1000,cutoff=0.5,mode='binary',objfun='roc',maxx=1)
#first line of the output
#[1] 0.7281709 0.7003796

#urine from the package boot, sample size 79, we use 77, 33 least likely outcomes
data(urine,package="boot")

#maximising accuracy
set.seed(209)
regr_ind(urine[setdiff(1:79,c(1,55)),2:7],urine[setdiff(1:79,c(1,55)),1],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7752381 0.5700000 0.7536250

#maximising AUROC

set.seed(210)
regr_ind(urine[setdiff(1:79,c(1,55)),2:7],urine[setdiff(1:79,c(1,55)),1],1000,cutoff=0.5,mode='binary',objfun='roc')#first line of the output
#[1] 0.8468434 0.8437506

#infert from the package datasets, sample size 248, 83 least likely outcomes

#maximising accuracy
set.seed(211)
regr_ind(infert[,c(1:4,6:8)],infert[,5],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7771429 0.6630000 0.7130800

#maximising AUROC

set.seed(212)
regr_ind(infert[,c(1:4,6:8)],infert[,5],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.7674035 0.6975847

#aSAH from the package pROC, sample size 113, 41 less likely outcomes
data(aSAH,package="pROC")

#maximising accuracy
set.seed(213)
regr_ind(aSAH[,c(3:7)],aSAH[,2],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.8596491 0.6375000 0.7177500

#maximising AUROC

set.seed(214)
regr_ind(aSAH[,c(3:7)],aSAH[,2],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.7832593 0.7782922


#kyphosis from the package rpart, sample size 81, 17 least likely outcomes


data(kyphosis,package="rpart")

#maximising accuracy
set.seed(215)
regr_ind(kyphosis[,2:4],kyphosis[,1],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.791 0.791 0.791

#maximising AUROC

set.seed(220)
regr_ind(kyphosis[,2:4],kyphosis[,1],1000,cutoff=0.5,mode='binary',objfun='roc')#first line of the output
#[1] 0.8183897 0.8180974

#including the quadratic terms
#maximising accuracy
set.seed(217)
regr_ind(quadr(kyphosis[,2:4]),kyphosis[,1],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.7864444 0.7864444 0.7948889

#maximising AUROC
set.seed(218)
regr_ind(quadr(kyphosis[,2:4]),kyphosis[,1],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.8226104 0.8007378

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

#maximising accuracy
set.seed(230)
regr_ind(stagec[ind,c(1,3:8)],stagec[ind,2],1000,cutoff=0.5,mode='binary')
#first line of the output
#[1] 0.8167857 0.6353571 0.7910714

#maximising AUROC

set.seed(231)
regr_ind(stagec[ind,c(1,3:8)],stagec[ind,2],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.8869770 0.8819919

#bladder from the package survival, sample size 340, 112 least likely outcomes 


data(bladder,package="survival")

#maximising accuracy
set.seed(232)
regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7227941 0.6706471 0.7132353

#maximising AUROC

set.seed(233)
regr_ind(bladder[,2:5],bladder[,6],1000,cutoff=0.5,mode='binary',objfun='roc')
#[1] 0.7714052 0.7630878

#including the quadratic terms
#maximising accuracy
set.seed(234)
regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.9117647 0.6729118 0.7175882

#maximising AUROC
set.seed(235)
regr_ind(quadr(bladder[,2:5]),bladder[,6],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.8093026 0.7703889

#bladder2 from the package survival, sample size 178, 66 least likely outcomes


data(bladder,package="survival")

#maximising accuracy
set.seed(234)
regr_ind(bladder2[,2:6],bladder2[,7],100,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7827778 0.3633333 0.7627778

#maximising AUROC

set.seed(235)
regr_ind(bladder2[,2:6],bladder2[,7],100,cutoff=0.5,mode='binary',objfun='roc')
#[1] 0.8026278 0.7786591



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

#maximising accuracy
set.seed(236)
regr_ind(cancer[ind,4:10],cancer[ind,3]-1,1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7818784 0.2757059 0.7563529

#maximising AUROC

set.seed(237)
regr_ind(cancer[ind,4:10],cancer[ind,3]-1,1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.6666318 0.6434980


#cgd from the package survival, sample size 203, 76 least likely outcomes


data(cgd,package="survival")

#maximising accuracy
set.seed(238)
regr_ind(cgd[,c(4:11,13,15)],cgd[,16],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.8139456 0.6245238 0.7419524

#maximising AUROC

set.seed(239)
regr_ind(cgd[,c(4:11,13,15)],cgd[,16],1000,cutoff=0.5,mode='binary',objfun='roc')
#[1] 0.8143154 0.7987056

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

#maximising accuracy
set.seed(240)
regr_ind(colon[ind,c(3:9,13:15)],colon[ind,16]-1,1000,cutoff=0.5,mode='binary',objfun='acc')#first line of the output
#[1] 0.5521858 0.4994481 0.5458689

#maximising AUROC

set.seed(241)
regr_ind(colon[ind,c(3:9,13:15)],colon[ind,16]-1,1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.5825579 0.5825579

#crabs from the package MASS, sample size 200, 100 of each outcome


data(crabs,package="MASS")

#maximising accuracy
set.seed(242)
regr_ind(crabs[,c(3,5:8)],crabs[,1],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.98930 0.49645 0.97920

#maximising AUROC

set.seed(243)
regr_ind(crabs[,c(3,5:8)],crabs[,1],100,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.9989594 0.9958940


#quine from the package MASS, sample size 146, 63 least likely outcomes


data(quine,package="MASS")

#maximising accuracy
set.seed(244)
regr_ind(quine[,c(1:3,5)],quine[,4],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7358025 0.5733333 0.7224000

#maximising AUROC

set.seed(245)
regr_ind(quine[,c(1:3,5)],quine[,4],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.8083812 0.7787639

#jasa1 from the package survival, sample size 170, 75 least likely outcomes


data(heart,package="survival")

#maximising accuracy
set.seed(246)
regr_ind(jasa1[,c(2:3,5:8)],jasa1[,4],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7468235 0.5553529 0.6805882

#maximising AUROC

set.seed(247)
regr_ind(jasa1[,c(2:3,5:8)],jasa1[,4],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.8187166 0.7484923


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

#maximising accuracy
set.seed(248)
regr_ind(mgus2[ind,2:8],mgus2[ind,10],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.7952910 0.3013806 0.7897164

#maximising AUROC

set.seed(249)
regr_ind(mgus2[ind,2:8],mgus2[ind,10],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.8426302 0.8393732


#nwtco from the package survival, sample size 4028, 571 least likely outcomes
data(nwtco,package="survival")

#maximising accuracy
set.seed(250)
regr_ind(nwtco[,c(2:5,7:8)],nwtco[,6],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.9313375 0.8579553 0.9240918

#maximising AUROC

set.seed(251)
regr_ind(nwtco[,c(2:5,7:8)],nwtco[,6],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.9664795 0.9584666

#retinopathy from the package survival, sample size 394, 155 least likely outcomes
data(retinopathy,package="survival")

#maximising accuracy
set.seed(252)
regr_ind(retinopathy[,c(2:7,9)],retinopathy[,8],1000,cutoff=0.5,mode='binary',objfun='acc')
#first line of the output
#[1] 0.823850 0.606325 0.816175

#maximising AUROC

set.seed(253)
regr_ind(retinopathy[,c(2:7,9)],retinopathy[,8],1000,cutoff=0.5,mode='binary',objfun='roc')
#first line of the output
#[1] 0.8865818 0.8789591

############################################################################################

#the following examples are for the mode 'multin'


#iris from the package datasets, sample size 150, 50 of each outcome

set.seed(255)
regr_ind(iris[,1:4],iris[,5],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.9689067 0.2160667 0.9690000

#Melanoma from the package MASS, sample size 205, 14 least likely outcomes

data(Melanoma, package="MASS")

set.seed(258)
regr_ind(Melanoma[,3:7],Melanoma[,2],100,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.2828571 0.6471429 0.1780952

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

set.seed(259)
regr_ind(painters[,1:4],p,1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.5468333 0.4015000 0.6283333

#quadratic terms
set.seed(301)
regr_ind(quadr(painters[,1:4]),p,1000,cutoff=0.5,mode='multin')

#first line of the output
#[1] 0.5715000 0.3988333 0.5793333

#solder from the package survival, sample size 720, 240 of each outcome

data(solder, package="rpart")

set.seed(260)
regr_ind(solder[,2:6],solder[,1],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.5319722 0.2791944 0.5188472

#bladder1 from the package survival, sample size 294, 29 least likely outcomes

data(bladder, package="survival")

bladder1[which(bladder1[,8]==3),8]=2

set.seed(261)
regr_ind(bladder1[,2:7],bladder1[,8],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.8249667 0.6452000 0.7850667

# #colon from the package survival, sample size 1855
# 
# data(colon, package="survival")
# 
# #removing all the NA values, reducing the sample size to 1776
# datai<-colon[,c(3:9,11)]
# 
# col=dim(datai)[2]
# row=dim(datai)[1]
# 
# 
# ind=1:row
# for (i in 1:col){
# ind=intersect(ind,which(is.na(datai[,i])==FALSE))
# }
# 
# 
# set.seed(262)
# regr_ind(colon[ind,c(3:9,11)],colon[ind,12],100,cutoff=0.5,mode='multin')
# #first line of the output
# #[1] 0.1180934 0.8213483 0.1180337


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

set.seed(263)
regr_ind(pbc[ind,c(5:6,10:11,13,18:20)],pbc[ind,3],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.7207305 0.5590500 0.7187500


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

set.seed(264)
regr_ind(transplant[ind[ind1],1:5],transplant[ind[ind1],6],1000,cutoff=0.5,mode='multin')
#first line of the output
#[1] 0.8743307 0.8141316 0.8490000


