library(car)
library(afex)
library(nnet)
library(doParallel)

#This set of functions is ientical to the original one except the functions cross_val and regr_ind, where the function of computing and displaying the predictive power achieved by all significant predictors is included
#Only the modified parts of cross-val and regr_ind are commented



#'Turning a non-numeric variable into a numeric one
#'
#'Function which turns a single categorical (non-numeric) variable into a numeric one (or several) by introducing dummy '0'/'1' variables.
#'
#'@param vari array of values to be transformed
#'@param outcome TRUE/FALSE indicates whether the variable \code{vari} is an outcome (TRUE) or a predictor (FALSE)
#'@param ra indices of the input array \code{vari} which indicate which values will be transformed
#'@usage make_numeric(vari, outcome, ra)
#'@return Returned value is an M x N matrix where M is the length of the input array of indices \code{ra} and N is \code{length(vari)-1}.
#'@details This function is essentially a standard way to turn categorical non-numeric variables into numeric ones in order to run a regression
#'@export make_numeric
#'@examples
#'#creating a non-numeric set
#'
#'a<-t(rmultinom(100,1,c(0.2,0.3,0.5)))%*%c(1,2,3)
#'
#'a[a==1]='red'
#'a[a==2]='green'
#'a[a==3]='blue'
#'
#'#running the function
#'
#'make_numeric(a,FALSE,sample(1:100,50))
#'
#'make_numeric(a,TRUE,sample(1:100,50))





make_numeric<-function(vari,outcome,ra){
  
  # if ((length(ra)>length(vari))|(max(ra)>length(vari))) stop("Infeasible set of indices 'ra'")
  #
  # if (is.numeric(vari)==TRUE) stop("Array 'vari' is already numeric")
  
  if (outcome==TRUE){
    
    un=unique(vari);
    
    lun=length(un);
    
    lunu<-array(NA,lun)
    
    for (i in 1:lun){
      
      lunu[i]=length(vari[vari==un[i]]);
      
    }
    
    o<-order(lunu);
    
    vari1<-as.numeric(as.factor(vari));
    
    for (j in 1:lun){
      
      vari1[vari==un[o[j]]]=lun-j;
      
      
    }
    
    
    as.numeric(vari1);
    
  }
  else{
    
    un=unique(vari);
    
    lun=length(un);
    
    lunu<-array(NA,lun)
    
    for (i in 1:lun){
      
      lunu[i]=length(vari[vari==un[i]]);
      
    }
    
    o<-order(lunu);
    
    vari1<-matrix(0,length(vari),lun-1)
    
    for (i in 1:lun-1){
      
      vari1[which(vari==un[o[i]]),i]=1;
      
    }
    
    vari1[ra,];
    
  }
  
  
}

#'Transforming the set of predictors into a numeric set
#'
#'Function which turns a set of predictors containing non-numeric variables into a fully numeric set
#'
#'@param a An M x N matrix, containing all possible subsets (N overall) of the size M of predictors' indices; therefore each column of \code{a} defines a unique subset of the predictors
#'@param ai array of indices of the array \code{a}
#'@param k index of the array \code{ai}
#'@param vari set of all predictors
#'@param ra array of sample indices of \code{vari}
#'@param l size of the sample
#'@usage make_numeric_sets(a,ai,k,vari,ra,l)
#'@return Returns a list containing two objects: \code{tr} and \code{test}\cr
#'\item{tr}{training set transformed into a numeric one}
#'\item{test}{test set transformed into a numeric one}
#'@details Function transforms the whole set of predictors into a numeric set by consecutively calling function \code{make_numeric} for each predictor
#'@seealso \code{\link{make_numeric}}
#'@export make_numeric_sets
#'@examples
#'#creating a categorical numeric variable
#'
#'a<-t(rmultinom(100,1,c(0.2,0.3,0.5)))%*%c(1,2,3)
#'
#'#creating an analogous non-numeric variable
#'
#'c<-array(NA,100)
#'c[a==1]='red'
#'c[a==2]='green'
#'c[a==3]='blue'
#'
#'#creating a data-set
#'
#'b<-data.frame(matrix(c(a,rbinom(100,1,0.3),runif(100,0,1)),ncol=3))
#'
#'#making the first column of the data-set non-numeric
#'
#'b[,1]=data.frame(c)
#'
#'#running the function
#'
#'make_numeric_sets(combn(3,2),1:3,1,b,sample(1:100,60),100)



make_numeric_sets<-function(a,ai,k,vari,ra,l){
  
  # if (min(dim(as.matrix(a)))==1) a=as.matrix(t(a))
  # if (min(dim(as.matrix(ai)))==1) ai=as.matrix(t(ai))
  # if (min(dim(as.matrix(vari)))==1) vari=as.matrix(t(vari))
  #
  # if (dim(vari)[1]!=l) stop('Length does not correspond to the dimension of the predictors')
  #
  # if (dim(a)[1]>dim(vari)[2]) stop('Size of the subset is large than the size of the set')
  # if (dim(a)[2]<length(ai)) stop('Length of the indices is larger than the length of the array')
  #
  # if (k>dim(ai)[2]) stop('Index is larger than the length of the array')
  # if (length(ra)>l) stop('Subset of sample indices is larger than sample size')
  # if (max(ra)>l) stop('Non-existent sample index')
  
  testset1<-array(NA,0)
  trset1<-array(NA,0)
  
  for (m in 1:length(a[,ai[k]])){
    
    
    if (is.numeric(vari[,a[m,ai[k]]])==FALSE){
      
      anum1<-make_numeric(vari[,a[m,ai[k]]],FALSE,ra)
      anum<-make_numeric(vari[,a[m,ai[k]]],FALSE,setdiff(1:l,ra))
      
      testset1<-cbind(testset1,anum)
      trset1<-cbind(trset1,anum1)
      
      
    }
    else{
      
      testset1<-cbind(testset1,vari[setdiff(1:l,ra),a[m,ai[k]]])
      trset1<-cbind(trset1,vari[ra,a[m,ai[k]]])
      
      
    }
    
  }
  
  
  list("test" = testset1, "tr" = trset1)
  
  
}

#'Weights of predictors
#'
#'Function which computes the weight of each predictor according to the rules of thumb and outputs it into corresponding array
#'
#'@param vari_col number of predictors
#'@param vari set of predictors
#'@details Continuous or categorical numerical variable with more then 5 categories has weight 1, otherwise it has weight \code{n-1} where \code{n} is the number of categories
#'@return Returns an array of weights of the size \code{vari_col}
#'@usage compute_weights(vari_col, vari)
#'@export compute_weights
#'@references{
#'\insertRef{ref1}{CARRoT}
#'}
#'@references{
#'\insertRef{ref2012-18631-001}{CARRoT}
#'}
#'@importFrom Rdpack reprompt
#'@examples
#'#creating data-set with for variables
#'
#'a<-matrix(NA,nrow=100,ncol=4)
#'
#'#binary variable
#'
#'a[,1]=rbinom(100,1,0.3)
#'
#'#continuous variable
#'
#'a[,2]=runif(100,0,1)
#'
#'#categorical numeric with les than 5 categories
#'
#'a[,3]=t(rmultinom(100,1,c(0.2,0.3,0.5)))%*%c(1,2,3)
#'
#'#categorical numeric with 5 categories
#'
#'a[,4]=t(rmultinom(100,1,c(0.2,0.3,0.3,0.1,0.1)))%*%c(1,2,3,4,5)
#'
#'#running the function
#'
#'compute_weights(4,a)


compute_weights<-function(vari_col,vari){
  
  # if (min(dim(as.matrix(vari)))==1) vari=as.matrix(t(vari))
  #
  # if (vari_col>dim(vari)[2]) stop('"vari_col" is larger than the number of predictors')
  
  
  we<-matrix(nrow=0,ncol=1);
  
  for (i in 1:vari_col){
    
    if (is.numeric(vari[,i])==TRUE) {
      
      we<-array(c(we,1))
      
    } else{
      
      we<-array(c(we,length(unique(vari[,i]))-1));
    }
    
  }
  
  we
  
}

#'Maximum feasible weight of the predictors
#'
#'Function which computes maximal weight (multiplied by 10) of a regression according to the rule of thumb applied to the outcome variable. Weight of a regression equals the sum of weights of its predictors.
#'
#'@details For continuous outcomes it equals sample size divided by 10, for multinomial it equals the size of the smallest category divided by 10
#'@param outi set of outcomes
#'@param mode indicates the mode: 'linear' (linear regression), 'binary' (logistic regression), 'multin' (multinomial regression)
#'@usage compute_max_weight(outi,mode)
#'@return returns an integer value of maximum allowed weight multiplied by 10
#'@export compute_max_weight
#'@examples
#'#continuous outcomes
#'
#' compute_max_weight(runif(100,0,1),'linear')
#'
#' #binary outcomes
#'
#' compute_max_weight(rbinom(100,1,0.4),'binary')
#'@references{
#'\insertRef{ref1}{CARRoT}
#'}
#'@importFrom Rdpack reprompt




compute_max_weight<-function(outi,mode){
  
  if (mode=='linear') {
    
    numr=length(outi)
    
  } else{
    
    unio<-unique(outi);
    
    lo<-array(NA,length(unio))
    
    for (i in 1:length(unio)) lo[i]=length(which(outi==unio[i]));
    
    numr=min(lo);
  }
  
  numr
  
}

#'Cumulative weights of the predictors' subsets
#'
#'Function which computes the sum of predictors' weights for each subset containing a fixed number of predictors
#'
#'@param a an \code{m} x N matrix, containing all possible subsets (N overall) of the size \code{m} of predictors' indices; therefore each column of \code{a} defines a unique subset of the predictors
#'@param m number of elements in each subset of indices
#'@param we array of weights of the predictors
#'@usage sum_weights_sub(a,m,we)
#'@return Returns an array of weights for predictors defined by each colun of the matrix \code{a}
#'@export sum_weights_sub
#'@examples
#'#all two-element subsets of the set 1:3
#'
#'a<-combn(3,2)
#'
#'sum_weights_sub(a,2,c(1,2,1))



sum_weights_sub<-function(a,m,we,st){
  
  # if (min(dim(as.matrix(a)))==1) a=as.matrix(t(a))
  #
  # if (dim(a)[1]!=m) stop('Mismatch between the number of elements and a subset matrix')
  
  s<-array(NA,ncol(a));
  
  if ((m>1)|(is.null(st)==FALSE)){
    
    for (h in 1:ncol(a))
      
      s[h]=sum(we[a[((h-1)*m+1):(h*m)]]);
    
  } else s=we;
  
  s
  
}


#'Finds certain subsets of predictors
#'
#'Reorders the columns of matrix \code{a} according to the ordered elements of array \code{s}
#'@param a A \code{j} x N matrix, containing all possible subsets (N overall) of the size \code{j} of predictors' indices.
#'@param s array of numbers of the size N
#'@param j number of rows in \code{a}
#'@param c array of all indices of the predictors
#'@usage find_sub(a,s,j,c)
#'@return Returns a submatrix of matrix \code{a} which consits of columns determined by the input array \code{s}
#'@export find_sub
#'@examples
#'#all two-element subsets of 1:3
#'
#'a<-combn(3,2)
#'s<-c(3,2,3)
#'
#'find_sub(a,s,2,1:3)



find_sub<-function(a,s,j,c,st){#
  
  
  if (j==1){
    
    a=t(matrix(a[,order(s)]));
    
  } else{
    
    if (dim(a)[2]==1){
      
      a=matrix(a[,order(s)]);
      
    } else a=a[,order(s)];
    
  }
  
  a
  
}

#'Maximum number of the regressions
#'
#'Function which computes the maximum number of regressions with fixed number of variables based on the rule of thumb
#'
#'@param vari_col number of predictors
#'@param k maximum weight of the predictors
#'@param c array of all indices of the predictors
#'@param we array of weights of the predictors. Continuous or categorical numerical variable with more then 5 categories has weight 1, otherwise it has weight \code{n-1} where \code{n} is the number of categories
#'@param maxx maximum number of predictors, total number of variables by default
#'@import utils
#'@return Integer correponding to maximum number of regressions of the same size
#'@usage compute_max_length(vari_col,k,c,we,maxx)
#'@seealso Function uses \code{\link[utils]{combn}}
#'@export compute_max_length
#'@references{
#'\insertRef{ref1}{CARRoT}
#'}
#'@references{
#'\insertRef{ref2012-18631-001}{CARRoT}
#'}
#'@importFrom Rdpack reprompt
#'@examples
#'compute_max_length(4,40,1:4,c(1,1,2,1))


compute_max_length<-function(vari_col,k,c,we,maxx=NULL,st){ #
  
  
  le<-array(NA,min(min(vari_col,k),maxx))
  
  for (m in 1:min(min(vari_col,k),maxx)){
    
    a<-combn(c,m);
    
    s<-sum_weights_sub(a,m,we,st);
    
    
    le[m]=length(which(s<=k));
    
  }
  
  max(le)
  
  
}


#'Probabilities for multinomial regression
#'
#'Function which computes probabilities of outcomes on the test set by applying regression parameters inferred by a run on the training set. Works for logistic or multinomial regression
#'@param trset values of predictors on the training set
#'@param testset values of predictors on the test set
#'@param outc values of outcomes on the training set
#'@param mode \code{'binary'} (logistic regression) or \code{'multin'} (multinomial regression)
#'@usage get_probabilities(trset,testset,outc,mode)
#'@return Probabilities of the outcomes. In \code{'binary'} mode returns an array of the size of the number of observations in a testset. In \code{'multin'} returns an M x N matrix where M is the size of the number of observations in a testset
#'and N is the number of unique outcomes minus 1.
#'@details In binary mode this function computes the probabilities of the event '0'. In multinomial mode computes the probabilities of the events '0','1',...,'N-1'.
#'@seealso Function uses \code{\link[nnet]{multinom}}  and \code{\link[stats]{coef}}
#'@import stats
#'@import nnet
#'@export get_probabilities
#'@examples
#'trset<-matrix(c(rbinom(70,1,0.5),runif(70,0.1)),ncol=2)
#'
#'testset<-matrix(c(rbinom(10,1,0.5),runif(10,0.1)),ncol=2)
#'
#'get_probabilities(trset,testset,rbinom(70,1,0.6),'binary')


get_probabilities<-function(trset,testset,outc,mode){
  
  # if (min(dim(as.matrix(trset)))==1) trset=as.matrix(t(trset))
  #
  # if (min(dim(as.matrix(testset)))==1) testset=as.matrix(t(testset))
  #
  # if (dim(testset)[2]!=dim(trset)[2]) stop('Mismatch between training and test set dimensions')
  #
  # if (dim(trset)[1]!=length(outc)) stop('Sample size for predictors and outcomes is not of the same length')
  
  
  d<-dim(data.matrix(testset));
  
  
  if (mode=='binary'){
    
    
    
    regr<-data.matrix(coef(multinom(-outc~.,data=data.frame(trset),trace=FALSE)));
    
    ps=exp(matrix(rep(1,d[1]))%*%regr[1,]+data.matrix(testset)%*%regr[2:length(regr),]);
    
    
  } else {
    
    regr<-apply(data.matrix(coef(multinom(-outc~.,data=data.frame(trset),trace=FALSE))),2,rev);
    
    ps=rowSums(exp(matrix(rep(1,d[1]))%*%regr[,1]+data.matrix(testset)%*%t(regr[,2:dim(regr)[2]])));
    
  }
  
  coe=8;
  
  ps[ps==Inf]=max(ps[ps<Inf])*coe;
  
  while(length(ps[ps==Inf])>0){
    coe=coe/2;
    ps[ps==Inf]=max(ps[ps<Inf])*coe;
  }
  p1=t(1/(1+ps));
  
  if (mode=='binary') {
    
    p0=exp(matrix(rep(1,d[1]))%*%regr[1,]+data.matrix(testset)%*%regr[2:length(regr),]);
    
  } else p0=exp(matrix(rep(1,d[1]))%*%regr[,1]+data.matrix(testset)%*%t(regr[,2:dim(regr)[2]]));
  
  if (mode=='binary') {
    
    p=t(p1)*p0;
  } else {
    
    p=t(p1)*p0[,1]
    
    for (i in 2:dim(p0)[2]) p=cbind(p,t(p1)*p0[,i])
    
  }
  
  p
  
  
}

#'Predictions for linear regression
#'
#'Function which runs a linear regression on a training set, computes predictions for the test set
#'
#'@param trset values of predictors on the training set
#'@param testset values of predictors on the test set
#'@param outc values of predictors on the training set
#'@param k length of the test set
#'@usage get_predictions_lin(trset,testset,outc,k)
#'@return An array of continous variables of the length equal to the size of a \code{testset}
#'@seealso Function uses function \code{\link[stats]{lsfit}} and \code{\link[stats]{coef}}
#'@export get_predictions_lin
#'@examples
#'trset<-matrix(c(rnorm(90,2,4),runif(90,0,0.5),rbinom(90,1,0.5)),ncol=3)
#'
#'testset<-matrix(c(rnorm(10,2,4),runif(10,0,0.5),rbinom(10,1,0.5)),ncol=3)
#'
#'get_predictions_lin(trset,testset,runif(90,0,1),10)

get_predictions_lin<-function(trset,testset,outc,k){
  
  # if (min(dim(as.matrix(trset)))==1) trset=as.matrix(t(trset))
  #
  # if (min(dim(as.matrix(testset)))==1) testset=as.matrix(t(testset))
  #
  # if (dim(testset)[2]!=dim(trset)[2]) stop('Mismatch between training and test set dimensions')
  #
  # if (dim(trset)[1]!=length(outc)) stop('Sample size for predictors and outcomes is not of the same length')
  
  
  
  regr<-data.matrix(coef(lsfit(as.matrix(trset),outc)));
  
  pred<-array(NA,c(1,k));
  
  if (sum(outc%%1)==0){
    
    pred<-round(data.matrix(testset)%*%matrix(regr[2:length(regr)])+regr[1]);
    
  } else{
    
    pred<-data.matrix(testset)%*%matrix(regr[2:length(regr)])+regr[1];
    
    
    
  }
  
  if (length(outc[outc<0]==0)) pred[pred<0]=0;
  
  pred
  
  
}

#'Predictions for multinomial regression
#'
#'Function which makes a prediction for multinomial/logistic regression based on the given cut-off value and probabilities.
#'@param p probabilities of the outcomes for the test set given either by an array (logistic regression) or by a matrix (multinomial regression)
#'@param k size of the test set
#'@param cutoff cut-off value of the probability
#'@param cmode \code{'det'} or \code{''}; \code{'det'} always predicts the more likely outcome as determined by the odds ratio; \code{''} predicts certain outcome with probability corresponding to its odds ratio (more conservative). Option available for multinomial/logistic regression
#'@param mode \code{'binary'} (logistic regression), \code{'multin'} (multinomial regression)
#'@usage get_predictions(p,k,cutoff,cmode,mode)
#'@seealso Uses \code{\link[stats]{rbinom}}, \code{\link[stats]{rmultinom}}
#'@return Outputs the array of the predictions of the size of \code{p}.
#'@export get_predictions
#'@examples
#'#binary mode
#'
#'get_predictions(runif(20,0.4,0.6),20,0.5,'det','binary')
#'
#'#creating a data-set for multinomial mode
#'
#'p1<-runif(20,0.4,0.6)
#'p2<-runif(20,0.1,0.2)
#'p3<-1-p1-p2
#'
#'#running the function
#'
#'get_predictions(matrix(c(p1,p2,p3),ncol=3),20,0.5,'det','multin')


get_predictions<-function(p,k,cutoff,cmode,mode){
  
  
  pred<-array(NA,c(1,k));
  
  for (m in 1:k){
    
    
    if (mode=='binary'){
      
      
      
      if (cmode=='det'){
        
        if (p[m]>cutoff){
          pred[m]=0;
        } else pred[m]=1;
        
      } else{
        
        pred[m]=1-rbinom(1,1,p[m])
      }
      
    } else{
      
      if (cmode=='det'){
        
        maxpr=which(c(p[m,],1-sum(p[m,]))==max(c(p[m,],1-sum(p[m,]))));
        
        pred[m]=maxpr-1;
        
      } else{
        
        pred[m]=c(0:(k-1))%*%rmultinom(1,1,c(p[m,],1-sum(p[m,])))
        
      }
    }
  }
  
  pred
  
}

#'Combining in a list
#'
#'Function for combining outputs in a list
#'@param ... an argument of \code{mapply} used by this function
#'@seealso Function \code{\link[base]{mapply}}
#'@export comb
#'@examples
#'#array of numbers to be separated in a list
#'
#'a<-1:4
#'
#'#running the function
#'
#'comb(a)

comb <- function(...) {
  mapply('cbind', ..., SIMPLIFY=FALSE)
}

#'Area Under the Curve
#'
#'Function enables efficient computation of area under receiver operating curve (AUC). Source: \url{https://stat.ethz.ch/pipermail/r-help/2005-September/079872.html}
#'@param probs probabilities
#'@param class outcomes
#'@usage AUC(probs, class)
#'@return A value for AUC
#'@export AUC
#'@examples
#'AUC(runif(100,0,1),rbinom(100,1,0.3))

AUC <- function(probs, class) {
  x <- probs
  y <- class
  x1 = x[y==1]; n1 = length(x1);
  x2 = x[y==0]; n2 = length(x2);
  r = rank(c(x1,x2))
  auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
  return(auc)
}

#'Cross-validation run
#'
#'Function running a single cross-validation by partitioning the data into training and test set
#'
#'@param vari set of predictors
#'@param outi array of outcomes
#'@param c set of all indices of the predictors
#'@param rule rule of 10 in this case
#'@param part indicates partition of the original data-set into training and test set in a proportion \code{(part-1):1}
#'@param l number of observations
#'@param we weights of the predictors
#'@param vari_col overall number of predictors
#'@param preds array to write predictions into, intially empty
#'@param cmode \code{'det'} or \code{''}; \code{'det'} always predicts the more likely outcome as determined by the odds ratio; \code{''} predicts certain outcome with probability corresponding to its odds ratio (more conservative). Option available for multinomial/logistic regression
#'@param mode \code{'binary'} (logistic regression), \code{'multin'} (multinomial regression)
#'@param predm \code{'exact'} or \code{''}; for logistic and multinomial regression; \code{'exact'} computes how many times the exact outcome category was predicted, \code{''} computes how many times either the exact outcome category or its nearest neighbour was predicted
#'@param cutoff cut-off value for logistic regression
#'@param objfun \code{'roc'} for maximising the predictive power with respect to AUC, \code{'acc'} for maximising predictive power with respect to accuracy.
#'@param minx minimum number of predictors to be included in a regression, defaults to 1
#'@param maxx maximum number of predictors to be included in a regression, defaults to maximum feasible number according to one in ten rule
#'@param maxw maximum weight of predictors to be included in a regression, defaults to maximum weight according to one in ten rule
#'@param nr a subset of the data-set, such that \code{1/part} of it lies in the test set and \code{1-1/part} is in the training set, defaults to empty set
#@usage cross_val(vari,outi,c,rule,part,l,we,vari_col,preds,mode,cmode,predm,cutoff,objfun,minx,maxx,nr,maxw)
#'@return
#'\item{regr}{An M x N matrix of sums of the absolute errors for each element of the test set for each feasible regression. M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression. Therefore each row corresponds to results obtained from running regressions with the same number of variables and columns correspond to different subsets of predictors used.}
#'\item{regrr}{An M x N matrix of sums of the relative errors for each element of the test set (only for \code{mode = 'linear'}) for each feasible regression. M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression. Therefore each row corresponds to results obtained from running regressions with the same number of variables and columns correspond to different subsets of predictors used.}
#'\item{nvar}{Maximum feasible number of variables in the regression}
#'\item{emp}{An accuracy of always predicting the more likely outcome as suggested by the training set (only for \code{mode = 'binary'} and \code{objfun = 'acc'})}
#'In \code{regr} and \code{regrr} \code{NA} values are possible since for some numbers of variables there are fewer feasible regressions than for the others.
#'@seealso Uses \code{\link{compute_max_weight}}, \code{\link{sum_weights_sub}}, \code{\link{make_numeric_sets}}, \code{\link{get_predictions_lin}}, \code{\link{get_predictions}}, \code{\link{get_probabilities}}, \code{\link{AUC}}, \code{\link[utils]{combn}}
#'@export cross_val
#'@examples
#'#creating variables
#'
#'vari<-matrix(c(1:100,seq(1,300,3)),ncol=2)
#'
#'#creating outcomes
#'
#'out<-rbinom(100,1,0.3)
#'
#'#creating array for predictions
#'
#'preds<-array(NA,c(2,2))
#'
#'#running the function
#'
#'cross_val(vari,out,1:2,10,10,100,c(1,1),2,preds,'binary','det','exact',0.5,'acc',nr=c(1,4))

cross_val<-function(vari,outi,c,rule,part,l,we,vari_col,preds,preds_si,mode,cmode,predm,cutoff,objfun,minx=NULL,maxx=NULL,nr=NULL,maxw=NULL,st=NULL){
  
  if ((objfun!='acc')&(objfun!='roc')) stop('Unknown value of the parameter "objfun"')
  

  
  if (mode=='linear') {
    
    predsr<-preds
    
 #   preds_sir<-preds_si
    
  }
  
  # if (mode=='binary'){
  # 
  #   predspos<-preds
  #   predsfpos<-preds
  #   predsfneg<-preds
  # 
  # }
  
  if (is.null(nr)==TRUE){
    
    ra=sample(1:l,floor((1-(1/part))*l));
    
  } else {
    
    lnr=length(nr);
    
    ranr=sample(nr,floor((1-(1/part))*lnr))
    
    rar=sample(setdiff(1:l,nr),floor((1-(1/part))*l)-floor((1-(1/part))*lnr));
    
    ra=c(ranr,rar);
    
  }
  
  trset<-vari[ra,];
  
  outc<-outi[ra];
  
  testset<-vari[setdiff(c(1:l),ra),];
  
  #defining the set of significant predictors as an empty set
  
  signi=c();
  
  #going through all predictors, for each one of them fitting a univariate regression
  
  for (q in 1:vari_col){
    
   #turning non-numeric variables nto numeric ones 
    
    if (is.numeric(vari[ra,q])==FALSE){
      
      sit<-make_numeric(vari[,q],FALSE,ra) 
    } else {
      
      sit<-vari[ra,q]
      
    }
    
    
    #for the linear mode compute p-value for the f-statistic
    
    if (mode=='linear'){
      
      #re=summary(lm(outc~vari[ra,q]))$coefficients[,4]
      
      re=summary(lm(outc~sit))$fstatistic
      
      if (is.null(re)==F){
        pv <- pf(re[1],re[2],re[3],lower.tail=F)
      } else {
        
        pv<-100
      }
      
    }
    else{ #for binary and multinomial modes compute p-value based on likelihood ratio tests as used in the function Anova
      
      re=multinom(outc~.,data=data.frame(vari[ra,q]),trace=FALSE)
      
      # z<-re$coefficients/re$standard.errors
      # 
      # re <- (1 - pnorm(abs(z), 0, 1)) * 2
      # 
      
      pv<-Anova(re,type="III")[3]
    }
    
    #variables exhibiting p-values lower than 0.05 are included inot the set of significant predictors
    
    if (pv<0.05) signi=c(signi,q);
    
  }
  
  mw<-compute_max_weight(outc,mode)
  
  mw=min(mw,maxw);
  
  nvar=floor((1/rule)*mw);
  
  if (nvar==0) stop('not enough events to build a regression model');
  
  if (is.null(maxx)==FALSE){
    
    maxj=min(maxx,min(nvar,vari_col))
    
  } else {
    
    maxj=min(nvar,vari_col)
  }
  
  if (is.null(minx)==FALSE){
    
    minj=max(1,minx)
    
  } else {
    
    minj=1
  }
  
  c=setdiff(c,st);
  
  lest=length(st)
  
  for (j in max(minj,lest):maxj){
    #  j=1
    
    
      
      a<-combn(c,(j-lest));
    
      if ((length(c)<2)&(j>lest)) a=matrix(c)
      
    
    if (is.null(st)==FALSE) a<-rbind(matrix(st,ncol=dim(a)[2],nrow=lest),a)
    
    
    #if (){
    
    s<-sum_weights_sub(a,j,we,st)
    
    
    a<-find_sub(a,s,j,c(st,c),st)
    #
    s=sort(s);
    
    ai=which(s<=max(nvar));
    
    if (length(ai)>0){
      
      for (k in 1:length(ai)){
        
        set_num<-make_numeric_sets(a,ai,k,vari,ra,l)
        
        testset1<-set_num$test
        
        trset1<-set_num$tr
        
        
        if (mode=='linear'){
          
          pred<-get_predictions_lin(trset1,testset1,outc,l-floor((1-1/part)*l))
          
          diff<-outi[setdiff(1:l,ra)]-pred;
          
          diffr<-(pred/outi[setdiff(1:l,ra)])-1;
          
          preds[j,k]=sum(abs(diff));
          
          predsr[j,k]=sum(abs(diffr));
          
          # if (all(a[,ai[k]] %in% signi)){
          #   
          #   pred_si<-get_predictions_lin(trset1,testset1,outc,l-floor((1-1/part)*l))
          #   
          #   diff_si<-outi[setdiff(1:l,ra)]-pred_si;
          #   
          #   diff_sir<-(pred_si/outi[setdiff(1:l,ra)])-1;
          #   
          #   preds_si[j,k]=sum(abs(diff_si));
          #   
          #   preds_sir[j,k]=sum(abs(diff_sir));
          #   
          # }
      
          
          
        } else{
          
          p<-get_probabilities(trset1,testset1,outc,mode);
          
          
          if (objfun=='acc'){
            
            pred<-get_predictions(p,l-floor((1-1/part)*l),cutoff,cmode,mode)
            
            diff<-outi[setdiff(1:l,ra)]-array(pred);
            
            #predsfpos[j,k]=length(which(diff==-1));
            
            #predsfneg[j,k]=length(which(diff==1));
            
            if (predm=='exact')  {
              
              diff[abs(diff)>0]=1;
            } else{
              
              
              diff[abs(diff)<2]=0;
              diff[abs(diff)>1]=1;
            }
            
            
            preds[j,k]=l-floor((1-1/part)*l)-sum(abs(diff));
            #predspos[j,k]=sum(pred);
            
            
          } else{
            
            preds[j,k]<-AUC(1-p,outi[setdiff(1:l,ra)])
          }
        }
        
      }
    }
  }
  
  #if the set of significant predictors is not empty convert the corresponding variables into numeric ones
  
  if (is.null(signi)==FALSE){
    set_num_si<-make_numeric_sets(matrix(signi,ncol=1),1,length(q),vari,ra,l)
  
  testset1si<-set_num_si$test
  
  trset1si<-set_num_si$tr
  }
  
  if (mode=='linear'){#computing predictions based on the set of significant predictors in case of linear regression

  
  
  if (is.null(signi)==FALSE){
    

  
  predsi<-get_predictions_lin(trset1si,testset1si,outc,l-floor((1-1/part)*l))

  } else {
    
    predsi=NA
  }
  
  } else{
    
    
    
    if (is.null(signi)==FALSE){#computing predictions based on the set of significant predictors in case of logistic or multinomial regression
      
      #computing probabilities on the test set
      
      psi<-get_probabilities(trset1si,testset1si,outc,mode)
      
      if (objfun=='acc'){
        
        #computing predictions on the test set given cut-off
      
        predsi<-get_predictions(psi,l-floor((1-1/part)*l),cutoff,cmode,mode)
        
      }
      else{
        
        #computing AUROC value
        
        predsi<-AUC(1-psi,outi[setdiff(1:l,ra)])
      }
      
      
    } else {
      
      predsi=NA
    }
    
    
    
    
    
    
  }
  
  #}
  
  #print(predspos)
  # print(sum(outi[setdiff(1:l,ra)]))
  # print(predsfpos)
  # print(predsfneg)
  
  if ((mode=='binary')&(objfun=='acc')){
    
    cpred=sum(outi[setdiff(1:l,ra)])/(l-floor((1-1/part)*l));
    
    predsi=1-sum(abs(outi[setdiff(1:l,ra)]-predsi)/(l-floor((1-1/part)*l)));
    
    #writing the predictive power as the last element of the output of the function cross_val
    
    list("regr" = preds, "nvar"=nvar, "emp" = cpred, "regrsi"=predsi)
    #"fp"=predsfpos,"fn"=predsfneg,"pos"=predspos)
    
    
  } else{
    
    if ((mode=='multin')|(objfun=='roc')) {
      
      if (mode=='multin')  {
        
        predsi=1-length(which(abs(outi[setdiff(1:l,ra)]-array(predsi))>0))/(l-floor((1-1/part)*l));

        uo<-unique(outc);
        
        cpred=1-length(which(outi[setdiff(1:l,ra)]==uo[which.max(tabulate(match(outc,uo)))]))/(l-floor((1-1/part)*l));
        
        #writing the predictive power as the third element of the output of the function cross_val
        
        list("regr" = preds, "nvar"=nvar, "regrsi"=predsi, "emp"=cpred) 
      } else {
        
        #writing the predictive power as the last element of the output of the function cross_val
        
        list("regr" = preds, "nvar"=nvar, "regrsi"=predsi)
      }
      
    } else{
      
      cpred=sum(abs(outi[setdiff(1:l,ra)]-mean(outi[ra])))/(l-floor((1-1/part)*l));
      
      cpredr=sum(abs((outi[setdiff(1:l,ra)]-mean(outi[ra]))/outi[setdiff(1:l,ra)]))/(l-floor((1-1/part)*l));
      
      #for linear mode computing the absolute error based on the set of significant predictors
      
      cpredsi=sum(abs(outi[setdiff(1:l,ra)]-predsi)/(l-floor((1-1/part)*l)));
      
      #for linear mode computing the relative error based on the set of significant predictors
      
      cpredsir=(sum(abs((outi[setdiff(1:l,ra)]-predsi)/outi[setdiff(1:l,ra)]))/(l-floor((1-1/part)*l)));
  
      #writing the predictive power as the last two elements of the output of the function cross_val
          
      list("regr" = preds, "nvar"=nvar, "regrr"=predsr, "emp"=cpred,"empr"=cpredr,"regrsi"=cpredsi, "regrsir"=cpredsir)
      
    }
    
  }
  
  
}


#'Averaging out the predictive power
#'
#'Function which averages out the predictive power over all cross-validations
#'@param preds An M x \code{crv}N matrix consisting of \code{crv} horizontally concatenated M x N matrices. These M x N matrices are the matrices of predictive powers for all feasible regressions (M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression)
#'@param crv number of cross-validations
#'@param k size of the test set for which the predictions are made
#'@usage av_out(preds,crv,k)
#'@return Returns an M x N matrix of average predictive powers where M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression
#'@export av_out
#'@examples
#'#creating a matrix of predictive powers
#'
#'preds<-cbind(matrix(runif(40,1,4),ncol=10),matrix(runif(40,1.5,4),ncol=10))
#'preds<-cbind(preds,matrix(runif(40,1,3.5),ncol=10))
#'
#'#running the function
#'
#'av_out(preds,3,5)


av_out<-function(preds,crv,k){
  
  si<-dim(preds);
  
  si[2]=si[2]/crv;
  
  predsp<-array(NA,c(si[1],si[2]))
  
  for (i in 1:si[1]){
    for (j in 1:si[2]){
      
      pr<-preds[i,seq(j,si[2]*crv,si[2])];
      
      predsp[i,j]=mean(pr[is.finite(pr)],na.rm=TRUE)/k;
      
    }
  }
  
  predsp
  
}

#'Best regression
#'
#'Function which identifies regressions with the highest predictive power
#'
#'@param predsp An M x N matrix of averaged out predictive power values. M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression.
#'@param nvar array of maximal number of variables for each cross-validation
#'@param c array of all indices of the prediction variables
#'@param we array of all weights of the prediction variables
#'@usage get_indices(predsp,nvar,c,we)
#'@return A list of arrays which contain indices of the predictors corresponfing to the best regressions
#'@seealso Uses \code{\link{sum_weights_sub}}, \code{\link{find_sub}}, \code{\link[utils]{combn}}
#'@export get_indices
#'@examples
#'#creating a set of averaged out predictive powers
#'
#'predsp<-matrix(NA,ncol=3,nrow=3)
#'
#'predsp[1,]=runif(3,0.7,0.8)
#'predsp[2,]=runif(3,0.65,0.85)
#'predsp[3,1]=runif(1,0.4,0.5)
#'
#'#running the function
#'
#'get_indices(predsp,c(3,3,3),1:3,c(1,1,1))

get_indices<-function(predsp,nvar,c,we,st){
  
  ll=list(0)
  
  nums<-which(predsp==max(predsp[predsp!=0],na.rm=TRUE))
  
  si=dim(predsp)
  
  numv=nums%%si[1]
  
  
  
  numss=ceiling(nums/si[1])
  
  c=setdiff(c,st)
  
  lest=length(st)
  
  for (i in 1:length(numv)){
    
    if (numv[i]==0) numv[i]=si[1]
    
   
      
      af<-combn(c,numv[i]-lest)
      
    if ((length(c)<2)&(numv[i]>lest)) af<-matrix(c)

    
    if (is.null(st)==FALSE) af<-rbind(matrix(st,ncol=dim(af)[2],nrow=lest),af)
    
    s<-sum_weights_sub(af,numv[i],we,st)
    
    af<-find_sub(af,s,numv[i],c,st)
    
    s=sort(s)
    
    aif=which(s<=max(nvar));
    
    #    print(af[,aif[numss[i]]])
    
    ll[[i]]<-af[,aif[numss[i]]]
    
    #     print(ll[[i]])
    
  }
  #  print(ll)
  
  ll
}

#'Indices of the best regressions
#'
#'One of the two main functions of the package. Identifies the predictors included into regressions with the highest average predictive power
#'@param vari set of predictors
#'@param outi array of outcomes
#'@param crv number of cross-validations
#'@param cutoff cut-off value for mode \code{'binary'}
#'@param part for each cross-validation partitions the dataset into training and test set in a proportion \code{(part-1):part}
#'@param cmode \code{'det'} or \code{''}; \code{'det'} always predicts the more likely outcome as determined by the odds ratio; \code{''} predicts certain outcome with probability corresponding to its odds ratio (more conservative). Option available for multinomial/logistic regression
#'@param mode \code{'binary'} (logistic regression), \code{'multin'} (multinomial regression)
#'@param predm \code{'exact'} or \code{''}; for logistic and multinomial regression; \code{'exact'} computes how many times the exact outcome category was predicted, \code{''} computes how many times either the exact outcome category or its nearest neighbour was predicted
#'@param objfun \code{'roc'} for maximising the predictive power with respect to AUC, available only for \code{mode='binary'}; \code{'acc'} for maximising predictive power with respect to accuracy.
#'@param parallel TRUE if using parallel toolbox, FALSE if not. Defaults to FALSE
#'@param cores number of cores to use in case of parallel=TRUE
#'@param minx minimum number of predictors to be included in a regression, defaults to 1
#'@param maxx maximum number of predictors to be included in a regression, defaults to maximum feasible number according to one in ten rule
#'@param maxw maximum weight of predictors to be included in a regression, defaults to maximum weight according to one in ten rule
#'@param nr a subset of the data-set, such that \code{1/part} of it lies in the test set and \code{1-1/part} is in the training set, defaults to empty set. This is to ensure that elements of this subset are included both in the training and in the test set.
#'@return Prints the best predictive power provided by a regression, predictive accuracy of the empirical prediction (value of \code{emp} computed by \code{cross_val} for logistic and linear regression). Returns indices of the predictors included into regressions with the highest predictive power written in a list. For \code{mode='linear'} outputs a list of two lists. First list corresponds to the smallest absolute error, second corresponds to the smallest relative error
#'@export regr_ind
#'@import doParallel
#'@import parallel
#'@import foreach
#'@seealso Uses \code{\link{compute_weights}}, \code{\link{make_numeric}}, \code{\link{compute_max_weight}}, \code{\link{compute_weights}}, \code{\link{compute_max_length}}, \code{\link{cross_val}},\code{\link{av_out}}, \code{\link{get_indices}}
#'@examples
#'#creating variables for linear regression mode
#'
#'variables_lin<-matrix(c(rnorm(56,0,1),rnorm(56,1,2)),ncol=2)
#'
#'#creating outcomes for linear regression mode
#'
#'outcomes_lin<-rnorm(56,2,1)
#'
#'#running the function
#'
#'regr_ind(variables_lin,outcomes_lin,100,mode='linear',parallel=TRUE,cores=2)
#'
#'#creating variables for binary mode
#'
#'vari<-matrix(c(1:100,seq(1,300,3)),ncol=2)
#'
#'#creating outcomes for binary mode
#'
#'out<-rbinom(100,1,0.3)
#'
#'#running the function
#'
#'regr_ind(vari,out,20,cutoff=0.5,part=10,mode='binary',parallel=TRUE,cores=2,nr=c(1,10,20),maxx=1)




regr_ind<-function(vari,outi,crv,cutoff=NULL,part=10,mode,cmode='det',predm='exact',objfun='acc',parallel=FALSE,cores,minx=NULL,maxx=NULL,nr=NULL,maxw=NULL,st=NULL){
  
  on.exit(closeAllConnections());
  
  
  if ((objfun=='roc')&(mode!='binary')) {
    
    stop('function "roc" is available for binary mode only')
    
  }
  
  
  
  rule=10;
  
  if (is.null(dim(vari))){
    
    vari=matrix(vari);
  }
  
  vari_col=ncol(vari);
  
  we<-compute_weights(vari_col,vari)
  
  l=nrow(vari);
  
  c<-c(1:vari_col);
  
  if (is.numeric(outi)==FALSE){
    
    outi<-make_numeric(outi,TRUE)
    
  }
  
  numr<-compute_max_weight(outi,mode);
  
  
  le<-compute_max_length(vari_col,floor((1.0/rule)*numr),c,we,maxx+1,st)
  
  
  preds<-array(NA,c(min(vari_col,min(floor((1/rule)*numr),maxx+1)),le));
  
  preds_si<-array(NA,c(min(vari_col,min(floor((1/rule)*numr),maxx+1)),le));
  
  # require(foreach)
  # requireNamespace(doParallel)
  
  `%fun%` <- `%do%`
  
  
  if (parallel==TRUE){
    
    `%fun%` <- `%dopar%`
    
    cl <- makeCluster(cores)
    
    registerDoParallel(cl)
    
    clusterEvalQ(cl,rm(list=ls()))
    clusterEvalQ(cl, library(nnet))
    clusterEvalQ(cl, library(foreach))
    clusterEvalQ(cl, library(doParallel))
    clusterEvalQ(cl, library(stats))
    clusterEvalQ(cl, library(afex))
    clusterEvalQ(cl, library(car))
    
    # print(args(cross_val))
    
    clusterExport(cl,"cross_val")
    clusterExport(cl,"compute_max_weight")
    clusterExport(cl,"sum_weights_sub")
    clusterExport(cl,"get_probabilities")
    clusterExport(cl,"get_predictions")
    clusterExport(cl,"get_predictions_lin")
    clusterExport(cl,"find_sub")
    clusterExport(cl,"make_numeric")
    clusterExport(cl,"make_numeric_sets")
    clusterExport(cl,"AUC")
    
    
    
  }
  
  result <- foreach(i=1:crv, .combine='comb', .multicombine=TRUE) %fun% {
    
    
    
    cross_val(vari,outi,c,rule,part,l,we,vari_col,preds,preds_si,mode,cmode,predm,cutoff,objfun,minx,maxx,nr,maxw,st);
    #   cross_val(vari,outi,c,rule,part,l,we,vari_col,preds,mode,cmode,predm,cutoff,objfun);
    
    
    
  }
  
  
  preds<-result[[1]];
  
  nvar<-result[[2]];
  
  if((mode=='multin')|(objfun=='roc')) predsi<-result[[3]];
  
  
  
  if (mode=='binary') {
    
    if (objfun=='acc') {
      
      cpred<-result[[3]];
      
      predsi<-result[[4]];
      
      # predsfpos<-result[[4]];
      #
      # predsfneg<-result[[5]];
      #
      # predspos<-result[[6]];
    }
  } else {
    
    if (mode=='linear'){
      
      predsr<-result[[3]];
    
    cpred<-result[[4]];
    
    cpredr<-result[[5]];
    
    predsi<-result[[6]];
    
    predsir<-result[[7]]
    
    # preds_si<-result[[8]];
    # 
    # preds_sir<-result[[9]];
    } else {
      
      cpred<-result[[4]]
    }
    
  }
  
  
  
  
  #
  
  #closeAllConnections()
  
  if (parallel==TRUE) stopCluster(cl)
  
  closeAllConnections()
  #
  
  if (objfun=='roc'){
    
    predsp<-av_out(preds,crv,1)
    

  } else{
    
    predsp<-av_out(preds,crv,l-floor((1-(1/part))*l))
    

    
  }
  
  if (mode=='linear')  {
    
    predspr<-av_out(predsr,crv,l-floor((1-(1/part))*l))
    
    # predsp_si<-av_out(preds_si,crv,l-floor((1-(1/part))*l))
    # 
    # predsp_sir<-av_out(preds_sir,crv,l-floor((1-(1/part))*l))
  }
  
  
  if (((mode=='binary')&(objfun=='acc'))|((mode=='multin'))) {
    
    print(c(max(predsp[predsp>0],na.rm=TRUE),1-sum(cpred)/crv,sum(predsi)/crv));
    
  } else{
    
    if (objfun=='roc') {
      
      print(c(max(predsp[predsp>0],na.rm=TRUE),sum(predsi[predsi<Inf],na.rm=TRUE)/(crv-length(which(predsi==Inf)))))
      
    } else{
      
  #    print(c(min(predsp[predsp>0],na.rm=TRUE),min(predspr[predspr>0],na.rm=TRUE),mean(cpred[cpred<Inf],na.rm=TRUE),mean(cpredr[cpredr<Inf],na.rm=TRUE),mean(predsi[predsi<Inf],na.rm=TRUE),mean(predsir[predsir<Inf],na.rm=TRUE),min(predsp_si[predsp_si>0],na.rm=TRUE),min(predsp_sir[predsp_sir>0],na.rm=TRUE)))
     print(c(min(predsp[predsp>0],na.rm=TRUE),min(predspr[predspr>0],na.rm=TRUE),mean(cpred[cpred<Inf],na.rm=TRUE),mean(cpredr[cpredr<Inf],na.rm=TRUE),mean(predsi[predsi<Inf],na.rm=TRUE),mean(predsir[predsir<Inf],na.rm=TRUE)))
       
    }
    
    
  }
  
  #  if (mode=='linear') print('min absolute error')
  
  #  print(get_indices(predsp,nvar,c,we))
  
  if (mode=='linear') {
    
    #    print('min relative error')
    
    #  print(get_indices(predspr,nvar,c,we))
    
 #   list(get_indices(-predsp,nvar,c,we,st),get_indices(-predspr,nvar,c,we,st),get_indices(-predsp_si,nvar,c,we,st),get_indices(-predsp_sir,nvar,c,we,st))
  
    list(get_indices(-predsp,nvar,c,we,st),get_indices(-predspr,nvar,c,we,st))
    
      
  } else{
    
    get_indices(predsp,nvar,c,we,st)
    
  }
  
  
  
  
  
  
}

#' Best regressions
#'
#' Function which prints the highest predictive power, predictive accuracy of the empirical prediction (value of \code{emp} computed by \code{cross_val} for logistic regression), outputs the regression objects corresponding to the highest average predictive power and the indices of the variables included into regressions with the best predictive power.
#' In the case of linear regression it outputs the best regressions with respect to both absolute and relative errors
#'@param vari set of predictors
#'@param outi array of outcomes
#'@param crv number of cross-validations
#'@param cutoff cut-off value for mode \code{'binary'}
#'@param part for each cross-validation partitions the dataset into training and test set in a proportion \code{(part-1):part}
#'@param cmode \code{'det'} or \code{''}; \code{'det'} always predicts the more likely outcome as determined by the odds ratio; \code{''} predicts certain outcome with probability corresponding to its odds ratio (more conservative). Option available for multinomial/logistic regression
#'@param mode \code{'binary'} (logistic regression), \code{'multin'} (multinomial regression)
#'@param predm \code{'exact'} or \code{''}; for logistic and multinomial regression; \code{'exact'} computes how many times the exact outcome category was predicted, \code{''} computes how many times either the exact outcome category or its nearest neighbour was predicted
#'@param objfun \code{'roc'} for maximising the predictive power with respect to AUC, available only for \code{mode='binary'}; \code{'acc'} for maximising predictive power with respect to accuracy.
#'@param parallel TRUE if using parallel toolbox, FALSE if not. Defaults to FALSE
#'@param cores number of cores to use in case of parallel=TRUE
#'@param minx minimum number of predictors to be included in a regression, defaults to 1
#'@param maxx maximum number of predictors to be included in a regression, defaults to maximum feasible number according to one in ten rule
#'@param maxw maximum weight of predictors to be included in a regression, defaults to maximum weight according to one in ten rule
#'@param nr a subset of the data-set, such that \code{1/part} of it lies in the test set and \code{1-1/part} is in the training set, defaults to empty set. This is to ensure that elements of this subset are included both in the training and in the test set.
#'@return Prints the highest predictive power provided by a regression, predictive accuracy of the empirical prediction (value of \code{emp} computed by \code{cross_val} for logistic regression).
#'\item{ind}{Indices of the predictors included into regressions with the best predictive power written in a list. For \code{mode='linear'} a list of two lists. First list corresponds to the smallest absolute error, second corresponds to the smallest relative error. This output is identical to the one from \code{regr_ind}}
#'\item{regr}{List of regression objects providing the best predictions. For \code{mode='multin'} and \code{mode='binary'}}
#'\item{regr_a}{List of regression objects providing the best predictions with respect to absolute error.For \code{mode='linear'}}
#'\item{regr_r}{List of regression objects providing the best predictions with respect to relative error.For \code{mode='linear'}}
#'@seealso Uses \code{\link{regr_ind}},\code{\link[stats]{lm}}, \code{\link[nnet]{multinom}}
#'@export regr_whole
#'@import doParallel
#'@import parallel
#'@import foreach
#'@examples
#'#creating variables for linear regression mode
#'
#'variables_lin<-matrix(c(rnorm(56,0,1),rnorm(56,1,2)),ncol=2)
#'
#'#creating outcomes for linear regression mode
#'
#'outcomes_lin<-rnorm(56,2,1)
#'
#'#running the function
#'
#'regr_whole(variables_lin,outcomes_lin,20,mode='linear',parallel=TRUE,cores=2)
#'
#'#creating variables for binary mode
#'
#'vari<-matrix(c(1:100,seq(1,300,3)),ncol=2)
#'
#'#creating outcomes for binary mode
#'
#'out<-rbinom(100,1,0.3)
#'
#'#running the function
#'
#'regr_whole(vari,out,20,cutoff=0.5,part=10,mode='binary',parallel=TRUE,cores=2)



regr_whole<-function(vari,outi,crv,cutoff=NULL,part=10,mode,cmode='det',predm='exact',objfun='acc',parallel=FALSE,cores,minx=NULL,maxx=NULL,nr=NULL,maxw=NULL,st=NULL){
  
  if ((objfun=='roc')&(mode!='binary')) {
    
    stop('function "roc" is available for binary mode only')
    
  }
  
  ind<-regr_ind(vari,outi,crv,cutoff,part,mode,cmode,predm,objfun,parallel,cores,minx,maxx,nr,maxw,st)
  
  if (mode=='linear'){
    
    regr_a=list(0)
    
    if (identical(ind[[1]][[1]],ind[[2]][[1]])){
      
      
      
      for (i in 1:length(ind[[1]])){
        
        regr_a[[i]]<-lm(outi~.,data=data.frame(vari[,ind[[1]][[i]]]));
        
      }
      
      
      
      
      list("regr_a"=regr_a,"regr_r"=regr_a,"ind"=ind)
      
      
    } else {
      
      
      
      regr_a=list(0)
      
      
      for (i in 1:length(ind[[1]])){
        
        regr_a[[i]]<-lm(outi~.,data=data.frame(vari[,ind[[1]][[i]]]));
        
      }
      
      
      regr_r=list(0)
      
      
      for (i in 1:length(ind[[2]])){
        
        regr_r[[i]]<-lm(outi~.,data=data.frame(vari[,unlist(ind[[2]][[i]])]));
        
      }
      
      
      
      list("regr_a"=regr_a,"regr_r"=regr_r,"ind"=ind)
      
      
      
    }
    
  } else {
    
    
    regr=list(0)
    
    for (i in 1:length(ind)){
      
      regr[[i]]<-multinom(-outi~.,data=data.frame(vari[,ind[[i]]]),trace=FALSE)
      
    }
    
    list("regr"=regr,"ind"=ind)
    
    
    
  }
  
  
}

quadr<-function(A){
  B<-A
  for (i in 1:dim(A)[2]){
    B=cbind(B,A[,i:dim(A)[2]]*A[,i])
  }
  
  B
}


