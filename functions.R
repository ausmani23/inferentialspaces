summarize.distribution2<-function(ests.distribution) {
  #ests.distribution<-lrm.distribution
  #ests.distribution <- rep(0,10)
  #get quantiles
  quantiles<-quantile(
    ests.distribution,
    c(
      0.01,
      0.025,
      0.05,
      0.5,
      0.95,
      0.975,
      0.99
    )
  )
  #return mu, mu.min, mu.max
  mu<-quantiles["50%"]
  mu.min<-quantiles["2.5%"]
  mu.max<-quantiles["97.5%"]
  #and also a pval classification
  if(mu>=0) {
    if(quantiles["1%"]>0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["2.5%"]>0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["5%"]>0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  } else if(mu<0) {
    if(quantiles["99%"]<0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["97.5%"]<0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["95%"]<0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  }
  # #se
  # #est of se explodes when lagdv coef is over 1
  # #so need something that is robust to that scenario
  # tmpboot<-boot(
  #   ests.distribution,
  #   f.sd,
  #   R=500
  # )
  # se<-mean(tmpboot$t)
  # se.q <- ( quantiles[3] - quantiles[1] ) / 4
  #SE is less rather than more helpful
  se<-NA 
  #se.q<-NA
  #get something like a two-sided pval test
  #pval<-ecdf(ests.distribution)(0)
  #pval<-ifelse(mu<0,(1-pval)*2,pval*2)
  pval<-NA
  #return me
  data.frame(
    mu,
    mu.min,
    mu.max,
    se=se,
    #se.q=se.q,
    pval=pval,
    pval.class=pval.class,
    stringsAsFactors=F
  )
}

########################################################################
########################################################################

#this function takes the vcov of an object
#and a prediction matrix

#it draws from teh vcov
#generates predictions
#and returns margins for each variable

getPredictions <- function(
  m,
  predictdf,
  reps = 1
) {
  # # 
  # m<-m
  # predictdf = popdf
  # reps=1
  # 
  
  #get keyvars
  keyvars <- names(predictdf)
  
  #get vcov
  mycoefs <- m$coefficients
  myvcov <- vcov(m)
  
  #remove NA's, in case of estimation issues
  mycoefs <- mycoefs[!is.na(mycoefs)]
  myvcov <- myvcov[names(mycoefs),names(mycoefs)]
  
  
  #add intercept, and reorder
  predictdf$`(Intercept)` <- 1
  neworder<-match(
    names(mycoefs),
    names(predictdf)
  )
  predictdf<-predictdf[,neworder]
  
  draws <- MASS::mvrnorm(
    n=reps,
    mu = mycoefs, 
    Sigma = myvcov
  )
  draws<-matrix(draws,ncol=length(mycoefs))
  yhatdf <- apply(draws,1,function(beta) {
    beta %*% t(as.matrix(predictdf))
  }) %>% as.data.frame
  names(yhatdf)<-1:reps
  yhatdf <- cbind(predictdf,yhatdf)
  yhatdf<-gather_(
    yhatdf,
    "rep",
    "yhat",
    as.character(1:reps)
  ) %>% data.table
  
  #get diff for each var
  returndf <- lapply(keyvars,function(v) {
    #v<-keyvars[1]
    if( v%in%names(yhatdf) ) {
      tmpdf <- yhatdf[
        ,
        .(
          yhat=mean(yhat)
        )
        ,
        by=c(
          'rep',
          v
        )
        ]
      tmpdf<-spread_(
        tmpdf,
        v,
        "yhat"
      )
      summarize.distribution2(tmpdf$`1` - tmpdf$`0`)
    } else {
      data.frame(mu=NA)
    }
  }) %>% rbind.fill
  returndf$var <- keyvars
  
  returndf
  
}


# #args
# reps <- 100
# m.tmp <- m
# predictdf <- popdf




