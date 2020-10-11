########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(data.table)
require(tidyr)
require(rprojroot)

#extras
require(MASS)
require(ggplot2)
require(relaimpo)

homedir<-find_root(
  criterion=has_file('_inferentialspaces.Rproj')
)
outputdir<-file.path(
  homedir,
  "output"
)
filesdir<-file.path(
  homedir,
  "files"
)

#load functions
setwd(homedir); dir()
source('functions.R')

#########################################################
#########################################################

#set up graph prelims
require(ggplot2)

#initialize graphlist
gs.list<-list()

#########################################################
#########################################################

#loop through these
loopdf<-expand.grid(
  #many worlds
  seed=1:100,
  #number of agents
  N_agents=1000,
  #network size
  netsize=100,
  #income distribution
  income_dist=c('normal'),
  race_dist=c(0.36), #1 - white, nonhispanics in the USA  
  #betas_stage1
  beta_race_income=c(1),
  #betas_stage2
  beta_income_nhood=c(1),
  beta_race_nhood=c(1),
  beta_ability_nhood=c(0),
  #betas_stage3
  beta_income_school=c(0),
  beta_ability_school=c(0.25),
  beta_race_school=c(0,0.1), #no/yes discrimination
  beta_nhood_school=c(1),
  #betas_stage4
  beta_income_earnings=c(0),
  beta_ability_earnings=c(0.25),
  beta_race_earnings=c(0,0.1), #no/yes discrimination
  beta_nhood_earnings=c(0),
  beta_school_earnings=c(1),
  #how is network distributed across stages
  share_random=c(0,1),#),1),#0.1),
  share_in_nhood=c(0,1),#,1),#0.90),
  share_in_school=c(0,1),#1),#0.90),
  share_in_earnings=c(0,1),#1),#0.90),
  #how much does luck matter in our world
  luck=c(1),#,0.1)
  #rescale all vars to N(0,1) at every stage?
  rescale = T,
  stringsAsFactors = F
)

#get rid of all race permuationts
#where race_dist is 0
tmp<-loopdf$race_dist==1 &
  loopdf$beta_race_income!=0 & 
  loopdf$beta_race_nhood!=0 & 
  loopdf$beta_race_school!=0 &
  loopdf$beta_race_earnings!=0
loopdf<-loopdf[!tmp,]

#drop the superfluous worlds
tmp<-(
  loopdf$beta_race_school==0 & 
    loopdf$beta_race_earnings!=0
) |  (
  loopdf$beta_race_earnings==0 & 
    loopdf$beta_race_school!=0
)
loopdf<-loopdf[!tmp,]

#get rid of all shares which don't add up to 1
sharevars<-str_detect(names(loopdf),"share\\_")
tmp<-apply(
  loopdf[,sharevars],1,sum
)==1
loopdf<-loopdf[tmp,]

#assign i's
loopdf$i<-1:nrow(loopdf); print(nrow(loopdf))
fulloutput<-lapply(loopdf$i,function(i) {
  
  #i<-11
  
  #tracker
  pct_done <- round(i/nrow(loopdf) * 100)
  if(i==1)
    print(paste0(nrow(loopdf),' iterations'))
  print(paste0("Iteration ",i))
  if(pct_done%%10==0)
    print(paste0(pct_done,"% done"))
  
  thisrow<-loopdf[loopdf$i==i,]
  
  #params
  set.seed(thisrow$seed)
  #all groups will be the same size
  N_groups <- thisrow$N_agents/
    thisrow$netsize
  
  #generate agents
  agentsdf<-data.frame(
    agentid=1:thisrow$N_agents,
    race_i=as.numeric(pnorm(rnorm(thisrow$N_agents))>thisrow$race_dist),
    ability_i=rnorm(thisrow$N_agents)
  ) %>% data.table
  #get ability quantile
  tmpqs<-seq(0,1,length.out = 101)
  cutpoints<-quantile(agentsdf$ability_i,tmpqs)
  agentsdf$ability_i_q<-cut(
    agentsdf$ability_i,
    breaks=cutpoints,
    include.lowest = T
  ) %>% as.numeric - 1 #0 to 99
  
  ###stage one, get inherited income; f(race + luck)
  agentsdf$income_i <- 
    #effect of race on inherited income
    thisrow$beta_race_income * agentsdf$race_i +
    #brute luck
    rnorm(thisrow$N_agents,sd=thisrow$luck)
  
  #rescale?   
  if(thisrow$rescale) 
    agentsdf$income_i <- scale(agentsdf$income_i)
  
  #income quanitle
  tmpqs<-seq(0,1,length.out = 101)
  cutpoints<-quantile(agentsdf$income_i,tmpqs)
  agentsdf$income_i_q<-cut(
    agentsdf$income_i,
    breaks=cutpoints,
    include.lowest = T
  ) %>% as.numeric - 1 #0 to 99
  
  ###stage two, get nhood
  agentsdf$nhood_raw <-
    #impact of race on nhood
    thisrow$beta_race_nhood * agentsdf$race_i + 
    #impact of income on nhood
    thisrow$beta_income_nhood * agentsdf$income_i + 
    #impact of ability on nhood
    thisrow$beta_ability_nhood * agentsdf$ability_i +
    #noise
    rnorm(thisrow$N_agents,sd=thisrow$luck)
  
  #rescale?
  if(thisrow$rescale) 
    agentsdf$nhood_raw <- scale(agentsdf$nhood_raw)
  
  
  #this gets the nhood proper name
  tmpqs<-seq(0,1,length.out = N_groups + 1) 
  cutpoints<-quantile(agentsdf$nhood_raw,tmpqs)
  agentsdf$nhood_proper<-cut(
    agentsdf$nhood_raw,
    breaks=c(cutpoints),
    include.lowest = T
  ) %>% as.numeric 
  
  ###stage three, get school they go to
  agentsdf$school_raw <-
    #impact of race on school
    thisrow$beta_race_school * agentsdf$race_i + 
    #impact of ability on school
    thisrow$beta_ability_school * agentsdf$ability_i +
    #impact of inherited income on school
    thisrow$beta_income_school * agentsdf$income_i + 
    #impact of nhood on school
    thisrow$beta_nhood_school * agentsdf$nhood_raw + 
    #noise
    rnorm(thisrow$N_agents,sd=thisrow$luck)
  
  #rescale?
  if(thisrow$rescale) 
    agentsdf$school_raw <- scale(agentsdf$school_raw)
  
  #this gets the school proper name
  tmpqs<-seq(0,1,length.out = N_groups + 1)
  cutpoints<-quantile(agentsdf$school_raw,tmpqs)
  agentsdf$school_proper<-cut(
    agentsdf$school_raw,
    breaks=c(cutpoints),
    include.lowest = T
  ) %>% as.numeric 
  
  ###stage four, get their final earnings
  agentsdf$earnings_f <-
    #impact of race on earnings
    thisrow$beta_race_earnings * agentsdf$race_i + 
    #impact of ability on earnings
    thisrow$beta_ability_earnings * agentsdf$ability_i +
    #impact of inherited income on earnings
    thisrow$beta_income_earnings * agentsdf$income_i + 
    #impact of your nhood on earnings
    thisrow$beta_nhood_earnings * agentsdf$nhood_raw + 
    #impact of school on earnings
    thisrow$beta_school_earnings * agentsdf$school_raw + 
    #noise
    rnorm(thisrow$N_agents,sd=thisrow$luck)
  
  #rescale?
  if(thisrow$rescale) 
    agentsdf$earnings_f <- scale(agentsdf$earnings_f)
  
  #this splits them into income groups
  tmpqs<-seq(0,1,length.out = N_groups + 1)
  cutpoints<-quantile(agentsdf$earnings_f,tmpqs)
  agentsdf$earnings_proper<-cut(
    agentsdf$earnings_f,
    breaks=c(cutpoints),
    include.lowest = T
  ) %>% as.numeric 
  
  #we also want earnings centiles
  #for the quantile regession, a la Chetty et al
  tmpqs<-seq(0,1,length.out = 100 + 1)
  cutpoints<-quantile(agentsdf$earnings_f,tmpqs)
  agentsdf$earnings_f_q<-cut(
    agentsdf$earnings_f,
    breaks=c(cutpoints),
    include.lowest = T
  ) %>% as.numeric
  
  ###they form social networks
  networks<-lapply(1:thisrow$N_agents,function(j) {
    #print(j)
    #j<-1
    
    #go through different stages and form networks
    mynetwork<-c()
    
    #random
    mynetwork<-c(
      mynetwork,
      sample(
        agentsdf$agentid[agentsdf$agentid!=j],
        thisrow$share_random * thisrow$netsize * 0.8
      )
    )
    
    #nhood
    thisnhood<-agentsdf$nhood_proper[agentsdf$agentid==j]
    mynetwork<-c(
      mynetwork,
      sample(
        agentsdf$agentid[agentsdf$nhood_proper==thisnhood & agentsdf$agentid!=j],
        thisrow$share_in_nhood * thisrow$netsize * 0.8,
        replace=F
      )
    )
    
    #school
    thisschool<-agentsdf$school_proper[agentsdf$agentid==j]
    mynetwork<-c(
      mynetwork,
      sample(
        agentsdf$agentid[agentsdf$school_proper==thisschool & agentsdf$agentid!=j],
        thisrow$share_in_school * thisrow$netsize * 0.8,
        replace=F
      )
    )
    
    #earnings
    thisearnings<-agentsdf$earnings_proper[agentsdf$agentid==j]
    mynetwork<-c(
      mynetwork,
      sample(
        agentsdf$agentid[agentsdf$earnings_proper==thisearnings & agentsdf$agentid!=j],
        thisrow$share_in_earnings * thisrow$netsize * 0.8,
        replace=F
      )
    )
    
    #return
    mynetwork
  })
  
  ###fourth, everyone looks at their networks and draws inferences
  inferencesdf<-lapply(1:thisrow$N_agents,function(j) {
    
    #j<-62
    #print(j)
    if( floor(j)%%200 == 0 )
      print( paste0("Agent ", j, " out of ",max(thisrow$N_agents)))
    
    
    #get my social space
    myfriends <- networks[[j]]
    mydf<-agentsdf[agentsdf$agentid%in%myfriends]
    
    #store dfs
    tmpdfs<-list()
    
    ###inequality of earnings
    tmpdf<-data.frame(
      agentid=j,
      model=c(
        'inequality',
        'totalss',
        'withinss',
        'betweenss'
      ),
      var="var",
      mu=c(
        var(mydf$earnings_f),
        (mydf$earnings_f - mean(mydf$earnings_f))^2 %>% sum,
        tapply(mydf$earnings_f,mydf$race_i,function(x) sum((x - mean(x))^2 ) ) %>% sum,
        (unlist(
            tapply(mydf$earnings_f,mydf$race_i,function(x) rep(mean(x),length(x)) )
          ) - mean(mydf$earnings_f))^2 %>% sum
      )
    )
    tmpdfs[['descriptive']]<-tmpdf
    
    ggplot(
      mydf,
      aes(
        x=earnings_f,
        group=race_i,
        fill=race_i
      )
    ) + 
      geom_density()
    
    
    ###causal inference about inequality of earnings
    
    #normal model
    m<-lm(
      data=mydf,
      formula=earnings_f ~ 
        income_i + 
        ability_i + 
        race_i
    )
    #save the inferneces
    m.tmp<-m
    msum<-summary(m.tmp)
    tmpdf<-data.frame(msum$coefficients)
    names(tmpdf)<-c("mu","se","tval","pval")
    tmpdf$var<-row.names(tmpdf)    
    row.names(tmpdf)<-NULL
    #add any missing rows
    allvars<-attr(m$terms,'term.labels')
    if ( sum(!allvars%in%tmpdf$var) > 0) {
      newrow<-data.frame(
        var = allvars[!allvars%in%tmpdf$var]
      ) 
    } else {
      newrow <- NULL
    }
    tmpdf<-rbind.fill(tmpdf,newrow)
    tmpdf$agentid<-j
    tmpdf$model<-'normal'
    tmpdfs[['causal_normal']]<-tmpdf
    
    #save the relaimpo inferences, separately
    #relaimpo doesn't run when a var (i.e. race) cant be estimated
    #so we have to drop the vars which yield NA inferences
    relimp_vars <- tmpdf$var[!is.na(tmpdf$mu) & tmpdf$var!="(Intercept)"]
    m.relimp <- calc.relimp(
      object=mydf$earnings_f,
      x = mydf[,relimp_vars,with=F]
    )
    tmpdf <- data.frame(
      mu = c(1 - m.relimp@R2,m.relimp@lmg),
      var = c("luck",names(m.relimp@lmg)),
      agentid = j,
      model = 'normal_relimp'
    )
    row.names(tmpdf)<-NULL
    tmpdfs[['causal_normal_relimp']] <- tmpdf
    
    #DEPRECATED
    # #quantile model
    # mq<-lm(
    #   data=mydf,
    #   formula=earnings_f_q ~ 
    #     income_i_q + 
    #     ability_i_q + 
    #     race_i
    # )
    # m.tmp<-mq
    # msum<-summary(m.tmp)
    # tmpdf<-data.frame(msum$coefficients)
    # names(tmpdf)<-c("mu","se","tval","pval")
    # tmpdf$var<-row.names(tmpdf) 
    # row.names(tmpdf)<-NULL
    # #add any missing rows
    # allvars<-attr(mq$terms,'term.labels')
    # if ( sum(!allvars%in%tmpdf$var) > 0 ) {
    #   newrow<-data.frame(
    #     var = allvars[!allvars%in%tmpdf$var]
    #   ) 
    # } else {
    #   newrow <- NULL
    # }
    # tmpdf$var<-str_replace(tmpdf$var,"\\_q$","") #rename
    # tmpdf<-rbind.fill(tmpdf,newrow)
    # tmpdf$agentid<-j
    # tmpdf$model<-'quantile'
    # tmpdfs[['causal_quantile']]<-tmpdf
    mq <- NULL 
    
    #race-only model
    mrace<-lm(
      data=mydf,
      formula=earnings_f ~ 
        race_i
    )
    m.tmp<-mrace
    msum<-summary(m.tmp)
    tmpdf<-data.frame(msum$coefficients)
    names(tmpdf)<-c("mu","se","tval","pval")
    tmpdf$var<-row.names(tmpdf) 
    row.names(tmpdf)<-NULL
    #add any missing rows
    allvars<-attr(mrace$terms,'term.labels')
    if ( sum(!allvars%in%tmpdf$var) > 0) {
      newrow<-data.frame(
        var = allvars[!allvars%in%tmpdf$var]
      ) 
    } else {
      newrow <- NULL
    }
    tmpdf<-rbind.fill(tmpdf,newrow)
    tmpdf$agentid<-j
    tmpdf$model<-'race'
    tmpdfs[['causal_race']]<-tmpdf
    
    #discrimination only
    mdiscrimination <- lm(
      data=mydf,
      formula=earnings_f ~ 
        race_i + 
        nhood_raw
    )
    m.tmp<-mdiscrimination
    msum<-summary(m.tmp)
    tmpdf<-data.frame(msum$coefficients)
    names(tmpdf)<-c("mu","se","tval","pval")
    tmpdf$var<-row.names(tmpdf) 
    row.names(tmpdf)<-NULL
    #add any missing rows
    allvars<-attr(mdiscrimination$terms,'term.labels')
    if ( sum(!allvars%in%tmpdf$var) > 0) {
      newrow<-data.frame(
        var = allvars[!allvars%in%tmpdf$var]
      ) 
    } else {
      newrow <- NULL
    }
    tmpdf<-rbind.fill(tmpdf,newrow)
    tmpdf$agentid<-j
    tmpdf$model<-'discrimination'
    tmpdfs[['causal_discrimination']]<-tmpdf
    
    #get estimates of share explained via 42
    mymods <- list(
      normal = m,
      #quantile = mq,
      race = mrace,
      discrimination = mdiscrimination
    )
    tmpdf <- lapply(seq_along(mymods),function(k) {
      thism <- mymods[[k]]
      thism_sum <- summary(thism)
      data.frame(
        agentid = j,
        model = names(mymods)[k],
        var = c("r2","adjr2"),
        mu = c(thism_sum$r.squared,thism_sum$adj.r.squared)
      )
    }) %>% rbind.fill
    tmpdfs[['variance_explained']]<-tmpdf
    
    #combine
    returndf<-rbind.fill(tmpdfs)
    
    #return
    returndf
    
  }) %>% rbind.fill
  
  #add indices to the datasets
  agentsdf$i<-i
  inferencesdf$i<-i
  
  #returning all info from these worlds
  returnlist<-list(
    agentsdf=agentsdf,
    #networks=networks,
    inferencesdf=inferencesdf
  )
  
}) 

#########################################################
#########################################################

#save out
setwd(filesdir)
save.image(
  '01_run.RData'
)

