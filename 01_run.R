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
require(igraph)

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

#the indirect effect
race_indirect <- 0.75 
#this is helpful to equalize race=class=ability, roughly

#loop through these
loopdf<-expand.grid(
  #many worlds
  seed=1:100,
  #number of agents
  N_agents=1000,
  #network size
  netsize=100,
  #friend size,
  friendsize=0.8,#c(0.2,0.8),
  #income distribution
  income_dist=c('normal'),
  race_dist=c(0.36), #1 - white, nonhispanics in the USA  
  #betas_stage1
  beta_race_income=c(race_indirect),
  #betas_stage2
  beta_income_nhood=c(1),
  beta_race_nhood=c(0,race_indirect/10), #no/yes discrimination
  beta_ability_nhood=c(0),
  #betas_stage3
  beta_income_school=c(0),
  beta_ability_school=c(0.3),
  beta_race_school=c(0,race_indirect/10), #no/yes discrimination
  beta_nhood_school=c(1),
  #betas_stage4
  beta_income_earnings=c(0),
  beta_ability_earnings=c(0.3),
  beta_race_earnings=c(0,race_indirect/10), #no/yes discrimination
  beta_nhood_earnings=c(0),
  beta_school_earnings=c(1),
  #how are social networks formed
  network_formation='smallworld',#c('smallworld','random'),
  network_stage=c('random','nhood','school','earnings'),
  # share_random=c(0,1),
  # share_in_nhood=c(0,1),
  # share_in_school=c(0,1),
  # share_in_earnings=c(0,1),
  #how much does luck matter in our world
  luck=c(1),#,0.1)
  #rescale all vars to N(0,1) at every stage?
  rescale = T,
  #is this main or robustness
  main = "main",
  stringsAsFactors = F
)

#make discrimination vars vary together
disc_vars <- c(
  'beta_race_nhood',
  'beta_race_school',
  'beta_race_earnings'
)
tmp<-round(
  apply(
    loopdf[,disc_vars],
    1,
    sum
  ),2)==round(race_indirect*3/10,2) #floating point problem
loopdf<-loopdf[tmp,]

# #get rid of all shares which don't add up to 1
# sharevars<-str_detect(names(loopdf),"share\\_")
# tmp<-apply(
#   loopdf[,sharevars],1,sum
# )==1
# loopdf<-loopdf[tmp,]

#########################################################
#########################################################

# ROBUSTNESS CHECKS
robustness<-T
if(robustness) {
  
  #get basic loopdf; decide how many seeds you want
  robustness_seeds <- ifelse(max(loopdf$seed)==10,5,10)
  basedf<-loopdf[loopdf$seed%in%c(1:robustness_seeds),]
  
  #(1) direct effect >>> indirect effect
  #(roughly, aiming for 70% direct, 30% indirect)
  tmpdf<-basedf
  tmpdf$beta_race_earnings<-
    tmpdf$beta_race_nhood<-
    tmpdf$beta_race_school<-race_indirect/6
  tmpdf$beta_race_income<-race_indirect/4
  tmpdf$main<-"robustness_directindirect"
  loopdf<-rbind.fill(
    loopdf,
    tmpdf
  ) 
  
  #(2) race effect > class effect; class effect > race effect
  tmpdf<-basedf
  mult_factor <- 1.5
  racevars<-names(tmpdf)[str_detect(names(tmpdf),"beta_race")]
  classvars<-names(tmpdf)[str_detect(names(tmpdf),"beta_income")]
  for(v in racevars)
    tmpdf[[v]]<-mult_factor * tmpdf[[v]] #increase race effect
  for(v in classvars)
    tmpdf[[v]]<-tmpdf[[v]] / mult_factor #reduce class effect
  tmpdf$main<-"robustness_racebigger"
  loopdf<-rbind.fill(
    loopdf,
    tmpdf
  ) 
  tmpdf<-basedf
  for(v in racevars)
    tmpdf[[v]]<-tmpdf[[v]] / mult_factor 
  for(v in classvars)
    tmpdf[[v]]<-mult_factor * tmpdf[[v]] 
  tmpdf$main<-"robustness_classbigger"
  loopdf<-rbind.fill(
    loopdf,
    tmpdf
  ) 
  
}

#########################################################
#########################################################

#assign i's
loopdf$i<-1:nrow(loopdf); print(nrow(loopdf))
fulloutput<-lapply(loopdf$i,function(i) {
  
  #i<-1
  
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
  #how large are networks
  smallw_factor <- 
    thisrow$netsize/
    ((thisrow$friendsize * thisrow$netsize)/2)
  random_factor<-thisrow$friendsize

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
  #networks is a list, by agent, of who each is connected to
  if ( thisrow$network_formation == 'smallworld' ) {
    
    
    if(thisrow$network_stage=='random') {
      
      g <- sample_smallworld(
        1,
        nrow(agentsdf), #number of agents
        nrow(agentsdf)/(smallw_factor*10), #regulates # of connections
        0.05
      )
      g_list <- get.edgelist(g)
      networks<-lapply(1:nrow(agentsdf),function(i) {
        as.vector(neighbors(g,V(g)[i]))
      })
      
    } else if (thisrow$network_stage=='nhood') {
      
      #loop through neighborhoods
      networks <- lapply( 1:max(agentsdf$nhood_proper) ,function(i) {
        #i<-1
        #these are the agents in this nhood
        myagents <- agentsdf$agentid[agentsdf$nhood_proper==i]
        #in each neigbhorhood, create a network
        g <- sample_smallworld(
          1,
          length(myagents),
          length(myagents)/smallw_factor,
          0.05
        )
        g_list <- get.edgelist(g)
        tmpnetworks<-lapply(1:length(myagents),function(i) {
          #get neighbors, in terms of myagents vector
          myagents[as.vector(neighbors(g,V(g)[i]))]
        })
        names(tmpnetworks)<-myagents
        tmpnetworks
      }) %>% 
        Reduce(f='c') #makes it a single list
      #order this by agentid
      networks <- networks[order(as.numeric(names(networks)))] 
      networks <- unname(networks) #then it can be unnamed
      
    } else if (thisrow$network_stage=='school') {
      
      #loop through schools
      networks <- lapply( 1:max(agentsdf$school_proper) ,function(i) {
        #i<-1
        #these are the agents in this nhood
        myagents <- agentsdf$agentid[agentsdf$school_proper==i]
        #in each neigbhorhood, create a network
        g <- sample_smallworld(
          1,
          length(myagents),
          length(myagents)/smallw_factor,
          0.05
        )
        g_list <- get.edgelist(g)
        tmpnetworks<-lapply(1:length(myagents),function(i) {
          #get neighbors, in terms of myagents vector
          myagents[as.vector(neighbors(g,V(g)[i]))]
        })
        names(tmpnetworks)<-myagents
        tmpnetworks
      }) %>% 
        Reduce(f='c') #makes it a single list
      #order this by agentid
      networks <- networks[order(as.numeric(names(networks)))] 
      networks <- unname(networks) #then it can be unnamed
      
    } else if (thisrow$network_stage=='earnings') {
      
      #loop through schools
      networks <- lapply( 1:max(agentsdf$earnings_proper) ,function(i) {
        #i<-1
        #these are the agents in this nhood
        myagents <- agentsdf$agentid[agentsdf$earnings_proper==i]
        #in each neigbhorhood, create a network
        g <- sample_smallworld(
          1,
          length(myagents),
          length(myagents)/smallw_factor, 
          0.05
        )
        g_list <- get.edgelist(g)
        tmpnetworks<-lapply(1:length(myagents),function(i) {
          #get neighbors, in terms of myagents vector
          myagents[as.vector(neighbors(g,V(g)[i]))]
        })
        #get my own name, in terms of my agents vector
        names(tmpnetworks)<-myagents
        tmpnetworks
      }) %>% 
        Reduce(f='c') #makes it a single list
      #order this by agentid
      networks <- networks[order(as.numeric(names(networks)))] 
      networks <- unname(networks) #then it can be unnamed
      
    }
    
  } else if ( thisrow$network_formation=='random' ) {
    
    #if ties are formed at random, either overall or within-place
    #this is easier: we don't need to form smallwork networks, first
    
    networks<-lapply(1:thisrow$N_agents,function(j) {
      #print(j)
      #j<-1
      
      if(thisrow$network_stage=='random') {
        
        mynetwork<-sample(
          agentsdf$agentid[agentsdf$agentid!=j],
          thisrow$netsize * random_factor,
          replace=F
        )
        
      } else if (thisrow$network_stage=='nhood') {
        
        thisnhood<-agentsdf$nhood_proper[agentsdf$agentid==j]
        mynetwork<-sample(
          agentsdf$agentid[agentsdf$nhood_proper==thisnhood & agentsdf$agentid!=j],
          thisrow$netsize * random_factor,
          replace=F
        )
        
      } else if (thisrow$network_stage=='school') {
        
        thisschool<-agentsdf$school_proper[agentsdf$agentid==j]
        mynetwork<-sample(
          agentsdf$agentid[agentsdf$school_proper==thisschool & agentsdf$agentid!=j],
          thisrow$netsize * random_factor,
          replace=F
        )
        
      } else if (thisrow$network_stage=='earnings') {
        
        thisearnings<-agentsdf$earnings_proper[agentsdf$agentid==j]
        mynetwork<-sample(
          agentsdf$agentid[agentsdf$earnings_proper==thisearnings & agentsdf$agentid!=j],
          thisrow$netsize * random_factor,
          replace=F
        )
        
      }
      
      #return
      mynetwork
    })
    
  }

  ###fourth, everyone looks at their networks and draws inferences
  ###(we also loop through once extra, and calculate everything unconstrained;
  ###this gives us God's estimate; which we take as ground truth)
  inferencesdf<-lapply(1:(thisrow$N_agents+1),function(j) {
    
    #j<-1001
    #print(j)
    if( floor(j)%%200 == 0 )
      print( paste0("Agent ", j, " out of ",max(thisrow$N_agents)))
    
    
    #get my social space
    if(j<=thisrow$N_agents) {
      myfriends <- networks[[j]]
    } else {
      myfriends <- 1:thisrow$N_agents
    }
    mydf<-agentsdf[agentsdf$agentid%in%myfriends]
    

    
    #store dfs
    tmpdfs<-list()
    
    ###inequality of earnings
    tmpf<-ecdf(mydf$earnings_f) #for perceived pos
    tmpdf<-data.frame(
      agentid=j,
      model=c(
        'inequality',
        'totalss',
        'withinss',
        'betweenss',
        'perpos' #perceived position
      ),
      var="var",
      mu=c(
        var(mydf$earnings_f), #total variance
        (mydf$earnings_f - mean(mydf$earnings_f))^2 %>% sum, #total sum of squares
        tapply(mydf$earnings_f,mydf$race_i,function(x) sum((x - mean(x))^2 ) ) %>% sum, #within-group sum of squares
        (unlist(
          tapply(mydf$earnings_f,mydf$race_i,function(x) rep(mean(x),length(x)) )
        ) - mean(mydf$earnings_f))^2 %>% sum, #between sum of squares
        ifelse(
          j<=thisrow$N_agents,
          100 * tmpf(agentsdf$earnings_f[agentsdf$agentid==j]),
          NA
          )#agent's own perceived position in distribution
      )
    )
    tmpdfs[['descriptive']]<-tmpdf
    
    
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
    
    #get estimates of share of variance explained 
    mymods <- list(
      normal = m,
      race = mrace
    )
    tmpdf <- lapply(seq_along(mymods),function(k) {
      #k<-1
      thism <- mymods[[k]]
      thism_sum <- summary(thism)
      
      #get residual sum of squares
      rss <- sum(resid(thism)^2)
      #get total sum of squares
      tss <- sum ( 
        (thism$model$earnings_f - mean(thism$model$earnings_f))^2 
      )
      #get explained some of squares
      ess <- sum( 
        (thism$fitted.values - mean(thism$model$earnings_f))^2 
      )
      
      data.frame(
        agentid = j,
        model = names(mymods)[k],
        var = c("r2","adjr2","rss","ess","tss"), #r2 = 1 - rss/tss, or ess/tss
        mu = c(thism_sum$r.squared,thism_sum$adj.r.squared,rss,ess,tss)
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
    #networks=networks, #if we want to return networks..
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

