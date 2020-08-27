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

#########################################################
#########################################################

#set up graph prelims
require(ggplot2)

#initialize graphlist
gs.list<-list()

#quick function to outputdfs
output <- function(df,tmpname) {
  setwd(outputdir)
  if( str_detect(tmpname,"\\.pdf$") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$",".csv")
  write.csv(
    df,
    tmpname,
    row.names=F
  )
}

#########################################################
#########################################################

#load the image
setwd(filesdir)
load(
  '01_run.RData'
)

#fix loopdf
loopdf$network<-"random"
loopdf$network[loopdf$share_in_nhood==1]<-"nhood"
loopdf$network[loopdf$share_in_nhood==0.90]<- "nhood95"
loopdf$network[loopdf$share_in_school==1]<-"school"
loopdf$network[loopdf$share_in_school==0.90]<- "school95"
loopdf$network[loopdf$share_in_earnings==1]<-"earnings"
loopdf$network[loopdf$share_in_earnings==0.90]<-"earnings95"
loopdf$share_in_school<-
  loopdf$share_in_earnings<-
  loopdf$share_in_nhood<-
  loopdf$share_random<-NULL
loopdf

tmp<-loopdf$beta_race_school==0 & 
  loopdf$beta_race_earnings==0
loopdf$world[tmp]<-"nodiscrimination"
tmp<-loopdf$beta_race_school>0 & loopdf$beta_race_earnings>0
loopdf$world[tmp]<-"yesdiscrimination"

#########################################################
#########################################################

#(6) MAP INFERENCES TO REDISTRIBUTIVE PREFERENCES

#inferences are important b/c they help people think about redistribution? 

#what might this mapping look like? 

#i. descriptive inequality
#the more inequality in final earnings, the more redistribution

#ii. meritocracy
#less meritocracy, less redistribution, so:
#the larger class/race coefficients, larger redistribution
#later ability coefficient, less redistribution

#iii. own income
#the poorer you are, the more you want redistribution

#iv. luck 
#the more luck matters, the more you want redistribution

#how each of these matters can itself be made to vary

#below, standard model is p(redistribution) rises 
#+ with class coefficent
#+ with race coefficient 
#- with ability coefficient
#- with own income

#we explore, optionally
#+ with descriptive inequality
#+ with luck/share of variance unexplained



#########################################################
#########################################################

#gather all the info we need for the mapping
inferencesdf<-lapply(
  fulloutput,
  function(x) x$inferencesdf
) %>% 
  rbind.fill %>%
  data.table
tmpdf<-inferencesdf[
  ,
  .(
    descriptive = mu[model=='inequality'],
    class = mu[model=='normal' & var=='income_i'],
    race = mu[model=='normal' & var=='race_i'],
    ability = mu[model=='normal' & var=='ability_i'],
    luck = 1 - mu[model=='normal' & var=='r2']
  )
  ,
  by=c(
    'agentid',
    'i'
  )
  ]
tmpdf2<-lapply(
  fulloutput,
  function(x) x$agentsdf[,c('agentid','i','earnings_f')]
) %>%
  rbind.fill %>%
  data.table
names(tmpdf2)[names(tmpdf2)=='earnings_f'] <- 'ownincome'
fulldf <- merge(
  tmpdf,
  tmpdf2,
  by=c('i','agentid')
)
#standardize all vars across all agents/sims
tmpvars<-c('ability','class','descriptive','ownincome','luck','race')
for(v in tmpvars) {
  fulldf[[v]] <- scale(fulldf[[v]])[,1]
}

#we are going to map these via inverse logit
#to obtain attitudes towards redistribution on 0-1 scale
coefsdf <- expand.grid(
  ##these are pretty obvious
  theta_class = c(1),
  theta_race = c(1),
  theta_ability = c(-1),
  theta_ownincome = c(-1),
  ###these, up for grabs;
  ###we'll want a preference, 
  ###and then we can have robustness
  theta_descriptive = c(0,1),
  theta_luck = c(0,1)
)

# #trim to preferred model
# tmp<-coefsdf$theta_descriptive==0 & 
#   coefsdf$theta_class==1 & 
#   coefsdf$theta_race==1 & 
#   coefsdf$theta_ability==-1 &
#   coefsdf$theta_ownincome==1 & 
#   coefsdf$theta_luck==1
#coefsdf <- coefsdf[tmp,]

coefsdf$modnum <- 1:nrow(coefsdf)

#prep for loop
mycoefs <- names(coefsdf)[!names(coefsdf)%in%c('modnum')]
myvars <- str_replace(mycoefs,"theta\\_","")

#now, loop through
tmpseq.i <- 1:nrow(coefsdf)
phatdf <- lapply(tmpseq.i,function(i) {
  #i<-1
  thisrow <- coefsdf[i,]
  thiscoefs <- thisrow[,mycoefs]

  #predict the 
  intercept <- 0 #this is for scaling purposes
  yhat <- intercept + as.matrix(fulldf[,myvars,with=F]) %*% 
    t(as.matrix(thiscoefs))

  #apply inverse logit link to this yhats
  phat <- 1/(1 + exp(-1 * yhat)) 
  
  data.frame(
    modnum=thisrow$modnum,
    i=fulldf$i,
    agentid=fulldf$agentid,
    phat = as.vector(phat),
    owincome_ptile = ecdf(fulldf$ownincome)(fulldf$ownincome)
  )
}) %>% rbind.fill

#merge
fulldf <- merge(
  phatdf,
  coefsdf,
  by='modnum'
)
fulldf <- merge(
  fulldf,
  loopdf,
  by='i'
) %>% data.table

#########################################################
#########################################################

#PLOTTING PREP

#fix groups
tmplevels<-c(
  "random",
  "nhood",
  "nhood95",
  "school",
  "school95",
  "earnings",
  "earnings95"
)
tmplabels<-c(
  "No Segregation",
  "Neighborhood",
  "Mostly Neighborhood",
  "School",
  "Mostly School",
  "Income",
  "Mostly Income"
)
fulldf$network<-factor(
  fulldf$network,
  tmplevels,
  tmplabels
)

#explain models
fulldf$modname <- "f(inferences)"
fulldf$modname[fulldf$theta_luck==1]<-str_replace(
  fulldf$modname[fulldf$theta_luck==1],"\\)",",luck)"
)
fulldf$modname[fulldf$theta_descriptive==1]<-str_replace(
  fulldf$modname[fulldf$theta_descriptive==1],"\\)",",inequality)"
)
tmplevels<-c(
  "f(inferences)",
  "f(inferences,luck)",
  "f(inferences,inequality)",
  "f(inferences,luck,inequality)"
)
fulldf$modname <- factor(
  fulldf$modname,
  tmplevels
)


#########################################################
#########################################################

#PLOT MEDIAN p(Redistribution)
plotdf <- fulldf[
  ,
  .(
    mu=median(phat,na.rm=T)
  )
  ,
  by=c(
    'modname',
    'network',
    'seed'
  )
]
plotdf<-plotdf[
  ,
  .(
    mu=quantile(mu,0.5),
    mu.min=quantile(mu,0.025),
    mu.max=quantile(mu,0.975)
  )
  ,
  by=c(
    'modname',
    'network'
  )
  ]

g.tmp<-ggplot(
  plotdf,
  aes(
    x=modname,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    color=network
  )
) +
  geom_point(
    size=2,
    position=position_dodge(0.4)
  ) +
  geom_linerange(
    size=1.25,
    position=position_dodge(0.4)
  ) +
  scale_color_discrete(
    name="Social World"
  ) +
  xlab("") + 
  ylab("Median Attitude Towards Redistribution\n") +
  theme_bw() 

tmpname<-"fig_redistribution.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=6*1.5,
  height=3*1.5
)
output(plotdf,tmpname)


#########################################################
#########################################################


#OUTPUT
#output graphlist
setwd(outputdir)
this.sequence<-seq_along(gs.list)
for(i in this.sequence) {
  print(
    paste0(
      "saving ",i," of ",length(this.sequence)
    )
  )
  thiselement<-gs.list[[i]]
  ggsave(
    filename=thiselement$filename,
    plot=thiselement$graph,
    width=thiselement$width,
    height=thiselement$height
  )
  Sys.sleep(0.5)
}








