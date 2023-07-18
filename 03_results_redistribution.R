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

#set up stats list
stats.list <- list()

#########################################################
#########################################################

#load the image
setwd(filesdir)
load(
  '01_run.RData'
)

#in case running on diff computers
#and .RData has reset wdirs
#you'll need to set them back
filesdir<-getwd()
setwd('..')
homedir<-getwd()
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

#below, standard model is p(redistribution) = f(...)
#+ with class coefficent
#+ with race coefficient (total)
#- with ability coefficient
#- with own income
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
  agentid<=max(loopdf$N_agents)
  ,
  .(
    descriptive = mu[model=='inequality'], 
    class = mu[model=='normal' & var=='income_i'], #total effect of class
    race = mu[model=='race' & var=='race_i'], #total effect of race
    ability = mu[model=='normal' & var=='ability_i'], #total effect of ability
    luckest = 1 - mu[model=='normal' & var=='r2']
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
tmpvars<-c('ability','class','descriptive','ownincome','luckest','race')
for(v in tmpvars) {
  fulldf[[v]] <- scale(fulldf[[v]])[,1]
}

#########################################################
#########################################################

#mapping

#we are going to map these via inverse logit
#to obtain attitudes towards redistribution on 0-1 scale
coefsdf <- expand.grid(
  ##this is default
  theta_ownincome = c(-1),
  ##these vary together
  theta_class = c(0,1),
  theta_race = c(0,1),
  theta_ability = c(0,-1),
  ##inequality
  theta_descriptive = c(0,1),
  ##luck
  theta_luckest = c(0,1)
)

#number mods
coefsdf$modnum <- 1:nrow(coefsdf)

#prep for loop
mycoefs <- names(coefsdf)[!names(coefsdf)%in%c('modnum')]
myvars <- str_replace(mycoefs,"theta\\_","")

####

#remove all in which coefs don't vary together
tmp<-coefsdf$theta_race==coefsdf$theta_class & 
  coefsdf$theta_class==(-1*coefsdf$theta_ability)
coefsdf<-coefsdf[tmp,]

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
  #phat <- 1/(1 + exp(-1 * yhat)) #equivalent
  phat <- exp(yhat)/( exp(yhat) + 1)
  
  data.frame(
    modnum=thisrow$modnum,
    i=fulldf$i,
    agentid=fulldf$agentid,
    phat = as.vector(phat),
    ownincome = fulldf$ownincome,
    owincome_ptile = ecdf(fulldf$ownincome)(fulldf$ownincome)
  )
}) %>% rbind.fill %>% data.table

#########################################################
#########################################################

#fix loopdf
# loopdf$network<-"random"
# loopdf$network[loopdf$share_in_nhood==1]<-"nhood"
# loopdf$network[loopdf$share_in_nhood==0.90]<- "nhood95"
# loopdf$network[loopdf$share_in_school==1]<-"school"
# loopdf$network[loopdf$share_in_school==0.90]<- "school95"
# loopdf$network[loopdf$share_in_earnings==1]<-"earnings"
# loopdf$network[loopdf$share_in_earnings==0.90]<-"earnings95"
# loopdf$share_in_school<-
#   loopdf$share_in_earnings<-
#   loopdf$share_in_nhood<-
#   loopdf$share_random<-NULL
# loopdf

tmp<-loopdf$beta_race_school==0 & 
  loopdf$beta_race_earnings==0
loopdf$world[tmp]<-"nodiscrimination"
tmp<-loopdf$beta_race_school>0 & loopdf$beta_race_earnings>0
loopdf$world[tmp]<-"yesdiscrimination"

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
  "Integrated",
  "Neighborhood",
  "Mostly Neighborhood",
  "School",
  "Mostly School",
  "Workplace",
  "Mostly Workplace"
)
fulldf$network_stage<-factor(
  fulldf$network_stage,
  tmplevels,
  tmplabels
)

#name models
fulldf$modname<-"f(ownincome"
tmp<-(
  fulldf$theta_class==1 & 
    fulldf$theta_race==1 & 
    fulldf$theta_ability==-1
)
fulldf$modname[tmp] <- paste0(fulldf$modname[tmp],",coefs")
tmp<-fulldf$theta_descriptive==1
fulldf$modname[tmp] <- paste0(fulldf$modname[tmp],",inequality")
tmp<-fulldf$theta_luck==1
fulldf$modname[tmp] <- paste0(fulldf$modname[tmp],",luck")
fulldf$modname <- paste0(fulldf$modname,")")

if(length(table(table(fulldf$modname)))!=1)
  stop('the naming hasnt worked properly; too many lumped in one')

#########################################################
#########################################################

#PLOT MEDIAN p(Redistribution)
plotdf <- fulldf[
  main=='main' & 
  world=='yesdiscrimination' #plot in n discrimination world
  ,
  .(
    mu=median(phat,na.rm=T)
  )
  ,
  by=c(
    'modname',
    'network_stage',
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
    'network_stage'
  )
  ]

#decide which to include and how to order
tmplevels<-c(
  "f(ownincome)",
  "f(ownincome,coefs)",
  "f(ownincome,inequality)",
  "f(ownincome,luck)",
  "f(ownincome,coefs,inequality,luck)"
)
plotdf<-plotdf[plotdf$modname%in%tmplevels,]
plotdf$modname <- factor(
  plotdf$modname,
  rev(tmplevels)
)

tmpcolors<-c(
  'Ground Truth'='grey',
  'Integrated'='#e41a1c',
  'Neighborhood'='#377eb8',
  'School'='#4daf4a',
  'Workplace'='#984ea3'
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=modname,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    color=network_stage
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
  scale_color_manual(
    name="Social World",
    values=tmpcolors
  ) +
  xlab("") + 
  ylab("\nMedian Attitude Towards Redistribution") +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position='top',
    legend.direction='horizontal'
  )

setwd(outputdir)
tmpname<-"fig_redistribution.pdf"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=7*1.25,
  height=4*1.25
)
output(plotdf,tmpname)


#########################################################
#########################################################

