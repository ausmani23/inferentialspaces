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

#retrieve inferences, 
infdf<-lapply(
  fulloutput,
  function(x) x$inferencesdf
) %>% 
  rbind.fill %>%
  data.table
rm(fulloutput) #don't keep this in memory

#use the N+1'th agent as baseline
infdf$se<-
  infdf$tval<-
  infdf$pval<-
  # infdf$mu.min<-
  # infdf$mu.max<-
  # infdf$pval.class<-
  NULL

#set N+1th agent to be the baseline
infdf<-dcast.data.table(
  infdf,
  i + model + var ~ agentid,
  value.var='mu'
)
tmp<-names(infdf)==unique(loopdf$N_agents+1)
names(infdf)[tmp]<-"baseline"
gathcols<-1:max(loopdf$N_agents) %>% as.character
infdf<-melt.data.table(
  infdf,
  variable.name="agentid",
  value.name="mu",
  measure.vars=gathcols
) %>% data.table
infdf$mu_bias<- infdf$mu - infdf$baseline
#infdf$baseline<-NULL

#fix vars
infdf$var %>% unique
infdf$oldvar<-infdf$var
tmplevels<-c(
  '(Intercept)',
  'race_i',
  'income_i',
  'ability_i',
  ####
  "adjr2",
  "r2",
  'tss',
  'rss',
  'ess',
  "nhood_raw",
  "school_raw",
  "logvar"
  
)
tmplabels<-c(
  "Intercept",
  "Race",
  "Class",
  "Ability",
  ####
  "Luck (Adj.)",
  "Luck",
  'Total SS',
  'Residual SS',
  'Estimated SS',
  "Neighborhood",
  "School",
  "Inequality"
)
infdf$var<-factor(
  infdf$var,
  tmplevels,
  tmplabels
)

#########################################################
#########################################################

#fix loopdf
#loopdf$network<-loopdf$network_stage
# loopdf$network[loopdf$share_in_nhood==1]<-"nhood"
# loopdf$network[loopdf$share_in_school==1]<-"school"
# loopdf$network[loopdf$share_in_earnings==1]<-"earnings"
# loopdf$share_in_school<-
#   loopdf$share_in_earnings<-
#   loopdf$share_in_nhood<-
#   loopdf$share_random<-NULL
#loopdf

tmp<-loopdf$beta_race_school==0 & 
  loopdf$beta_race_earnings==0 & 
  loopdf$beta_race_nhood==0
loopdf$world[tmp]<-"nodiscrimination"
tmp<-loopdf$beta_race_school>0 & 
  loopdf$beta_race_earnings>0 &
  loopdf$beta_race_nhood>0
loopdf$world[tmp]<-"yesdiscrimination"

#add extra information, from loopdf
infdf<-merge.data.table(
  data.table(loopdf),
  infdf
) 
infdf[,i:=NULL] #not necessary, post merge

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
infdf$group<-factor(
  infdf$network_stage,
  tmplevels,
  tmplabels
)
infdf[,network_stage:=NULL] #rename

#########################################################
#########################################################

### STATS

### is race = class = ability, roughly? 

tmpdf1<-infdf[
  model=="normal" & 
    var%in%c('Class','Ability') & #total effects of class,a bility
    world=='yesdiscrimination' &
    main=="main"
]
tmpdf2<-infdf[
  model%in%c('race') & #total effect of race
    var=='Race' &
    world=='yesdiscrimination' & 
    main=="main"
]
tmpdf<-rbind.fill(tmpdf1,tmpdf2) %>% data.table
tmpdf<-tmpdf[
  ,
  .(
    mu = median(baseline,na.rm=T)
  )
  ,
  by=c(
    'oldvar',
    'seed'
  )
]
tmpdf<-spread(tmpdf,oldvar,mu) %>% data.table

tmpdf$class_ability <- tmpdf$income_i - tmpdf$ability_i
tmpdf$class_race <- tmpdf$income_i - tmpdf$race_i
tmpdf$race_ability <- tmpdf$race_i - tmpdf$ability_i

#i want these all to be close to zero, roughly
median(tmpdf$class_ability) 
median(tmpdf$class_race) 
median(tmpdf$race_ability) 
#pretty well-calibrated

### direct > indirect

tmpdf<-infdf[
  model%in%c('race','normal') & #total effect of race
    var=='Race' &
    world=='yesdiscrimination' & 
    main%in%c("main")
]
tmpdf<-tmpdf[
  ,
  .(
    mu = median(baseline,na.rm=T)
  )
  ,
  by=c(
    'oldvar',
    'seed',
    'model',
    'main'
  )
]
tmpdf<-spread(tmpdf,model,mu) %>% data.table

#centered roughly where we want.. 
quantile(tmpdf$normal/tmpdf$race,c(0.025,0.5,0.975)) #pretty good

### direct vs. indirect robustness test
#we want race = class = ability to still be roughly true
#but we want the ratio of direct/indirect to have changed.. 

tmpdf1<-infdf[
  model=="normal" & 
    var%in%c('Class','Ability','Race') & #total effects of class,a bility
    world=='yesdiscrimination' &
    main=="robustness_directindirect"
]
tmpdf2<-infdf[
  model%in%c('race') & #total effect of race
    var=='Race' &
    world=='yesdiscrimination' & 
    main=="robustness_directindirect"
]
tmpdf<-rbind.fill(tmpdf1,tmpdf2) %>% data.table
tmpdf$varmodel<-paste0(tolower(tmpdf$var),"_",tmpdf$model)
tmpdf<-tmpdf[
  ,
  .(
    mu = median(baseline,na.rm=T)
  )
  ,
  by=c(
    'varmodel',
    'seed'
  )
]
tmpdf<-spread(tmpdf,varmodel,mu) %>% data.table

#direct indirect
quantile(tmpdf$race_normal/tmpdf$race_race,c(0.025,0.5,0.975)) #perfect

#race = class = ability?
tmpdf$class_ability <- tmpdf$class_normal - tmpdf$ability_normal
tmpdf$class_race <- tmpdf$class_normal - tmpdf$race_race
tmpdf$race_ability <- tmpdf$race_race - tmpdf$ability_normal

#i want these all to be close to zero, roughly
median(tmpdf$class_ability) 
median(tmpdf$class_race) 
median(tmpdf$race_ability) 
#race < smaller than the rest, 
#but this is just robustness check,
#no big deal.. 

#########################################################
#########################################################

### PLOTS

## DESCRIPTIVE AND CAUSAL INFERENCES 
# plot all descriptive and causl infererence about inequality

tmpdf1<-infdf[
  model=="normal" & 
    var%in%c('Class','Race','Ability') &
    world=='yesdiscrimination' &
    main=="main" #not robustness 
]
tmpdf1$inference<-c("B. Causal (Coefficients)")
tmpdf2<-infdf[
  model=='inequality' &
    world=='yesdiscrimination' &
    main=="main"
]
tmpdf2$var<-c("Inequality")
tmpdf2$inference<-c("A. Descriptive")

tmpdf3<-infdf[
  oldvar%in%c("r2") & 
    world=="yesdiscrimination" & 
    model=="normal" &
    main=="main"
]
#put in terms of luck, not r2
tmpdf3$mu <- 1 - tmpdf3$mu; tmpdf3$baseline <- 1 - tmpdf3$baseline 
tmpdf3$var<-c("Luck")
tmpdf3$inference<-c("C. Causal (Luck)")

plotdf<-rbind.fill(
  tmpdf1,
  tmpdf2,
  tmpdf3
) %>% data.table

plotdf<-plotdf[
  ,
  .(
    mu=median(mu,na.rm=T)
  )
  ,
  by=c(
    'network_formation',
    'friendsize',
    'inference',
    'group',
    'var',
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
    'network_formation',
    'friendsize',
    'inference',
    'group',
    'var'
  )
]
plotdf$extra<-F

extradf<-rbind.fill(
  tmpdf1,
  tmpdf2,
  tmpdf3
) %>% data.table

extradf<-extradf[
  group=='Integrated' #arbitrary group
  ,
  .(
    mu=quantile(baseline,0.5),
    mu.min=quantile(baseline,0.025),
    mu.max=quantile(baseline,0.975)
  )
  ,
  by=c(
    'network_formation',
    'friendsize',
    'inference',
    'var'
  )
]
extradf$group <- 'Ground Truth'
extradf$extra <- T

#put these together
plotdf<-rbind.fill(
  plotdf,
  extradf
) %>% data.table

#sort levels/colors/etc.
tmplevels <- c(
  "A. Descriptive",
  "B. Causal (Coefficients)",
  "C. Causal (Luck)"
)
plotdf$inference<-factor(
  plotdf$inference,
  tmplevels
)

tmplevels<-c(
  "Ground Truth",
  "Integrated",
  "Neighborhood",
  "School",
  "Workplace"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels
)
tmpcolors<-c(
  'grey',
  '#e41a1c',
  '#377eb8',
  '#4daf4a',
  '#984ea3'
)
names(tmpcolors)<-tmplevels

tmpshapes<-c(16,4)
#tmpalphas<-c(1,0.9)
names(tmpshapes)<-c(F,T)#names(tmpalphas)<-c(F,T)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=var,
    y=mu,
    # ymin=mu.min,
    # ymax=mu.max,
    color=group#,
    #shape=extra#,
    #alpha=extra
  )
) +
  geom_point(
    size=2,
    position=position_dodge(0.4)
  ) +
  geom_linerange(
    aes(ymin=mu.min,ymax=mu.max),
    size=1.25,
    position=position_dodge(0.4)
  ) +
  scale_color_manual(
    name="",#,Social World",
    values=tmpcolors
  ) +
  # scale_alpha_manual(
  #   guide='none',
  #   values=tmpalphas
  # ) +
  scale_shape_manual(
    guide='none',
    values=tmpshapes
  ) +
  coord_flip() +
  xlab("") + 
  ylab("\nEstimate") +
  #facet_wrap(
  #  network_formation ~ friendsize,
  #) +
  facet_wrap(
    inference ~ .,
    ncol=1,
    scales='free'
  ) +
  theme_bw() +
  theme(
    legend.position='top',
    legend.direction='horizontal'
  )

tmpname<-"fig_inferences.pdf"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=4*1.5,
  height=6*1.5
)
output(plotdf,tmpname)


#########################################################
#########################################################

## RACIAL INEQUALITY
# people make all sorts of mistake about racial inequality

tmpdfs<-list()

#RACIAL GAP + DISCRIMINATION
tmpdf<-infdf[
  model%in%c('race','normal') & #normal gives direct effect of race
    var=='Race' &
    world=='yesdiscrimination' &
    main=="main"
]
tmpdf$mu_bias<-tmpdf$baseline<-NULL

#add ground truth
tmpdf2<-infdf[
  (model%in%c('race','normal') &
     var=='Race' & 
     world=='yesdiscrimination' &
     group=='Integrated') &
    main=="main"
]
tmpdf2$mu<-tmpdf2$baseline
tmpdf2$group<-'Ground Truth'
tmpdf2$mu_bias<-tmpdf2$baseline<-NULL

tmpdf<-rbind.fill(
  tmpdf,
  tmpdf2
) %>% data.table

tmpdf<-spread(
  tmpdf,
  model,
  mu
) %>% data.table

#what fraction of the racial gap is attributed to discrimination? 
tmpdf$pct_discrimination <- 100 * tmpdf$normal/tmpdf$race
tmpdf$racialgap <-
  tmpdf$race
tmpdf$discrimination <- 
  tmpdf$normal
#median pct_discimrination and racial gapin a given world
tmpdf <- tmpdf[
  ,
  .(
    pct_discrimination = median(pct_discrimination,na.rm=T),
    racialgap = median(racialgap,na.rm=T),
    discrimination = median(discrimination)
  )
  ,
  by=c(
    'network_formation',
    'group',
    'seed'
  )
]
tmpdf <- gather(
  tmpdf,
  stat,
  mu,
  pct_discrimination:discrimination
) %>% data.table

#store
tmpdfs[['racialgap+discrimination']]<-tmpdf

#calculate within-minority inequality
tmpdf<-infdf[
  model%in%c('betweenss','totalss') & 
    world=='yesdiscrimination'
]
tmpdf$mu_bias<-tmpdf$baseline<-NULL
#add ground truth
tmpdf2<-infdf[
  model%in%c('betweenss','totalss') & 
    world=='yesdiscrimination' &
    group=='Integrated'
]
tmpdf2$mu<-tmpdf2$baseline
tmpdf2$group<-'Ground Truth'
tmpdf2$mu_bias<-tmpdf2$baseline<-NULL

tmpdf<-rbind.fill(
  tmpdf,
  tmpdf2
) %>% data.table

tmpdf<-spread(
  tmpdf,
  model,
  mu
) %>% data.table
tmpdf<-tmpdf[
  ,
  .(
    betweenshare = 100 * median(betweenss/totalss)
  )
  ,
  by=c(
    'network_formation',
    'group',
    'seed'
  )
]

tmpdf <- gather(
  tmpdf,
  stat,
  mu,
  betweenshare
) %>% data.table
tmpdfs[['betweeninequality']]<-tmpdf

#put them together
tmpdf<-rbind.fill(
  tmpdfs
) %>% data.table

#95% CI for this quantity, across reps
plotdf<-tmpdf[
  ,
  .(
    mu=quantile(mu,0.5,na.rm=T),
    mu.min=quantile(mu,0.025,na.rm=T),
    mu.max=quantile(mu,0.975,na.rm=T)
  )
  ,
  by=c(
    'network_formation',
    'group',
    'stat'
  )
]

tmplevels<-c(
  "racialgap",
  "betweenshare",
  "discrimination",
  "pct_discrimination"
)
tmplabels<-c(
  "A. Total Racial Gap",
  "B. Between-Race Inequality as % of Total",
  "C. Present-Day Discrimination",
  "D. % of Gap Due to Present-Day Discrimination"
)
plotdf$stat <- factor(
  plotdf$stat,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "Ground Truth",
  "Integrated",
  "Neighborhood",
  "School",
  "Workplace"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels
)
tmpcolors<-c(
  'grey',
  '#e41a1c',
  '#377eb8',
  '#4daf4a',
  '#984ea3'
)
names(tmpcolors)<-tmplevels

g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=mu,
    #shape=network_formation,
    ymin=mu.min,
    ymax=mu.max,
    group=group,
    color=group
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
    guide='none',
    values=tmpcolors
  ) +
  coord_flip() +
  facet_wrap(
    ~ stat,
    scales='free_x',
    ncol=1
  ) +
  ylab("") +
  xlab("") +
  theme_bw() 

setwd(outputdir)
tmpname<-"fig_race.pdf"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=4.5*1.5,
  height=4.5*1.5
)
output(plotdf,tmpname)

#########################################################
#########################################################

##ROBUSTNESS TO DIFFERENT ASSUMPTIONS

#we want to show that the same conclusions hold
#whether we live in a direct effect dominated world
#whether we live in a race-first/class-first world

tmpdf<-infdf[
  (model%in%c('race','normal') & #normal gives direct effect of race
     var=='Race' &
     world=='yesdiscrimination')
]
tmpdf$mu_bias<-tmpdf$baseline<-NULL

tmpdf2<-infdf[
  (model%in%c('race','normal') &
     var=='Race' & 
     world=='yesdiscrimination' &
     group=='Integrated')
]
tmpdf2$mu<-tmpdf2$baseline
tmpdf2$group<-'Ground Truth'
tmpdf2$mu_bias<-tmpdf2$baseline<-NULL

tmpdf<-rbind.fill(
  tmpdf,
  tmpdf2
) %>% data.table

tmpdf<-spread(
  tmpdf,
  model,
  mu
) %>% data.table
#what fraction of the racial gap is attributed to discrimination? 
tmpdf$pct_discrimination <- 100 * tmpdf$normal/tmpdf$race
tmpdf$discrimination <- tmpdf$normal
#median pct_discimrination and racial gapin a given world
tmpdf <- tmpdf[
  ,
  .(
    pct_discrimination = median(pct_discrimination,na.rm=T),
    discrimination = median(discrimination),
    racialgap = median(race)
  )
  ,
  by=c(
    'main',
    'group',
    'seed'
  )
]
tmpdf <- gather(
  tmpdf,
  stat,
  mu,
  pct_discrimination:racialgap
) %>% data.table

plotdf<-tmpdf[
  ,
  .(
    mu=quantile(mu,0.5,na.rm=T),
    mu.min=quantile(mu,0.025,na.rm=T),
    mu.max=quantile(mu,0.975,na.rm=T)
  )
  ,
  by=c(
    'main',
    'group',
    'stat'
  )
]


tmplevels<-c(
  "racialgap",
  "discrimination",
  "pct_discrimination",
  "betweenshare"
)
tmplabels<-c(
  "A. Total Racial Gap",
  "B. Discrimination",
  "C. % of Gap Due to Discrimination",
  "D. Between-Race Inequality as % of Total"
)
plotdf$stat <- factor(
  plotdf$stat,
  tmplevels,
  tmplabels
)

tmplevels<-c( 
  'main',
  'robustness_directindirect',
  'robustness_classbigger',
  'robustness_racebigger'
)
tmplabels<-c(
  "Default",
  "If Direct > Indirect",
  "If Class > Race",
  "If Race > Class"
)
plotdf$main<-factor(
  plotdf$main,
  tmplevels,
  tmplabels
)

#same color scheme as above
tmpcolors<-tmpcolors

g.tmp<-ggplot(
  plotdf[group!='Ground Truth'],
  aes(
    x=group,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    group=group,
    color=group
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
  geom_hline(
    data=plotdf[group=='Ground Truth'],
    aes(
      yintercept=mu
    )
    ,
    linetype='dashed',
    color='black'
  ) +
  scale_color_manual(
    guide='none',
    values=tmpcolors
  ) +
  coord_flip() +
  facet_grid(
    main ~ stat,
    scales='free'
  ) +
  ylab("") +
  xlab("") +
  theme_bw() 

setwd(outputdir)
tmpname<-"fig_race_robustness.pdf"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=6*1.5,
  height=4.5*1.5
)
output(plotdf,tmpname)











