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

#retrieve inferences, 
inferencesdf<-lapply(
  fulloutput,
  function(x) x$inferencesdf
) %>% 
  rbind.fill %>%
  data.table
inferencesdf<-merge(
  loopdf,
  inferencesdf
) %>% data.table

#use random as baseline
inferencesdf$i<-
inferencesdf$se<-
  inferencesdf$tval<-
  inferencesdf$pval<-
  # inferencesdf$mu.min<-
  # inferencesdf$mu.max<-
  # inferencesdf$pval.class<-
  NULL
biasdf<-spread(
  inferencesdf,
  network,
  mu
)
biasdf$baseline<-biasdf$random
gathcols<-unique(loopdf$network)
#gathcols<-gathcols[gathcols!="random"]
biasdf<-gather_(
  biasdf,
  "group",
  "mu",
  gathcols
) %>% data.table
biasdf$mu_bias<- biasdf$mu - biasdf$baseline
biasdf$baseline<-NULL

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
biasdf$group<-factor(
  biasdf$group,
  tmplevels,
  tmplabels
)

#fix vars
biasdf$var %>% unique
biasdf$oldvar<-biasdf$var
tmplevels<-c(
  '(Intercept)',
  'race_i',
  'income_i',
  'ability_i',
  ####
  "adjr2",
  "r2",
  "nhood_raw",
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
  "Neighborhood",
  "Inequality"
)
biasdf$var<-factor(
  biasdf$var,
  tmplevels,
  tmplabels
)

#########################################################
#########################################################

#(1)
#SHOW THAT CAUSAL INFERENCES ABOUT RACE AND INCOME SUFFER
#whether sampling on nhood, school or income, 
#these inferences suffer

#(2) 
#SHOW THAT CAUSAL INFERENCES ABOUT ABILITY SUFFER DIFFERENTLY
#neighborhood samplers see ability just fine
#school samplers only see a little of it
#income samplers see none of it

plotdf<-biasdf[
  model=="normal" & 
    luck==1 & 
    var%in%c('Class','Race','Ability')  &
    group!="Random" &
    world=="nodiscrimination"
  ]

plotdf<-plotdf[
  ,
  .(
    mu_bias=median(mu_bias,na.rm=T)
  )
  ,
  by=c(
    'group',
    'var',
    'seed'
  )
  ]
plotdf<-plotdf[
  ,
  .(
    mu=quantile(mu_bias,0.5),
    mu.min=quantile(mu_bias,0.025),
    mu.max=quantile(mu_bias,0.975)
  )
  ,
  by=c(
    'group',
    'var'
  )
  ]

g.tmp<-ggplot(
  plotdf,
  aes(
    x=var,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    group=group,
    color=group
  )
) +
  geom_point(
    position=position_dodge(0.4)
  ) +
  geom_linerange(
    position=position_dodge(0.4)
  ) +
  scale_color_discrete(
    name="Social World"
  ) +
  coord_flip() +
  theme_bw() +
  ylab("\nBias") +
  xlab("") 

tmpname<-"fig_inferences_bias.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=4*1.5,
  height=3*1.5
)
output(plotdf,tmpname)


#(3) PLOT ACTUAL COEFFICIENTS

plotdf<-biasdf[
  model=="normal" & 
    var%in%c('Class','Race','Ability') &
    world=='nodiscrimination'
  ]

plotdf<-plotdf[
  ,
  .(
    mu=median(mu,na.rm=T)
  )
  ,
  by=c(
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
    'group',
    'var'
  )
  ]

g.tmp<-ggplot(
  plotdf,
  aes(
    x=var,
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
  geom_line(
    alpha=0.25,
    size=2,
    position=position_dodge(0.4)
  ) +
  scale_color_discrete(
    name="Social World"
  ) +
  coord_flip() +
  theme_bw() +
  ylab("\nEstimated Effect on Earnings") +
  xlab("") 

tmpname<-"fig_inferences.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=4*1.5,
  height=3*1.5
)
output(plotdf,tmpname)

#########################################################
#########################################################

#(3) PLOT LUCK; HOW MUCH CAN'T THE ESTIMATES EXPLAIN?

plotdf <- biasdf[
  oldvar%in%c("r2") &
    world=='nodiscrimination' &
    model=="normal",
  .(
    mu = 1 - median(mu)
  )
  ,
  by=c(
    'group',
    'oldvar',
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
    'group',
    'oldvar'
  )
  ]

g.tmp <- ggplot(
  plotdf,
  aes(
    x=group,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
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
  scale_color_discrete(
    name="Social World"
  ) +
  coord_flip() +
  xlab("") + 
  ylab("\nLuck (i.e. 1 - R^2)") +
  theme_bw()

tmpname<-"fig_luck.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=4*1.5,
  height=3*1.5
)
output(plotdf,tmpname)

#########################################################
#########################################################

#1-3 PLOT COEFS/LUCK TOGETHER AS EXPLAINED

plotdf <- biasdf[
  model=='normal_relimp' &
    world=='nodiscrimination'
  ,
  .(
    mu = median(mu,na.rm=T)
  )
  ,
  by=c(
    'group',
    'oldvar',
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
    'group',
    'oldvar'
  )
  ]

ggplot(
  plotdf,
  aes(
    x=oldvar,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    color=oldvar
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
  facet_wrap(
    ~ group 
  ) +
  coord_flip() +
  xlab("") + 
  ylab("\nLuck (i.e. 1 - R^2)") +
  theme_bw()





#########################################################
#########################################################

#DEPRECATED; 
#works, but we decided this is not all that 
#useful a way of visualizing the results

# #MAKE THE FOREGOING VIVID BY ILLUSTRATING 
# #HOW AVG/MEDIAN PERSON SEES THE INCOME, RACE AND ABILITY GRADIENTS
# 
# tmpdf<-inferencesdf[
#   model=='normal' &
#     world=='nodiscrimination'
#   ,
#   .(
#     intercept=median(mu[var=="(Intercept)"],na.rm=T),
#     income_i=median(mu[var=="income_i"],na.rm=T),
#     ability_i=median(mu[var=="ability_i"],na.rm=T),
#     race_i=median(mu[var=="race_i"],na.rm=T)
#   ),
#   by=c(
#     'network',
#     'seed'
#   )
#   ]
# tmpdf$modnum<-1:nrow(tmpdf)
# 
# predictdf<-expand.grid(
#   intercept=1,
#   race_i=c(0,1),
#   income_i=seq(-3,3,by=0.05),
#   ability_i=c(-3,3,by=0.05)
# )
# predictdf$prednum<-1:nrow(predictdf)
# 
# #get predictionfs
# tmpvars<-c(
#   "intercept",
#   "income_i",
#   "ability_i",
#   "race_i"
# )
# yhat<-as.matrix(tmpdf[,tmpvars,with=F]) %*% 
#   as.matrix(t(predictdf[,tmpvars]))
# yhatdf<-data.frame(yhat)
# yhatdf$modnum<-1:nrow(yhatdf)
# gathcols<-names(yhatdf)[str_detect(names(yhatdf),"X")]
# yhatdf<-gather_(
#   yhatdf,
#   "prednum",
#   "yhat",
#   gathcols
# )
# yhatdf$prednum<-
#   str_replace(yhatdf$prednum,"X","") %>% 
#   as.numeric
# 
# #merge relevant info
# yhatdf<-merge(
#   yhatdf,
#   tmpdf[,c('modnum','network','seed')],
#   by='modnum'
# )
# yhatdf$modnum<-NULL
# yhatdf<-merge(
#   yhatdf,
#   predictdf[,c('prednum','race_i','income_i',"ability_i")],
#   by='prednum'
# )
# 
# #get intervals
# yhatdf<-data.table(yhatdf)
# 
# #fix groups
# tmplevels<-c(
#   "random",
#   "nhood",
#   "nhood95",
#   "school",
#   "school95",
#   "earnings",
#   "earnings95"
# )
# tmplabels<-c(
#   "No Segregation",
#   "At Neighborhood",
#   "Most At Neighborhood",
#   "At School",
#   "Most At School",
#   "By Income",
#   "Most By Income"
# )
# 
# yhatdf$group<-factor(
#   yhatdf$network,
#   tmplevels,
#   tmplabels
# )
# yhatdf$network<-NULL
# 
# #ILLUSTRATE
# 
# tmpvars<-c(
#   "income_i",
#   "ability_i",
#   "race_i"
# )
# plotdf<-lapply(tmpvars,function(v) {
#   tmpdf<-yhatdf[
#     ,
#     .(
#       yhat=quantile(yhat,0.5,na.rm=T),
#       yhat.min=quantile(yhat,0.025,na.rm=T),
#       yhat.max=quantile(yhat,0.975,na.rm=T)
#     )
#     ,
#     by=c(
#       'group',
#       v
#     )
#     ]
#   tmpdf$x <- tmpdf[[v]]; tmpdf[[v]]<-NULL
#   tmpdf$facet <- v
#   tmpdf
# }) %>% rbind.fill
# 
# 
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=x,
#     y=yhat,
#     group=group,
#     color=group,
#     fill=group
#   )
# ) +
#   geom_line(
#     size=1
#   ) +
#   xlab("\nAbillity, Class or Race") +
#   ylab("Predicted Earnings (SDs Above Avg.)\n") +
#   facet_wrap(
#     ~ facet,
#     scales='free',
#     ncol=1
#   ) + 
#   theme_bw()
# 
# tmpname<-"fig_predictions.pdf"
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=5.1,
#   height=7.6
# )
# output(plotdf,tmpname)

#########################################################
#########################################################

#(4) INFERENCES ABOUT INEQUALITY

plotdf<-biasdf[
  model=='inequality' &
    world=='nodiscrimination'
  ]

plotdf<-plotdf[
  ,
  .(
    mu=median(mu,na.rm=T)
  )
  ,
  by=c(
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
    'group',
    'var'
  )
  ]

g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
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
  scale_color_discrete(
    name="Social World"
  ) +
  coord_flip() +
  xlab("") + 
  ylab("\nObserved Inequality (Variance) in Final Income") +
  theme_bw()

tmpname<-"fig_inequality.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=4*1.5,
  height=3*1.5
)
output(plotdf,tmpname)

#########################################################
#########################################################

#(5) #RACIAL INEQUALITY
#people make all sorts of mistake about racial inequality

tmpdfs<-list()
#(1) constrianed estimators will underestimate the overall racial gap
#(above we show conditional racial gap; here we want to show overall)
#(2) constrained estimators will attribute a larger share 
#of the racial gap they see to discrimination
#(3) constrianed estimators will underestimate the level of intra-minotiry inequality

#RACIAL GAP + DISCRIMINATION
tmpdf<-biasdf[
  (model%in%c('race','discrimination') &
    var=='Race' &
    world=='yesdiscrimination') 
  ]
tmpdf$mu_bias<-NULL
tmpdf<-spread(
  tmpdf,
  model,
  mu
)

#what fraction of the racial gap is attributed to discrimination? 
tmpdf$pct_discrimination <- 
100 - 100 * (tmpdf$race - tmpdf$discrimination)/tmpdf$race
tmpdf$racialgap <-
  tmpdf$race
#median pct_discimrination and racial gapin a given world
tmpdf <- tmpdf[
  ,
  .(
    pct_discrimination = median(pct_discrimination,na.rm=T),
    racialgap = median(racialgap,na.rm=T)
  )
  ,
  by=c(
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

#store
tmpdfs[['racialgap+discrimination']]<-tmpdf

#calculate within-minority inequality
tmpdf<-biasdf[
  model=='mininequality' & 
    world=='yesdiscrimination'
]
tmpdf<-tmpdf[
  ,
  .(
    withininequality = median(mu)
  )
  ,
  by=c(
    'group',
    'seed'
  )
]

tmpdf <- gather(
  tmpdf,
  stat,
  mu,
  withininequality
) %>% data.table
tmpdfs[['withininequality']]<-tmpdf

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
    'group',
    'stat'
  )
  ]

tmplevels<-c(
  "withininequality",
  "racialgap",
  "pct_discrimination"
)
tmplabels<-c(
  "Intraracial Inequality",
  "Racial Gap (Unadjusted)",
  "% of Gap Due to Discrimination"
)
plotdf$stat <- factor(
  plotdf$stat,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
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
  scale_color_discrete(
    name="Social World"
  ) +
  coord_flip() +
  facet_wrap(
    ~ stat,
    scales='free_x'
  ) +
  ylab("") +
  xlab("") +
  theme_bw() 

tmpname<-"fig_race.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=4*1.5,
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

