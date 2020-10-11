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
  "Integrated",
  "Neighborhood",
  "Mostly Neighborhood",
  "School",
  "Mostly School",
  "Job",
  "Mostly Job"
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

# #(1)
# #SHOW THAT CAUSAL INFERENCES ABOUT RACE AND INCOME SUFFER
# #whether sampling on nhood, school or income, 
# #these inferences suffer
# 
# #(2) 
# #SHOW THAT CAUSAL INFERENCES ABOUT ABILITY SUFFER DIFFERENTLY
# #neighborhood samplers see ability just fine
# #school samplers only see a little of it
# #income samplers see none of it
# 
# plotdf<-biasdf[
#   model=="normal" & 
#     luck==1 & 
#     var%in%c('Class','Race','Ability')  &
#     group!="Random" &
#     world=="nodiscrimination"
#   ]
# 
# plotdf<-plotdf[
#   ,
#   .(
#     mu_bias=median(mu_bias,na.rm=T)
#   )
#   ,
#   by=c(
#     'group',
#     'var',
#     'seed'
#   )
#   ]
# plotdf<-plotdf[
#   ,
#   .(
#     mu=quantile(mu_bias,0.5),
#     mu.min=quantile(mu_bias,0.025),
#     mu.max=quantile(mu_bias,0.975)
#   )
#   ,
#   by=c(
#     'group',
#     'var'
#   )
#   ]
# 
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=var,
#     y=mu,
#     ymin=mu.min,
#     ymax=mu.max,
#     group=group,
#     color=group
#   )
# ) +
#   geom_point(
#     position=position_dodge(0.4)
#   ) +
#   geom_linerange(
#     position=position_dodge(0.4)
#   ) +
#   scale_color_discrete(
#     name="Social World"
#   ) +
#   coord_flip() +
#   theme_bw() +
#   ylab("\nBias") +
#   xlab("") 
# 
# # tmpname<-"fig_inferences_bias.pdf"
# # gs.list[[tmpname]]<-list(
# #   graph=g.tmp,
# #   filename=tmpname,
# #   width=4*1.5,
# #   height=3*1.5
# # )
# # output(plotdf,tmpname)
# 
# 
# #(3) PLOT ACTUAL COEFFICIENTS
# 
# plotdf<-biasdf[
#   model=="normal" & 
#     var%in%c('Class','Race','Ability') &
#     world=='nodiscrimination'
#   ]
# 
# plotdf<-plotdf[
#   ,
#   .(
#     mu=median(mu,na.rm=T)
#   )
#   ,
#   by=c(
#     'group',
#     'var',
#     'seed'
#   )
#   ]
# plotdf<-plotdf[
#   ,
#   .(
#     mu=quantile(mu,0.5),
#     mu.min=quantile(mu,0.025),
#     mu.max=quantile(mu,0.975)
#   )
#   ,
#   by=c(
#     'group',
#     'var'
#   )
#   ]
# 
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=var,
#     y=mu,
#     ymin=mu.min,
#     ymax=mu.max,
#     group=group,
#     color=group
#   )
# ) +
#   geom_point(
#     size=2,
#     position=position_dodge(0.4)
#   ) +
#   geom_linerange(
#     size=1.25,
#     position=position_dodge(0.4)
#   ) +
#   geom_line(
#     alpha=0.25,
#     size=2,
#     position=position_dodge(0.4)
#   ) +
#   scale_color_discrete(
#     name="Social World"
#   ) +
#   coord_flip() +
#   theme_bw() +
#   ylab("\nEstimated Effect on Earnings") +
#   xlab("") 

# tmpname<-"fig_inferences.pdf"
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=4*1.5,
#   height=3*1.5
# )
# output(plotdf,tmpname)

#########################################################
#########################################################

#(3) PLOT LUCK; HOW MUCH CAN'T THE ESTIMATES EXPLAIN?

# plotdf <- biasdf[
#   oldvar%in%c("r2") &
#     world=='nodiscrimination' &
#     model=="normal",
#   .(
#     mu = 1 - median(mu)
#   )
#   ,
#   by=c(
#     'group',
#     'oldvar',
#     'seed'
#   )
#   ]
# plotdf<-plotdf[
#   ,
#   .(
#     mu=quantile(mu,0.5),
#     mu.min=quantile(mu,0.025),
#     mu.max=quantile(mu,0.975)
#   )
#   ,
#   by=c(
#     'group',
#     'oldvar'
#   )
#   ]
# 
# g.tmp <- ggplot(
#   plotdf,
#   aes(
#     x=group,
#     y=mu,
#     ymin=mu.min,
#     ymax=mu.max,
#     color=group
#   )
# ) +
#   geom_point(
#     size=2,
#     position=position_dodge(0.4)
#   ) +
#   geom_linerange(
#     size=1.25,
#     position=position_dodge(0.4)
#   ) +
#   scale_color_discrete(
#     name="Social World"
#   ) +
#   coord_flip() +
#   xlab("") + 
#   ylab("\nLuck (i.e. 1 - R^2)") +
#   theme_bw()
# 
# tmpname<-"fig_luck.pdf"
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=4*1.5,
#   height=3*1.5
# )
# output(plotdf,tmpname)

#########################################################
#########################################################

#(4) INFERENCES ABOUT INEQUALITY

# plotdf<-biasdf[
#   model=='inequality' &
#     world=='nodiscrimination'
#   ]
# 
# plotdf<-plotdf[
#   ,
#   .(
#     mu=median(mu,na.rm=T)
#   )
#   ,
#   by=c(
#     'group',
#     'var',
#     'seed'
#   )
#   ]
# plotdf<-plotdf[
#   ,
#   .(
#     mu=quantile(mu,0.5),
#     mu.min=quantile(mu,0.025),
#     mu.max=quantile(mu,0.975)
#   )
#   ,
#   by=c(
#     'group',
#     'var'
#   )
#   ]
# 
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=group,
#     y=mu,
#     ymin=mu.min,
#     ymax=mu.max,
#     color=group
#   )
# ) +
#   geom_point(
#     size=2,
#     position=position_dodge(0.4)
#   ) +
#   geom_linerange(
#     size=1.25,
#     position=position_dodge(0.4)
#   ) +
#   scale_color_discrete(
#     guide=F
#   ) +
#   coord_flip() +
#   xlab("") + 
#   ylab("\nObserved Inequality in Final Income") +
#   theme_bw()

# tmpname<-"fig_inequality.pdf"
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=4*1.5,
#   height=3*1.5
# )
# output(plotdf,tmpname)

#########################################################
#########################################################

#PLOT ALL INFERENCES TOGETHER
#(replaces all figs above)

tmpdf1<-biasdf[
  model=="normal" & 
    var%in%c('Class','Race','Ability') &
    world=='nodiscrimination'
  ]
tmpdf1$inference<-c("Causal (Coefficients)")
tmpdf2<-biasdf[
  model=='inequality' &
    world=='nodiscrimination'
  ]
tmpdf2$var<-c("Inequality")
tmpdf2$inference<-c("Descriptive")

tmpdf3<-biasdf[
    oldvar%in%c("r2") & 
      world=="nodiscrimination" & 
      model=="normal"
]
tmpdf3$mu <- 1 - tmpdf3$mu
tmpdf3$var<-c("Luck")
tmpdf3$inference<-c("Causal (Luck)")

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
    'inference',
    'group',
    'var'
  )
  ]

tmplevels <- c(
  "Descriptive",
  "Causal (Coefficients)",
  "Causal (Luck)"
)


g.tmp <- ggplot(
  plotdf,
  aes(
    x=var,
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
  ylab("\nEstimate") +
  # facet_wrap(
  #   inference ~ .,
  #   ncol=1,
  #   scales='free'
  # ) +
  theme_bw()
g.tmp <- g.tmp + ggforce::facet_col(
  vars(inference), 
  scales='free',
  space='free'
)

tmpname<-"fig_inferences.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=4*1.5,
  height=5*1.5
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
#(3) constrianed estimators will 

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
tmpdf$pct_discrimination <- 100 * tmpdf$discrimination/tmpdf$race
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
  model%in%c('betweenss','totalss') & 
    world=='yesdiscrimination'
]
tmpdf$mu_bias<-NULL
tmpdf<-spread(
  tmpdf,
  model,
  mu
)
tmpdf<-tmpdf[
  ,
  .(
    betweenshare = 100 * median(betweenss/totalss)
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
    'group',
    'stat'
  )
  ]

tmplevels<-c(
  "racialgap",
  "betweenshare",
  "pct_discrimination"
)
tmplabels<-c(
  "Total Racial Gap",
  "Between-Race Inequality as % of Total",
  "% of Gap Due to Present-Day Discrimination"
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
    guide=F
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

tmpname<-"fig_race.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=4.5*1.5,
  height=4.5*1.5
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

