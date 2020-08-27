require(stringr)
require(rprojroot)

#time this process
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

#for timing
setwd(filesdir)
t0 <- proc.time()
write(t0[3],"t0.txt")

#run through
setwd(homedir)
tmp<-str_detect(dir(),"[0-9]{2}\\_") &
  !str_detect(dir(),"!!|00")
myfiles<-dir()[tmp]

#subset?
#myfiles<-myfiles

for(myfile in myfiles) {
  print("######")
  print("Running: ")
  print(myfile)
  homedir<-find_root(
    criterion=has_file('_inferentialspaces.Rproj')
  )
  setwd(homedir)
  source(myfile)
}

##########

#print final time
homedir<-find_root(
  criterion=has_file('_inferentialspaces.Rproj')
)
setwd(homedir)
source('dirs.R')
setwd(filesdir)
tf <- proc.time()
t0<-readLines("t0.txt") %>%
  as.numeric
print(
  paste(
    round((tf[3] - t0),3),
    "seconds"
  )
) #seconds
print(
  paste(
    round((tf[3] - t0) / 60,3),
    "minutes"
  )
) #minutes
print(
  paste(
    round((tf[3] - t0) / (60^2),3),
    "hours"
  )
) #hours
#delete to.txt
file.remove("t0.txt")
