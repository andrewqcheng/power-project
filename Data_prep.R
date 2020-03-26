# Data preparation script for the "Power and Accommodation" project (Andrew Cheng and Alice Shen)
# This script reads files from the folder 'meas', which contains one file per subject.
# Each file contians f0 values for both speakers: the confederate and the participant.
# The raw data are organized into smaller dataframes for the purposes of statistical
# analysis and visualization (see "Analysis_plots.R").
# Special thanks to Eric Wilbanks for help with this script.

library(tidyr)
library(dplyr)

##########################################################################################
# PART 1: READ IN RAW DATA
setwd("C:\\Users\\andrew\\Box Sync\\Power and Accommodation\\meas") # set directory
# loop over meas files, read in data, and append to `full` df
l = list() # to add data frames to
names <- dir(getwd(),pattern=".meas")
for(i in 1:length(names)){
  curr <- read.table(names[i],header=T,sep="\t",quote="")
  # parse file name to get code and condition
  split = unlist(strsplit(gsub(".multi.*meas","",names[i]),'-')) # split by `-`
  code = split[2] # speaker # code
  condition = split[3] # power/role condition code (i.e., FN, LS)
  # Determine speakers and phases
  if(length(split)==4){
    speaker = "subject"
    phase = split[4]
  } else if (length(split)==5){
    # Separate out Azin's prepost from interactions
    if(split[4]=="Azin"){
      speaker = "confederate"
      phase = split[5]
    } else if (split[4]=="interaction"){
      speaker = split[5]
      phase = split[4]
    }
  }
  # add parsed info to df
  curr$code <- rep(code,nrow(curr))
  curr$condition = rep(condition,nrow(curr))
  curr$speaker = rep(speaker,nrow(curr))
  curr$phase = rep(phase,nrow(curr))
  l[[i]] <- curr
}

full <- do.call(rbind,l) # rbind all data frames in `l`
full$code <- as.factor(as.character(full$code))
full$condition <- as.factor(full$condition)
full$speaker <- as.factor(full$speaker)
full$phase <- as.factor(full$phase)
full <- full[!(full$code %in% c('022','027')),]
full$code <- as.factor(as.character(full$code))

setwd("C:\\Users\\andrew\\Box Sync\\Power and Accommodation\\analysis") # set directory
demo <- read.csv(file='demo_20180726.csv',header=T) # read in demographics file
demo <- demo[,c(1:6,30:34)] # subset the demographics dataframe
demo <- demo %>% rename(code = Subject.., condition = Condition)
demo$code <- sprintf("%03d",demo$code) # convert code to constant width of 3 digits
demo$code <- as.factor(demo$code)

df <- merge(full,demo, by=c('code','condition')) # merge raw df and demographics

setwd("C:\\Users\\andrew\\Box Sync\\Power and Accommodation\\SC_analysis") # set directory
write.csv(df, file="raw_20200319.csv", row.names=F)

df <- df[!df$f0==0,] # Remove all zeroes (~64000 rows)
outliers <- boxplot(df$f0)$out # Get outliers (~3500 rows)
df <- df[-which(df$f0 %in% outliers),] # Remove all outliers
df$Bark <- ((26.81*df$f0)/(1960+df$f0))-0.53 # Convert Hertz to Bark

write.csv(df, file="full_20200319.csv", row.names = F)

##########################################################################################
# PART 2: SUMMARIZED DATAFRAMES
df <- read.csv('full_20200319.csv', header = T, row.names = F)

# Create a smaller dataframe that summarizes the data (mean f0) by phase, speaker, Gender,
# and personal power manipulation
summary.personal <- df %>%
  separate(col=condition,into=c("personal","interpersonal"),sep=1) %>% # split condition into 2 cols
  group_by(personal,speaker,phase,Gender) %>%
  summarise(mean=mean(f0),median=median(f0),sd=sd(f0))
summary.personal$phase <- factor(summary.personal$phase, levels=c("pre","interaction","post"))
write.csv(summary.personal, file="summary_personal.csv", row.names=F)

# Create a smaller dataframe that summarizes the data (mean f0) by phase, speaker, Gender,
# and interpersonal power manipulation
summary.interpersonal <- df %>%
  separate(col=condition,into=c("personal","interpersonal"),sep=1) %>% # split condition into 2 cols
  group_by(interpersonal,speaker,phase,Gender) %>%
  summarise(mean=mean(f0),median=median(f0),sd=sd(f0))
summary.interpersonal$phase <- factor(summary.interpersonal$phase, levels=c("pre","interaction","post"))
write.csv(summary.interpersonal, file="summary_interpersonal.csv", row.names=F)

# Create a smaller dataframe that calculates convergence measurements and across-task changes
# for both speakers of every subject trial
mean_pitch <- df %>%
  group_by(code,speaker,phase) %>%
  dplyr::summarise(mean=mean(Bark)) %>%
  unite(col=key,speaker,phase,sep="_") %>%
  group_by(code) %>%
  spread(key=key,value=mean) %>%
  mutate(convergence = # change in interlocutors' pitch differences pre to post
           abs(confederate_pre-subject_pre) - abs(confederate_post-subject_post),
         short_convergence = # change in interlocutors' pitch differences pre to interview
           abs(confederate_pre-subject_pre) - abs(confederate_interaction-subject_interaction),
         confederate_change = # change in confederate's pitch pre to post
           confederate_pre-confederate_post,
         confederate_shortchange = # change in confederate's pitch pre to interview
           confederate_pre-confederate_interaction,
         subject_change = # change in participant's pitch pre to post
           subject_pre-subject_post,
         subject_shortchange = # change in participant's pitch pre to interview
           subject_pre-subject_interaction)

# positive = convergence, 0 = no change, negative = divergence
mean_pitch$code <- sprintf("%03d",mean_pitch$code) # convert code to constant width of 3 digits
mean_pitch$code <- as.factor(mean_pitch$code)
mean_pitch <- merge(mean_pitch,demo, by="code")
mean_pitch <- mean_pitch %>% separate(col=condition,into=c("personal","interpersonal"),sep=1)

write.csv(mean_pitch, file="mean_pitch.csv", row.names=F)

# Convert wide to long
change_comparison <- mean_pitch %>%
  gather(key="key",value="diff",confederate_change,subject_change,confederate_shortchange,subject_shortchange) %>%
  separate(col=key,into=c("speaker","comparison"),sep="_")

write.csv(change_comparison, file="change_comparison.csv", row.names=F)

##########################################################################################
# PART 3: BINNING THE DATA IN 30-SEC INTERVALS

full <- df
full$speak_uniq <- do.call(paste0, c(full[c("code","condition","speaker")]))
full$speak_uniq <- as.factor(full$speak_uniq)

bin_size = 30
l <- list()

for (i in 1:length(levels(full$speak_uniq))){
  speak_uniq <- levels(full$speak_uniq)[i]
  code <- levels(as.factor(as.character(full[full$speak_uniq == speak_uniq,]$code)))[1]
  condition <- levels(as.factor(as.character(full[full$speak_uniq == speak_uniq,]$condition)))[1]
  speaker <- levels(as.factor(as.character(full[full$speak_uniq == speak_uniq,]$speaker)))[1]
  tmp <- full[full$speak_uniq == speak_uniq,]
  tmp$group <- cut(tmp$t1, seq(from = min(tmp$t1), to = max(tmp$t2), by = bin_size), include.lowest = T)
  tmp$group_num <- as.numeric(tmp$group)
  tmp_mean <- aggregate(f0 ~ group + group_num, data = tmp, mean)
  tmp_mean$speak_uniq <- as.factor(rep(speak_uniq,nrow(tmp_mean)))
  tmp_mean$code <- as.factor(rep(code,nrow(tmp_mean)))
  tmp_mean$condition <- as.factor(rep(condition,nrow(tmp_mean)))
  tmp_mean$speaker <- as.factor(rep(speaker,nrow(tmp_mean)))
  l[[i]] <- tmp_mean
}

mean_f0_bin <- do.call(rbind,l)
mean_f0_bin$code <- sprintf("%03d", mean_f0_bin$code) # convert code to constant width of 3 digits
mean_f0_bin$condition <- NULL
merge <- merge(mean_f0_bin,demo, by=c("code"))
bin <- merge %>%
  separate(col=condition,into=c("personal","interpersonal"),sep=1)
bin$Bark <- ((26.81*bin$f0)/(1960+bin$f0))-0.53 # Convert Hertz to Bark

setwd("C:\\Users\\andrew\\Box Sync\\Power and Accommodation\\SC_analysis") # set directory
write.csv(bin, file = "mean_f0_bin_20200321.csv", row.names = F)