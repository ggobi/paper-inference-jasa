# The following function creates indexes based on 6 data selection criteria
get_sceering_index <- function(dat){
  indx1 <- dat$id==dat$id
  indx2 <- !is.na(dat$gender)
  d34 <- ddply(dat,.(id), summarise, percent_correct=mean(response)*100)
  included_id3 <- d34$id[ d34$percent_correct > 0]
  indx3 <- dat$id %in% included_id3
  included_id4 <- d34$id[ d34$percent_correct >= 20 ]
  indx4 <- dat$id %in% included_id4
  d56 <- ddply(subset(dat, p_value < 0.0002),.(id),summarise,
               easy_cnt = length(response),
               percent_correct = mean(response)*100,
               include_criteria_6 = response[1],
               exclude_lineup_criteria_6 = paste(id[1],"_",pic_id[1],sep=""),
               exclude_lineup_p_value = p_value[1]
               )
  included_id5 <- d56$id[ d56$percent_correct > 49]
  indx5 <- dat$id %in% included_id5
  included_id6 <- d56$id[d56$include_criteria_6]
  indx6 <- dat$id %in% included_id6
  indx6[paste(dat$id,"_",dat$pic_id,sep="") %in% d56$exclude_lineup_criteria_6] <- FALSE
  return(data.frame(indx1,indx2,indx3,indx4,indx5,indx6))
}

# the following function returns the cleaned data based on screening criteria 6
clean_data <- function(raw_dat){
  easy_dat <- subset(raw_dat, p_value < 0.0002)
  if (raw_dat$experiment[1]=='turk3') easy_dat <- subset(raw_dat, difficulty==0)
  d <- ddply(subset(easy_dat),.(id),summarise,
              easy_cnt = length(response),
              percent_correct = mean(response)*100,
              include_id = response[1],
              excluded_lineup = paste(id[1],"_",pic_id[1],sep=""),
              excluded_lineup_p_value = round(p_value[1],4)
              )
  included_id <- d$id[d$include_id]
  indx <- dat$id %in% included_id
  excluded_lineup <- paste(dat$id,"_",dat$pic_id,sep="") %in% d$excluded_lineup
  indx[excluded_lineup] <- FALSE
  cleaned_dat <- subset(raw_dat,indx)
  return(cleaned_dat)
}








