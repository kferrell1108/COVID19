setwd("U:/Data Science/COVID19")

read.data <- function()
{
#  cty_url <- "https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv"  # data archive
#  sta_url <- "https://github.com/nytimes/covid-19-data/raw/master/us-states.csv"    # data archive
  cty_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"  # data archive
  sta_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"    # data archive
  
  covid_c <- read.csv(cty_url, stringsAsFactors = FALSE)                            # pull the most current data
  covid_s <- read.csv(sta_url, stringsAsFactors = FALSE)                            # pull the most current data
  covid_s$county <- "State_of"                            # dummy "county" for state level data
  covid_s2 <- covid_s[ , c(1, 6, 2, 3, 4, 5) ]            # reorder state columns to match county data
  covid_cs <- rbind(covid_c, covid_s2)
  return (covid_cs)
}


plot.cases <- function (state, county = "STATE_OF", output.to = "PDF")
{
#  state <- "AMERICAN SAMOA"
#  county <- "STATE_OF"
  x <- covid_cs[toupper(covid_cs$state)==toupper(state) & toupper(covid_cs$county)==toupper(county),]
  if (output.to == "PDF")
      pdf(trimws(paste(county, "_", state, ".pdf",sep = '')))
  if (output.to == "JPEG")
      jpeg(trimws(paste(county, "_", state, ".jpeg",sep = '')), width=800)
  
  if (length(x[,1]) > 0 & sum(x$cases > 0))
  {
    if (length(x$cases) >= 15)
      df <- data.frame(date = as.Date(x$date), 
                       cases = x$cases - c(rep(0, 14), x$cases[seq(1,length(x$cases)-14)]))
    if (length(x$cases) < 15)
      df <- data.frame(date = as.Date(x$date), cases = x$cases)
    df <- df[seq(min(which(df$cases > 0)),length(df[,1])),]
    top.y <- max(df$cases)
    main = paste("Current Cases -", county, "County",state)
    if (county == "STATE_OF")
      main = paste("Current Cases -", state)
    plot(df, 
         main=main,
         sub="14 day duration method, all confirmed cases", xlab="Date", 
         ylab="All Confirmed Cases",
         type="b",
         ylim=c(0,top.y * 1.1))
    
    text(df$date, df$cases, df$cases, adj=c(-0.5, 0), srt=90)
    abline(v=seq(min(df$date), max(df$date), by="7 day"), col="gray",lty=5)
    log.cases <- log(top.y, 10)-0.3
    log.cases <- ifelse(log.cases < 0, 0, log.cases)
    abline(h=seq(0, top.y * 1.1, 10^floor(log.cases)), col="gray",lty=5)
  } else {
    plot(c(0,10), c(0,10), col="white")
    text(5,5,paste("NO DATA FOR", county, "COUNTY,", state))
  }
    
  if (output.to == "PDF" | output.to == "JPEG" )
      dev.off()
}

calc.slope <- function(x,y)
{
    return(lm(y~x)$coefficients[2])
}

covid_cs <- read.data()
cs.county.state <- read.csv("CSCounties.csv", stringsAsFactors = FALSE, header=TRUE)

# Load County/State level population data
pop.data <- read.csv("co-est2019-alldata.csv", stringsAsFactors = FALSE)
pop.data.state <- pop.data[pop.data$SUMLEV==40, c("STNAME", "POPESTIMATE2019")]
pop.data <- pop.data[pop.data$SUMLEV==50, c("STNAME","CTYNAME", "POPESTIMATE2019")]
pop.data$state <- toupper(pop.data$STNAME)
pop.data$county <- toupper(pop.data$CTYNAME)
pop.data.state$state <- toupper(pop.data.state$STNAME)

## *******************STATE LOAD ****************
# Load State detail
electoral.data <- read.table("ElectionInfo.txt", header = TRUE, sep='\t', stringsAsFactors = FALSE)
electoral.data$state <- toupper(electoral.data$state)
state.detail <- covid_cs[toupper(covid_cs$county) == "STATE_OF",]
state.detail$date <- as.Date(state.detail$date)
state.detail <- state.detail[order(state.detail$state, state.detail$date),]

state.detail$state <- toupper(state.detail$state)

state.detail$new.cases = state.detail$cases
state.detail$current.cases = state.detail$cases
state.detail$percent.change.prior.day <- 0
state.detail$day <- 1
len <- length(state.detail[,1])
start.row <- len

for (i in rev(seq(2,len)))
{
  state.detail$day[i] <- state.detail$date[start.row] - state.detail$date[i] + 1
  if (state.detail$state[i-1] == state.detail$state[i]  )
  {
    state.detail$new.cases[i] <- state.detail$cases[i] - state.detail$cases[i-1] 
    state.detail$percent.change.prior.day[i] <- state.detail$new.cases[i]/state.detail$cases[i-1]
  } else start.row <- i-1
  
  if (i > 14)
    if (state.detail$state[i-14] == state.detail$state[i]  )
      state.detail$current.cases[i] =  state.detail$cases[i] - state.detail$cases[i-14]
}
state.detail$day[1] <- state.detail$date[start.row] - state.detail$date[1] + 1 


state.detail <- merge(state.detail, pop.data.state, 
                       by=c("state"), all.x=TRUE)
colnames(state.detail)[12] <- "Population"
state.detail <- merge(state.detail, electoral.data[,c("state", "category","color")], by="state")

# State Rankings
state.detail$new.cases.per.100k <- 100000 * state.detail$new.cases / state.detail$Population 
state.detail <- state.detail[order(state.detail$date, state.detail$new.cases.per.100k, decreasing=TRUE),]
state.detail$rank <- 1
last.rank <- 0
last.day <- state.detail$date[1]
for (i in seq(1, length(state.detail[,1])))
  
{
  if (state.detail$date[i] != last.day)
  {
    last.day <- state.detail$date[i]
    last.rank <- 0
  } 
  last.rank <- last.rank + 1
  
  state.detail$rank[i] <- last.rank
}

write.csv(state.detail[order(state.detail$state, state.detail$date), c(1,2,5:10,12:16)],file="StateDetail.csv", row.names = FALSE, quote=FALSE)


# Load county detail
#covid_cs <- read.data()
cs.county.state <- read.csv("CSCounties.csv", stringsAsFactors = FALSE, header=TRUE)
county.detail <- covid_cs[toupper(covid_cs$county) != "STATE_OF" & toupper(covid_cs$state) != "ALASKA"
                          & toupper(covid_cs$county) != "UNKNOWN",]
county.detail <- county.detail[order(county.detail$state, county.detail$county, county.detail$date),]

county.detail$state <- toupper(county.detail$state)
county.detail$county <- toupper(county.detail$county)
county.detail$date <- as.Date(county.detail$date)
county.detail$new.cases = county.detail$cases
county.detail$current.cases = county.detail$cases
county.detail$percent.change.prior.day <- 0
county.detail$day <- 1

len <- length(county.detail[,1])
start.row <- len

for (i in rev(seq(2,len)))
{
  county.detail$day[i] <- county.detail$date[start.row] - county.detail$date[i] + 1
  if (county.detail$state[i-1] == county.detail$state[i] &
         county.detail$county[i-1] == county.detail$county[i] )
  {
    county.detail$new.cases[i] <- county.detail$cases[i] - county.detail$cases[i-1] 
    county.detail$percent.change.prior.day[i] <- county.detail$new.cases[i]/county.detail$cases[i-1]
    } else start.row <- i-1

  if (i > 14)
    if (county.detail$state[i-14] == county.detail$state[i] & county.detail$county[i-14] == county.detail$county[i] )
      county.detail$current.cases[i] =  county.detail$cases[i] - county.detail$cases[i-14]
}
county.detail$day[1] <- county.detail$date[start.row] - county.detail$date[1] + 1 


county.detail <- merge(county.detail, pop.data, 
                       by=c("state","county"), all.x=TRUE)

county.detail$state <- toupper(county.detail$STNAME)
county.detail$county <- toupper(county.detail$CTYNAME)

colnames(county.detail)[13] <-  "population"
county.detail$new.cases.per.capita <- 100000 * county.detail$new.cases/county.detail$population
county.detail <- merge(county.detail, cs.county.state, by=c("state","county"), all.x=TRUE)
county.detail$cs[is.na(county.detail$cs)] = 'N'
county.detail$division[is.na(county.detail$division)] = '-'
county.detail$subdivision[is.na(county.detail$subdivision)] = '-'
county.detail$county[county.detail$county=="NEW YORK CITY"] <- "NEW YORK"
county.detail$population[county.detail$county=="NEW YORK"] <- 
  sum(pop.data$POPESTIMATE2019[pop.data$state == "NEW YORK" & 
        (pop.data$county == "NEW YORK CITY" | pop.data$county == "RICHMOND" | pop.data$county == "QUEENS"
          | pop.data$county == "KINGS" | pop.data$county == "BRONX")])


write.table(county.detail[,c(1:10,13:17)], file="countydetail.txt", sep='\t', quote=FALSE,
            row.names = FALSE, col.names = TRUE)


county.summary <- county.detail[is.na(county.detail$state) == FALSE & county.detail$day == 1  , 
                                c("state","county","date","cases","new.cases","population","division", "cs")]
days.to.slope <- 14
for (i in 1:length(county.summary[,1]))
{
  x <- which(county.detail$county == county.summary$county[i] &
               county.detail$state == county.summary$state[i] &
               county.detail$day <= days.to.slope &
               is.na(county.detail$state)==FALSE) 
  county.summary$slope[i] <- -1 * calc.slope(county.detail$day[x],county.detail$new.cases[x]) 
  county.summary$slope.norm[i] <- -1 * calc.slope(county.detail$day[x],county.detail$new.cases.per.capita[x]) 
  
}
write.table(county.summary, "countytrends.txt", sep='\t', row.names = FALSE, col.names = TRUE, quote = FALSE)

write.table(
  merge(
    aggregate(percent.change.prior.day~state+county, data=county.detail[county.detail$day <= 7,],FUN=mean),
    aggregate(date~state+county, data=county.detail[county.detail$day <= 7,],FUN=max),
    by=c("county","state")), 
  file="avgpercentchangebycounty.txt", 
  sep='\t', 
  col.names=TRUE, 
  row.names=FALSE,
  quote=FALSE)
