library(XML)
library(uuid)
library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)

f <- "electoral-college-votes.csv"
electoral.college <- read.csv(f, header=FALSE)
names(electoral.college) <- c("state", "electoral_votes")
head(electoral.college)

# exclude D.C. from the data pull b/c there aren't any polls!. we'll add it in manually
states <- electoral.college$state[c(1:50)]
results <- ldply(states, function(state) {
  url <- "http://www.electionprojection.com/latest-polls/%s-presidential-polls-trump-vs-clinton-vs-johnson-vs-stein.php"
  state.fmt <- gsub(" ", "-", tolower(state))
  
  url.state <- sprintf(url, state.fmt)
  print(url.state)
  r <- readHTMLTable(url.state, stringsAsFactors=FALSE)
  index1 = min(which(lapply(r, is.null) == FALSE))
  r <- r[[index1]]
  r$state <- state
  r$id <- 1:nrow(r)
  cols <- c("Dates", "Firm", "state", "Clinton", "Trump", "Johnson", "id")
  r <- r[2:nrow(r),][,cols]
  r <- melt(r, id=c("Dates", "Firm", "state", "id"), variable.name="candidate", value.name="vote")
  
  names(r) <- c("date", "poll", "state", "id", "candidate", "vote")
  r$race <- ""
  cols <- c("date", "race", "state", "poll", "candidate", "vote", "id")
  r <- r[,cols]
  r$vote <- as.numeric(r$vote)
  r
})

# adding D.C. on manually b/c it's slightly different. it also doesn't produce material changes to the
# results
results <- rbind(results, data.frame(
  date='10/20 - 10/28',
  race='',
  state='District of Columbia',
  poll='SurveyMonkey',
  candidate=c("Clinton", "Trump", "Johnson"),
  vote=c(87, 5, 4),
  id=1
))

head(results)
tail(results)
table(results$candidate)
table(results$state)
results <- results[order(results$state, results$id, results$candidate),]

poll.freq <- data.frame(table(results$state)/3)
ggplot(poll.freq, aes(x=Var1, weight=Freq)) +
  geom_bar() + 
  coord_flip() + 
  scale_y_continuous("# of Polls") +
  scale_x_discrete("State", limits=rev(levels(poll.freq$Var1)))


#################################

## Get latest poll ##
states <- electoral.college$state[c(1:50)]
results1 <- ldply(states, function(state) {
  url <- "http://www.electionprojection.com/latest-polls/%s-presidential-polls-trump-vs-clinton-vs-johnson-vs-stein.php"
  state.fmt <- gsub(" ", "-", tolower(state))
  
  url.state <- sprintf(url, state.fmt)
  print(url.state)
  r <- readHTMLTable(url.state, stringsAsFactors=FALSE)
  index1 = min(which(lapply(r, is.null) == FALSE))
  r <- r[[index1]]
  r$state <- state
  r$id <- nrow(r)
  cols <- c("Dates", "Firm", "state", "Clinton", "Trump", "Johnson", "id")
  r <- r[nrow(r),][,cols]
  r <- melt(r, id=c("Dates", "Firm", "state", "id"), variable.name="candidate", value.name="vote")
  
  names(r) <- c("date", "poll", "state", "id", "candidate", "vote")
  r$race <- ""
  cols <- c("date", "race", "state", "poll", "candidate", "vote", "id")
  r <- r[,cols]
  r$vote <- as.numeric(r$vote)
  r
})

# adding D.C. on manually b/c it's slightly different. it also doesn't produce material changes to the
# results
results1 <- rbind(results1, data.frame(
  date='10/20 - 10/28',
  race='',
  state='District of Columbia',
  poll='SurveyMonkey',
  candidate=c("Clinton", "Trump", "Johnson"),
  vote=c(87, 5, 4),
  id=1
))
#################################

################################
### for each state, run a simulation ####
####################################
C_final= c()
T_final = c()
J_final = c()
Kmax = 2000
for (K in 1:Kmax){
C_total = 0 
T_total = 0 
J_total = 0
for (i in 1:51){
  temp = runif(1000)*100
  count1 = length(which(temp<=results1[3*(i-1)+1,6]))
  count2 = length(which(temp>results1[3*(i-1)+1,6] & temp<=(results1[3*(i-1)+1,6]+results1[3*(i-1)+2,6])))
  count3 = length(which(temp>(results1[3*(i-1)+1,6]+results1[3*(i-1)+2,6])))
  count = which.max(c(count1, count2, count3))
  if (count == 1){
    C_total = C_total + electoral.college[i,2]
  }else if (count == 2){
    T_total = T_total + electoral.college[i,2]
  }else{
      J_total = J_total + electoral.college[i,2]
    }
  
}
C_final = c(C_final, C_total)
T_final = c(T_final, T_total)
J_final = c(J_final, J_total)
}
C_final = data.frame(C_final)
names(C_final) = 'final'
T_final = data.frame(T_final)
names(T_final) = 'final'
J_final = data.frame(J_final)
names(J_final) = 'final'
C_final$names <- 'Clinton'
T_final$names<- 'Trump'
J_final$names <- 'Johnson'
ll = rbind(C_final,T_final)
quartz()
ggplot(ll, aes(final, fill = names)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')
length(which(C_final>T_final))/Kmax 
############################
############################






