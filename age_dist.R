# take a look at electorates by age/sex

# plot we're going to use is going to be total
library(dplyr)
library(stringdist)
library(RColorBrewer)

dat <- read.csv("csv/Age group and sex by general electorate.csv", stringsAsFactors=FALSE)
dat_m <- read.csv("csv/Age group and sex by Māori electorate.csv", stringsAsFactors=FALSE)

party <- read.csv("../nzpol/swing/data/party2014.csv", stringsAsFactors=FALSE) %>% rename(Electorate=X)

dat <- dat %>% select(-level2)
dat <- rbind(dat, dat_m)
dat <- dat %>%
       rename(Age=level0, Electorate=level1, Sex=Age.group.and.sex) %>%
       filter(Age != "18 Years And Over",
              Electorate != "Area Outside General Electorate",
              Electorate != "Area Outside Māori Electorate")

# electorate remapping
electorates <- unique(dat$Electorate)

elect_map <- adist(party$Electorate, electorates)
elect_wch <- apply(elect_map,2,which.min)
elect_map <- matrix(party$Electorate[elect_wch], ncol=1)
rownames(elect_map) <- electorates

dat <- dat %>% mutate(Electorate = elect_map[Electorate,1])
electorates <- unique(dat$Electorate)

# party to left-right gauge
left   <- c("Green.Party", "Labour.Party", "Internet.MANA")
right  <- c("National.Party", "ACT.New.Zealand", "Conservative")

party$left <- rowSums(party[,names(party) %in% left])
party$right <- rowSums(party[,names(party) %in% right])
party$left > party$right

party <- party %>% mutate(right_left = (right - left) / (right + left))

dat <- dat %>% left_join(party %>% select(Electorate,right_left))

# colour mapping
colour_ramp <- colorRamp(brewer.pal(11, "RdBu"), space="Lab")
colour_map <- function(x, alpha) {
  rgb(colour_ramp(x), alpha = 255*alpha, max=255)
}

plot_electorate <- function(dat, elect_name, cols) {
  electorate <- dat %>% filter(Electorate == elect_name)
  males <- electorate %>% filter(Sex == "Male")
  females <- electorate %>% filter(Sex == "Female")
  total <- sum(electorate$count)

  # work out the colour for this electorate
  value <- (electorate$right_left[1] + 1)/2
  col_f <- colour_map(value, 0.4)
  col_m <- colour_map(value, 0.8)
  
  plot(NULL, xlim=c(0,100), ylim=c(-0.1,0.1), xlab="", ylab="",
       yaxt="n", xaxs="i", bty="n", fg="grey60", col.axis="grey60")
# mtext(elect_name, side=3, at=50, font=2, cex=0.9, line=-0.5, col="grey30")
  rect(seq(0,85,by=5), 0, seq(5,90,by=5), females$count/total, col=col_f, lwd=0.5)
  rect(seq(0,85,by=5), 0, seq(5,90,by=5), -males$count/total, col=col_m, lwd=0.5)
}

cols <- c("seashell2", "wheat3")

png("age_dist.png", width=960, height=1280)
par(mfrow=c(9,8), mar=c(3,1,2,1), omi=c(0.25,0.25,1,0.25))

# first 7
for (elect_name in electorates[1:7])
  plot_electorate(dat, elect_name, cols)

# legend top right
plot(NULL, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
legend(0, 0.8, legend=c("Female", "Male"), fill=grey(c(0.9,0.7)), bty="n", cex=1.5)

#or (elect_name in electorates[-(1:7)])
  plot_electorate(dat, elect_name, cols)

# header and footer
mtext("Age distribution of general electorates", outer=TRUE, at=0.5, adj=0.5, line=3, cex=2, side=3)
mtext("@jmarshallnz", side=1, line=0, at=1, adj=1, cex=0.8, outer=TRUE)

dev.off()