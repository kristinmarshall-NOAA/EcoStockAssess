library(MASS)
require(likert)
require(ggplot2)
require(reshape)
library(ggsidekick)
library(dplyr)
library(tidyr)


dat=read.csv("Data/RAMdatSept13.csv", header=T)

dat=dat[is.na(dat$Year)==F,]




datr=dat[,c(8:14)]
datr=round(datr)
dat[,c(8:14)]=datr

response=c('Habitat','Climate','Bycatch','TechInt','Diet','Predator')
predict=c('sp.type','Year','Region','Council')


dat.long=reshape(dat, direction="long", v.names="score", timevar="EcoInt",varying=list(8:14))
dat.long$EcoInt[dat.long$EcoInt==1]="Habitat"
dat.long$EcoInt[dat.long$EcoInt==5]="Climate"
dat.long$EcoInt[dat.long$EcoInt==2]="Bycatch Other"
dat.long$EcoInt[dat.long$EcoInt==6]="Bycatch Target"
dat.long$EcoInt[dat.long$EcoInt==3]="Diet"
dat.long$EcoInt[dat.long$EcoInt==4]="Predation"
dat.long$EcoInt[dat.long$EcoInt==7]="Competition"

levels(dat.long$EcoInt)=c('Habitat','Climate','Bycatch','TechInt','Diet','Predator','Competition')

dat.long$EcoInt=factor(dat.long$EcoInt, levels=c("Bycatch Target","Bycatch Other", "Habitat","Climate","Diet","Predation","Competition"))

  

dat.long.sub=dat.long
dat.long.sub$EcoInt=factor(dat.long.sub$EcoInt, levels=c("Bycatch Target","Bycatch Other", "Habitat","Climate","Diet","Predation","Competition"))

dat.all <- cbind(expand.grid(EcoInt=levels(dat.long.sub$EcoInt), score=levels(as.factor(dat.long.sub$score)), n.count=NA, per.count=NA))
dat.all$score=as.numeric(dat.all$score)-1
for(i in levels(dat.all$EcoInt)) {
  for(j in 0:3) {
    dat.all$n.count[dat.all$EcoInt==i & dat.all$score==j]=length(which(dat.long$EcoInt==i & dat.long$score==j))
  }
}

dat.all$per.count=dat.all$n.count/dim(dat)[1]
dat.sub=dat.all[dat.all$score>0,]
dat.sub$Usage=factor(dat.sub$score, labels=c('Background','Qual.','Quant.'))



##Figure 1, a histogram of frequency of scores by category of ecol. info
ggplot(dat.sub, aes(x=factor(EcoInt), y=per.count, fill=Usage), cex=2) +
  geom_bar(width=.7, position="dodge", stat="identity") +
  scale_fill_brewer(palette='Greys')+
  #theme(legend.title=element_blank(), text=element_text(size=18),panel.background = element_rect(fill = 'grey55'),
  #      panel.grid.major = element_line(colour = "grey65"),
  #      panel.grid.minor = element_line(colour = "grey65")) +
    labs(x="", y="Proportion") +theme_sleek()
  


##Diet Lab
dat.long.sub=dat.long[(dat.long$EcoInt=="Predation" | dat.long$EcoInt=="Diet"),]
dat.long.sub$DietLab=factor(dat.long.sub$DietLab,labels=c('No diet lab','Diet lab'))

dat.long.sub$Usage=factor(dat.long.sub$score, labels=c('None','Background','Qual.','Quant.'))
ggplot(dat.long.sub, aes(DietLab, fill=Usage)) +
  geom_bar(position='fill') +
  scale_fill_brewer(palette='Greys') +
  labs(x="", y="Proportion") +
  facet_wrap(~EcoInt, ncol=2) +
  theme_sleek()

summ.dat<- dat.long.sub %>%
  group_by(EcoInt,DietLab) %>%
  count(score)

  #filter(score>1)

##Overfishing
dat.long.sub=dat.long[dat.long$EcoInt!="Competition",]
dat.long.sub$EcoInt=factor(dat.long.sub$EcoInt, levels=c("Bycatch Target","Bycatch Other", "Habitat","Climate","Diet","Predation"))
dat.long.sub$Usage=factor(dat.long.sub$score, labels=c('None','Background','Qual.','Quant.'))
dat.long.sub$OF0105=factor(dat.long.sub$OF0105, labels=c('Not overfished','Overfished'))


ggplot(dat.long.sub, aes(OF0105, fill=Usage)) +
  geom_bar(position='fill') +
  scale_fill_brewer(palette='Greys') +
  labs(x="", y="Proportion") +
  facet_wrap(~EcoInt, ncol=2) +
  theme_sleek()



##Life History types
dat.long.sub$Sptype=factor(dat.long.sub$Sptype, labels=c('forage', 'demersal', 'invert', 'pelagic'))
ggplot(dat.long.sub, aes(factor(Sptype), fill=Usage)) +
  geom_bar(position='fill') +
  scale_fill_brewer(palette='Greys') +
  labs(x="", y="Proportion") +
  facet_wrap(~EcoInt, ncol=2) +
  theme_sleek()
  
pel.dat= dat.long.sub %>%
  filter(Sptype=='pelagic')
  
###plot by revenue
dat.long.sub=dat.long[dat.long$EcoInt %in% c("Habitat","Climate","Predation","Diet"),]
dat.long.sub$EcoInt=factor(dat.long.sub$EcoInt, levels=c("Bycatch Target","Bycatch Other", "Habitat","Climate","Diet","Predation"))
#dat.long.sub=dat.long.sub[dat.long.sub$score>0,]
dat.long.sub$Usage=factor(dat.long.sub$score, labels=c('None','Background','Qual.','Quant.'))
dat.long.sub$Rev2013=dat.long.sub$Rev2013+1

ggplot(dat.long.sub, aes((Rev2013), fill=Usage)) +
  geom_histogram(position='stack')+
  stat_bin(binwidth=1) +
  #geom_density() +
  scale_fill_brewer(palette='Greys') +
  scale_x_log10() +
 facet_wrap(~EcoInt, ncol=2) +
 # geom_jitter(height=0.2, width=0) 
  xlab('2013 Revenue (USD)') +ylab('Count')+
  theme_sleek()


##What proportion of stocks have at least one 3?
stocks.3<-filter(dat.long, score>2 & EcoInt %in% c('Habitat','Climate', 'Diet','Predation'))

stock.tab<- stocks.3 %>%
  spread(EcoInt, score ) %>%
  select(-c(source,Rev2013,Record:id)) 

stock.tab$Habitat[stock.tab$Habitat>2]="habitat"
stock.tab$Climate[stock.tab$Climate>2]="climate"
stock.tab$Predation[stock.tab$Predation>2]="predation"

stock.tabl<- stock.tab %>%
  mutate(Eco.Consid=paste(Habitat, Climate, Predation, sep=".")) %>%
  select(-c(Habitat:Predation))

write.csv(stock.tabl, "Data/StocksLevel3Table.csv")

hab.3<- filter(stocks.3, EcoInt=='Habitat')


dat$Habitat=as.factor(dat$Habitat)
dat$Climate=as.factor(dat$Climate)
dat$Bycatch=as.factor(dat$Bycatch)
dat$TechInt=as.factor(dat$TechInt)
dat$Diet=as.factor(dat$Diet)
dat$Predator=as.factor(dat$Predator)
dat$Lab=as.factor(dat$Lab)
dat$Council=as.factor(dat$Council)


cor(dat[,c(8:14)], method='kendall') # kendall's tau for rank correlation

m=polr(Habitat~Region, data=dat, Hess=T)


ind=which(is.na(dat$Rev2013))
datrev=dat[-ind,]
ind3=which(datrev$Rev2013==0)
datrev$Rev2013[ind3]=1

m=polr(Habitat~log(Rev2013), data=datrev, Hess=T)


(ci <- confint(m))
confint.default(m)
exp(coef(m))
exp(cbind(OR = coef(m), ci))







###################
##likert analysis-- add one to all values to get rid of zeros
likdat=round(dat[,c(8:13)]+1)
names(likdat)=c('Habitat','BycatchOther','Diet','Predator','Climate','BycatchTarget')

likdat=likdat[,c(1,3:5,6,2)]
likdat$Habitat=as.factor(likdat$Habitat)
likdat$Climate=as.factor(likdat$Climate)
likdat$BycatchOther=as.factor(likdat$BycatchOther)
likdat$BycatchTarget=as.factor(likdat$BycatchTarget)
likdat$Diet=as.factor(likdat$Diet)
likdat$Predator=as.factor(likdat$Predator)


levels(likdat$Habitat)=c("1","2","3","4")
levels(likdat$Climate)=c("1","2","3","4")
levels(likdat$BycatchOther)=c("1","2","3","4")
levels(likdat$BycatchTarget)=c("1","2","3","4")
levels(likdat$Diet)=c("1","2","3","4")
levels(likdat$Predator)=c("1","2","3","4")

lik=likert(likdat)
summary(lik)

plot(lik)

plot(lik, centered = TRUE, center=2.5, wrap = 30, low.col="dodgerblue", high.col="darkcyan")
  #plot(lik, centered = FALSE, wrap = 30, low.col='red', high.col='blue', low.col="grey70", high.col="darkcyan")
plot(lik, type = "density")

plot(lik, type = "heat")


indx=which(dat$Lab=="SWFSC")
dat$Lab2=dat$Lab
dat$Lab2[indx]="NW-SW"
ind2=which(dat$Lab=="ADFG")
dat$Lab2[ind2]="AFSC"


#Gruop by Science Center
likg <- likert(likdat, grouping = dat$Lab2)
plot(likg, centered=TRUE, include.histogram = TRUE)

#group by council
likgc<-likert(likdat, grouping=dat$Council)
plot(likgc, centered=FALSE, include.histogram = TRUE)


##group by species type
liksp<-likert(likdat[,c(1,3:5)], grouping=dat$Sptype)
plot(liksp, centered=TRUE, center=2.5, include.histogram = TRUE)


##group by dietlab
liksp<-likert(likdat[,c(2,3)], grouping=dat$DietLab)
plot(liksp, centered=TRUE, center=2.5, include.histogram = TRUE)


##group by OFearly
#liksp<-likert(likdat, grouping=dat$OF10.14)
#plot(liksp, centered=FALSE, include.histogram = TRUE)

liksp<-likert(likdat[,c(1:4)], grouping=dat$OF0105)
plot(liksp, centered=TRUE, include.histogram = TRUE)


### Think about something with a "total score" and look at that by region.  
##If regions have different things taht matter, then it should balance outoverall?


 ##################
 ###logistic regression with revenue
 ###################

##not really anything here with log(revenue)...

ind=which(dat[,c(8:14)]<2)
newdat=dat
ind=which(is.na(newdat$Rev2013))
newdat=newdat[-ind,]
ind3=which(newdat$Rev2013==0)
newdat$Rev2013[ind3]=1

newdat=na.omit(newdat)


for (i in 8:14){
    ind=which(dat[,i]>1)
    newdat[,i]=0
    newdat[ind,i]=1
} 
 
 glm1=glm(Diet~Rev2013, dat=newdat, family='binomial')
 glm2=glm(Predator~Rev2013, dat=newdat, family='binomial')
 
 plot(newdat$Rev2013, newdat$Diet)
 points(newdat$Rev2013,order(glm1$fitted.values), col='red')
 
 
 