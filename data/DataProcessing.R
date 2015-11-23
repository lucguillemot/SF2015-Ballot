library(dplyr)
###################################
# Read raw data and create a data frame for each ballot proposition.
con<-file('data/20151119_sov.tsv')
open(con)
propA<-read.table(con, sep="\t", header=TRUE,skip=10326,nrow=794)
propB<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propC<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propD<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propE<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propF<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propG<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propH<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propI<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propJ<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
propK<-read.table(con, sep="\t", header=TRUE,skip=58,nrow=794)
close(con)

# aggregate 'Vote By Mail' and 'Election Day' 
# + add prop letter
# + Calculate Percentage (excluding blank votes)
# + add who wins the game


propSummary <- function(prop, letter) {
      prop %>% group_by(PrecinctID)%>% summarise(Yes=sum(Yes),
                                                   No=sum(No),
                                                   Ballots=sum(Ballots.Cast),
                                                   Turnout=sum(Turnout....))%>% mutate(prop=letter, PYes=Yes/(Yes+No), PNo=No/(Yes+No)) %>% mutate(Perc=ifelse(PYes>PNo,PYes,-(PNo)))}

propA <- propSummary(propA, "A")
propB <- propSummary(propB, "B")
propC <- propSummary(propC, "C")
propD <- propSummary(propD, "D")
propE <- propSummary(propE, "E")
propF <- propSummary(propF, "F")
propG <- propSummary(propG, "G")
propH <- propSummary(propH, "H")
propI <- propSummary(propI, "I")
propJ <- propSummary(propJ, "J")
propK <- propSummary(propK, "K")


# propI4 <- propI %>% mutate(Perc=ifelse(PYes>PNo,PYes,-(PNo)))

# Create a big dataframe with all the propositions
# prop<-rbind(propA,propB,propC,propD,propE,propF,propG,propH,propI,propJ,propK)


#write.csv(propI, file="data/propI.csv")


