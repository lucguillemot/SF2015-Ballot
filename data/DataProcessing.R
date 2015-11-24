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
                                                   Turnout=sum(Turnout....))%>% mutate(prop=letter, PYes=Yes/(Yes+No), PNo=No/(Yes+No)) %>% mutate(Perc=ifelse(PYes>PNo,PYes,PNo)) %>% mutate(Winner=ifelse(PYes>PNo,"Yes", "No"))}

# Function to remove two precincts with inconsistent values
removePrecinct <- function(prop) {
      prop<-prop[!prop$PrecinctID==7648,]
      prop<-prop[!prop$PrecinctID==7933,]
}

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

propA <- removePrecinct(propA)
propB <- removePrecinct(propB)
propC <- removePrecinct(propC)
propD <- removePrecinct(propD)
propE <- removePrecinct(propE)
propF <- removePrecinct(propF)
propG <- removePrecinct(propG)
propH <- removePrecinct(propH)
propI <- removePrecinct(propI)
propJ <- removePrecinct(propJ)
propK <- removePrecinct(propK)
# propI4 <- propI %>% mutate(Perc=ifelse(PYes>PNo,PYes,-(PNo)))

# Create a big dataframe with all the propositions
# prop<-rbind(propA,propB,propC,propD,propE,propF,propG,propH,propI,propJ,propK)


write.csv(propA, file="data/propA.csv")
write.csv(propB, file="data/propB.csv")
write.csv(propC, file="data/propC.csv")
write.csv(propD, file="data/propD.csv")
write.csv(propE, file="data/propE.csv")
write.csv(propF, file="data/propF.csv")
write.csv(propG, file="data/propG.csv")
write.csv(propH, file="data/propH.csv")
write.csv(propI, file="data/propI.csv")
write.csv(propJ, file="data/propJ.csv")
write.csv(propK, file="data/propK.csv")


