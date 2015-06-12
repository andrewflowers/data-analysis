###############################
#
# Consumer Expenditure Survey Microdata
#
###############################

setwd("\\\\BRSFS1/CasselmB$/My Documents/R/CEX")
library(reshape2)

# Start by parsing the 2009 and 2010 microdata

# We need the FMLY data
fmly10.1<-read.csv("fmli101x.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
fmly10.2<-read.csv("fmli102.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
fmly10.3<-read.csv("fmli103.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
fmly10.4<-read.csv("fmli104.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
fmly10.5<-read.csv("fmli111.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)

# We need to turn these into a single year
fmly10.all<-rbind(fmly10.1,fmly10.2,fmly10.3,fmly10.4,fmly10.5)

# We need to create the variable MO_SCOPE, per BLS.
# This will tell us how many months in sample each
# CU is. Needed for appropriate weighting.
fmly10.all$MO_SCOPE[fmly10.all$QINTRVYR==2010 &
                        fmly10.all$QINTRVMO==1]<-0
fmly10.all$MO_SCOPE[fmly10.all$QINTRVYR==2010 &
                        fmly10.all$QINTRVMO==2]<-1
fmly10.all$MO_SCOPE[fmly10.all$QINTRVYR==2010 &
                        fmly10.all$QINTRVMO==3]<-2
fmly10.all$MO_SCOPE[fmly10.all$QINTRVYR==2010 &
                        fmly10.all$QINTRVMO>3 &
                        fmly10.all$QINTRVMO<13]<-3
fmly10.all$MO_SCOPE[fmly10.all$QINTRVYR==2011 &
                        fmly10.all$QINTRVMO==1]<-3
fmly10.all$MO_SCOPE[fmly10.all$QINTRVYR==2011 &
                        fmly10.all$QINTRVMO==2]<-2
fmly10.all$MO_SCOPE[fmly10.all$QINTRVYR==2011 &
                        fmly10.all$QINTRVMO==3]<-1

# Create scope_weight, and divide by four to create one pop samle
fmly10.all$scope_weight<-((fmly10.all$FINLWT21 * fmly10.all$MO_SCOPE)/3)/4

# We need population totals, both for the
# full population and for the income groups.
# Also limit ourselves to urban dwellers.
pop.full<-sum(fmly10.all$scope_weight[fmly10.all$BLS_URBN==1])
pop.L1<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==1 & fmly10.all$BLS_URBN==1])
pop.L2<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==2 & fmly10.all$BLS_URBN==1])
pop.L3<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==3 & fmly10.all$BLS_URBN==1])
pop.L4<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==4 & fmly10.all$BLS_URBN==1])
pop.L5<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==5 & fmly10.all$BLS_URBN==1])
pop.L6<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==6 & fmly10.all$BLS_URBN==1])
pop.L7<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==7 & fmly10.all$BLS_URBN==1])
pop.L8<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==8 & fmly10.all$BLS_URBN==1])
pop.L9<-sum(fmly10.all$scope_weight[fmly10.all$INCLASS==9 & fmly10.all$BLS_URBN==1])

# And the MTAB data
mtab10.1<-read.csv("mtbi101x.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
mtab10.2<-read.csv("mtbi102.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
mtab10.3<-read.csv("mtbi103.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
mtab10.4<-read.csv("mtbi104.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
mtab10.5<-read.csv("mtbi111.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)

mtab10.all<-rbind(mtab10.1,mtab10.2,mtab10.3,mtab10.4,mtab10.5)

save(fmly10.1,fmly10.2,fmly10.3,fmly10.4,fmly10.5,mtab10.1,mtab10.2,mtab10.3,mtab10.4,mtab10.5,file="INTVW2010.RData")
rm(fmly10.1,fmly10.2,fmly10.3,fmly10.4,fmly10.5,mtab10.1,mtab10.2,mtab10.3,mtab10.4,mtab10.5)

# We only want expenditures from 2010
spend2010<-subset(mtab10.all,REF_YR==2010)
save(fmly10.all,mtab10.all,file="INTVW10_all.RData")
rm(mtab10.all)


# Now we need to pull these into categories
ucc_map<-read.csv("ucc_map_revised.csv",header=TRUE,stringsAsFactors=FALSE)

spend2010<-merge(spend2010,ucc_map,by="UCC")

# Have to multiply three UCC codes by 4, per documentation
spend2010$COST[spend2010$UCC==(006001 | 006002 | 710110)]<-mapply(function(x)x*12,spend2010$COST[spend2010$UCC==(006001 | 006002 | 710110)])

# Also for OER
spend2010$COST[spend2010$UCC==910050]<-mapply(function(x)x*12,spend2010$COST[spend2010$UCC==910050])

save(spend2010,file="spend2010.RData")

# We're only worried about annual spending by CU
# for each category.So we can collapse this data by
# category.

require(doBy)
collapse<-summaryBy(as.numeric(COST) ~ NEWID + category_code + category_description,data=spend2010,FUN=sum)
rm(spend2010)
colnames(collapse)[4]<-"COST.sum"

# Now we need to combine this with FMLY in order to
# get spending by CU.
# We're gonna need more memory for this
memory.limit(size=20000)
FullSpend10<-merge(fmly10.all,collapse,by="NEWID")

save(collapse,fmly10.all,file="Micro2010.RData")
rm(collapse,fmly10.all)

# Now we need to adjust the spending figures.
# Now we need to multiply by their weight
# and covert to quarters
FullSpend10$weighted_COST<-(FullSpend10$COST.sum) * (FullSpend10$scope_weight/(pop.full))*4
save(FullSpend10,file="FullSpend10.RData")

sum(FullSpend10$weighted_COST)

#################################
#
# CEX Diary files

# Need FMLY and EXPN files
Dfmly10.1<-read.csv("fmld101.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
Dfmly10.2<-read.csv("fmld102.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
Dfmly10.3<-read.csv("fmld103.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
Dfmly10.4<-read.csv("fmld104.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)

Dexpnd10.1<-read.csv("expd101.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
Dexpnd10.2<-read.csv("expd102.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
Dexpnd10.3<-read.csv("expd103.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)
Dexpnd10.4<-read.csv("expd104.csv",na.strings=".",stringsAsFactors=FALSE,header=TRUE)

# Combine each into full-year files
Dfmly10.all<-rbind(Dfmly10.1,Dfmly10.2,Dfmly10.3,Dfmly10.4)
Dexpnd10.all<-rbind(Dexpnd10.1,Dexpnd10.2,Dexpnd10.3,Dexpnd10.4)
rm(Dfmly10.1,Dfmly10.2,Dfmly10.3,Dfmly10.4,Dexpnd10.1,Dexpnd10.2,Dexpnd10.3,Dexpnd10.4)

# We need population totals. Only want urban residents.
Dpop<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$BLS_URBN==1])
Dpop.L1<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==1 & Dfmly10.all$BLS_URBN==1])
Dpop.L2<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==2 & Dfmly10.all$BLS_URBN==1])
Dpop.L3<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==3 & Dfmly10.all$BLS_URBN==1])
Dpop.L4<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==4 & Dfmly10.all$BLS_URBN==1])
Dpop.L5<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==5 & Dfmly10.all$BLS_URBN==1])
Dpop.L6<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==6 & Dfmly10.all$BLS_URBN==1])
Dpop.L7<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==7 & Dfmly10.all$BLS_URBN==1])
Dpop.L8<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==8 & Dfmly10.all$BLS_URBN==1])
Dpop.L9<-sum(Dfmly10.all$FINLWT21[Dfmly10.all$INCLASS==9 & Dfmly10.all$BLS_URBN==1])

# Get ucc mapping and merge together
ucc_map<-read.csv("ucc_map_revised.csv",header=TRUE,stringsAsFactors=FALSE)
Dspend2010<-merge(Dexpnd10.all,ucc_map,by="UCC")

# Now we collapse these
require(doBy)
collapse<-summaryBy(COST ~ NEWID + category_code + category_description,data=Dspend2010,FUN=sum)
save(Dspend2010,file="Dspend2010.RData")
rm(Dspend2010)

# Now we merge this into the family file
Diary2010<-merge(Dfmly10.all,collapse,by="NEWID")

Diary2010$weighted_COST<-Diary2010$COST * (Diary2010$FINLWT21/Dpop)

# Prepare to merge with the interview files
DiarySub10<-subset(Diary2010,BLS_URBN==1,select=c(NEWID,CUID,BLS_URBN,REGION,AGE_REF,POV_CY,INCLASS,STATE,INC_RANK,FINCBEFM,FINCAFTM,FINLWT21,category_code,category_description,COST.sum,weighted_COST))
colnames(DiarySub10)[12]<-"weight"
colnames(DiarySub10)[10:11]<-c("FINCBTXM","FINCATAX")

# For merging with interview, we want ONLY food items
#food_codes<-read.csv("food_codes.csv",header=TRUE,stringsAsFactors=FALSE)
#DiarySub10<-merge(DiarySub10,food_codes,by="category_code")

# Have to adjust costs from weekly to annual
DiarySub10$weighted_COST<-DiarySub10$weighted_COST * 52

# Need these for the various income levels as well
DiarySub10.L1<-subset(DiarySub10,INCLASS==1)
DiarySub10.L2<-subset(DiarySub10,INCLASS==2)
DiarySub10.L3<-subset(DiarySub10,INCLASS==3)
DiarySub10.L4<-subset(DiarySub10,INCLASS==4)
DiarySub10.L5<-subset(DiarySub10,INCLASS==5)
DiarySub10.L6<-subset(DiarySub10,INCLASS==6)
DiarySub10.L7<-subset(DiarySub10,INCLASS==7)
DiarySub10.L8<-subset(DiarySub10,INCLASS==8)
DiarySub10.L9<-subset(DiarySub10,INCLASS==9)
DiarySub10.low<-subset(DiarySub10,INCLASS<9)

# And reweight the costs
DiarySub10.L1$weighted_COST<-DiarySub10.L1$COST.sum * (DiarySub10.L1$weight / Dpop.L1)*52
DiarySub10.L2$weighted_COST<-DiarySub10.L2$COST.sum * (DiarySub10.L2$weight / Dpop.L2)*52
DiarySub10.L3$weighted_COST<-DiarySub10.L3$COST.sum * (DiarySub10.L3$weight / Dpop.L3)*52
DiarySub10.L4$weighted_COST<-DiarySub10.L4$COST.sum * (DiarySub10.L4$weight / Dpop.L4)*52
DiarySub10.L5$weighted_COST<-DiarySub10.L5$COST.sum * (DiarySub10.L5$weight / Dpop.L5)*52
DiarySub10.L6$weighted_COST<-DiarySub10.L6$COST.sum * (DiarySub10.L6$weight / Dpop.L6)*52
DiarySub10.L7$weighted_COST<-DiarySub10.L7$COST.sum * (DiarySub10.L7$weight / Dpop.L7)*52
DiarySub10.L8$weighted_COST<-DiarySub10.L8$COST.sum * (DiarySub10.L8$weight / Dpop.L8)*52
DiarySub10.L9$weighted_COST<-DiarySub10.L9$COST.sum * (DiarySub10.L9$weight / Dpop.L9)*52
DiarySub10.low$weighted_COST<-DiarySub10.low$COST.sum * (DiarySub10.low$weight / (Dpop.L1 + Dpop.L2 +Dpop.L3+Dpop.L4+Dpop.L5+Dpop.L6+Dpop.L7+Dpop.L8))*52

save(Dfmly10.all,file="Dfmly10.RData")
save(Dexpnd10.all,file="Dexpnd10.RData")
save(DiarySub10,file="DiarySub10.RData")
rm(Dfmly10.all,Dexpnd10.all,DiarySub10)

##############
# We want to merge the diary and interview sections
# First we'll take only the subset we need of FullSpend10

SpendSub10<-subset(FullSpend10,BLS_URBN==1,select=c(NEWID,CUID,BLS_URBN,REGION,AGE_REF,POV_CY,INCLASS,STATE,INC_RANK,FINCBTXM,FINCATAX,scope_weight,category_code,category_description,COST.sum,weighted_COST))
colnames(SpendSub10)[12]<-"weight"

SpendSub10$weighted_COST<-SpendSub10$weighted_COST/3

save(FullSpend10,file="FullSpend10.RData")
rm(FullSpend10)

# We also need these for each income class
SpendSub10.L1<-subset(SpendSub10,INCLASS==1)
SpendSub10.L2<-subset(SpendSub10,INCLASS==2)
SpendSub10.L3<-subset(SpendSub10,INCLASS==3)
SpendSub10.L4<-subset(SpendSub10,INCLASS==4)
SpendSub10.L5<-subset(SpendSub10,INCLASS==5)
SpendSub10.L6<-subset(SpendSub10,INCLASS==6)
SpendSub10.L7<-subset(SpendSub10,INCLASS==7)
SpendSub10.L8<-subset(SpendSub10,INCLASS==8)
SpendSub10.L9<-subset(SpendSub10,INCLASS==9)
SpendSub10.low<-subset(SpendSub10,INCLASS<9)

# And we have to reweight the samples accordingly
SpendSub10.L1$weighted_COST<-SpendSub10.L1$COST.sum * (SpendSub10.L1$weight/pop.L1)*4/3
SpendSub10.L2$weighted_COST<-SpendSub10.L2$COST.sum * (SpendSub10.L2$weight/pop.L2)*4/3
SpendSub10.L3$weighted_COST<-SpendSub10.L3$COST.sum * (SpendSub10.L3$weight/pop.L3)*4/3
SpendSub10.L4$weighted_COST<-SpendSub10.L4$COST.sum * (SpendSub10.L4$weight/pop.L4)*4/3
SpendSub10.L5$weighted_COST<-SpendSub10.L5$COST.sum * (SpendSub10.L5$weight/pop.L5)*4/3
SpendSub10.L6$weighted_COST<-SpendSub10.L6$COST.sum * (SpendSub10.L6$weight/pop.L6)*4/3
SpendSub10.L7$weighted_COST<-SpendSub10.L7$COST.sum * (SpendSub10.L7$weight/pop.L7)*4/3
SpendSub10.L8$weighted_COST<-SpendSub10.L8$COST.sum * (SpendSub10.L8$weight/pop.L8)*4/3
SpendSub10.L9$weighted_COST<-SpendSub10.L9$COST.sum * (SpendSub10.L9$weight/pop.L9)*4/3
SpendSub10.low$weighted_COST<-SpendSub10.low$COST.sum * (SpendSub10.low$weight/(pop.L1 + pop.L2 + pop.L3 + pop.L4 + pop.L5 + pop.L6 + pop.L7 + pop.L8))*4/3


# Now we'll merge in the diary data
CombinedSpend10<-rbind(SpendSub10,DiarySub10)
CombinedSpend10.L1<-rbind(SpendSub10.L1,DiarySub10.L1)
CombinedSpend10.L2<-rbind(SpendSub10.L2,DiarySub10.L2)
CombinedSpend10.L3<-rbind(SpendSub10.L3,DiarySub10.L3)
CombinedSpend10.L4<-rbind(SpendSub10.L4,DiarySub10.L4)
CombinedSpend10.L5<-rbind(SpendSub10.L5,DiarySub10.L5)
CombinedSpend10.L6<-rbind(SpendSub10.L6,DiarySub10.L6)
CombinedSpend10.L7<-rbind(SpendSub10.L7,DiarySub10.L7)
CombinedSpend10.L8<-rbind(SpendSub10.L8,DiarySub10.L8)
CombinedSpend10.L9<-rbind(SpendSub10.L9,DiarySub10.L9)
CombinedSpend10.low<-rbind(SpendSub10.low,DiarySub10.low)
CombinedSpend10.high<-CombinedSpend10.L9

# Test this to see how it matches published data
sum(CombinedSpend10$weighted_COST)
sum(CombinedSpend10.L2$weighted_COST)
sum(CombinedSpend10.L4$weighted_COST)
sum(CombinedSpend10.L9$weighted_COST)

Total<-sum(CombinedSpend10$weighted_COST)
FoodBev<-sum(DiarySub10$weighted_COST)
Rent<-sum(CombinedSpend10$weighted_COST[CombinedSpend10$category_code=="HA01"])
OER<-sum(CombinedSpend10$weighted_COST[CombinedSpend10$category_code=="HC01"])
NewCar<-sum(CombinedSpend10$weighted_COST[CombinedSpend10$category_code=="TA01"])
UsedCar<-sum(CombinedSpend10$weighted_COST[CombinedSpend10$category_code=="TA02"])
Gas<-sum(CombinedSpend10$weighted_COST[CombinedSpend10$category_code=="TB01"])


(sum(CombinedSpend10$weighted_COST[CombinedSpend10$category_code=="HA01"])/sum(CombinedSpend10$weighted_COST)) * 100

sum(DiarySub10$weighted_COST[DiarySub10$category_code=="FX01"])
sum(CombinedSpend10$weighted_COST[CombinedSpend10$BLS_URBN==1 & CombinedSpend10$category_code=="HA01"])/sum(CombinedSpend10$weighted_COST[CombinedSpend10$BLS_URBN==1])
sum(FullSpend10$weighted_COST[FullSpend10$category_code=="HC01"])

sum(FullSpend10$weighted_COST[FullSpend10$category_code=="HC01" & FullSpend10$BLS_URBN==1])/sum(FullSpend10$weighted_COST[FullSpend10$BLS_URBN==1])

save(SpendSub10,file="SpendSub10.RData")

###################

# We now need to calculate the spending shares
SpendCast10<-summaryBy(weighted_COST ~ category_code,data=CombinedSpend10,FUN=sum)
colnames(SpendCast10)[2]<-"cost"
TotCost<-sum(SpendCast10$cost)
SpendCast10$share<-mapply(function(x)x/TotCost,SpendCast10$cost)
save(SpendCast10,file="SpendCast10.RData")

SpendCast10.low<-summaryBy(weighted_COST ~ category_code,data=CombinedSpend10.low,FUN=sum)
colnames(SpendCast10.low)[2]<-"cost"
TotCost.low<-sum(SpendCast10.low$cost)
SpendCast10.low$share<-mapply(function(x)x/TotCost.low,SpendCast10.low$cost)

SpendCast10.high<-summaryBy(weighted_COST ~ category_code,data=CombinedSpend10.high,FUN=sum)
colnames(SpendCast10.high)[2]<-"cost"
TotCost.high<-sum(SpendCast10.high$cost)
SpendCast10.high$share<-mapply(function(x)x/TotCost.high,SpendCast10.high$cost)
save(SpendCast10,SpendCast10.low,SpendCast10.high,file="SpendCast10.RData")
