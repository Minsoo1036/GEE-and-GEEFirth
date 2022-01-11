## Real data 발표용 GEE and PGEE ###

##### requirements ####
## github repository ##
#install.packages("remotes")
#remotes::install_github("mhmondol/geefirthr")
#install.packages("geepack")
library("geefirthr")
library("geepack")

################ Real Data Load Code ################

df=read.table("respir-data.txt")
head(df, 8)
#colnames(df)= c("id","hospital","treatment","time0","time1","time2","time3","time4")

### 환자 1명당 5개 데이터로 복제
V1= rep(df$V1,each=5)
V2= rep(df$V2,each=5)
V3= rep(df$V3,each=5)

# time부분 vector로 열거
response=c()
for(i in 1:111){
  response<-append(response,c(df[i,4],df[i,5],df[i,6],df[i,7],df[i,8]))
}
response

# df2 -> 5배로 증식된 데이터
df2=cbind(V1,V2,V3,response)
df2=as.data.frame(df2)
head(df2)
df2$V1 =as.numeric(df2$V1)
df2$V2 =as.numeric(df2$V2)
df2$V3 =as.numeric(df2$V3)
df2$V3 = 2-df2$V3 # P->0, A->1
df2$time=rep((0:4),111)
colnames(df2)= c("id","hospital","treatment","response","time")
head(df2)

### clinic 2의 샘플만 추출
df2_subset=subset(df2, hospital==2)
head(df2_subset)
#tail(df2_subset)

############# seed 설정 ##############
# 19, 24, 29, 37, 41, 43
# set.seed(43)
# ids=sample(57:111,15,replace=FALSE)

############# Scenario별 샘플링 ##############
# 실험 Scenario 1
ids1 = c(62, 63, 64, 68, 74, 75, 78, 80, 82,84 ,89, 91, 92, 96, 110)

# 실험 Scenario 2
# ids2 = c(59, 62, 63, 64, 65, 66, 67, 74, 78, 84, 86, 89, 91, 94, 102)

# 실험 Scenario 3
# ids3 = c(62, 64, 65, 66, 67, 72, 74, 78, 84, 86, 89, 91, 96, 102, 107)

# 실험 Scenario 4 -> df2그대로 사용

df2_subset2=subset(df2_subset, id %in% ids1)
head(df2_subset2)
table(df2_subset2$treatment, df2_subset2$response)

#-----------------------------------------------------------------------------------------
##### 병원에 따른 효과 확인을 위한 샘플링 #####
# Quasi-complete-Seperation
ids4 = c(2, 3, 5, 13, 23, 27, 28, 60, 61, 68, 74, 75, 77, 91, 110)

df2_subset2=subset(df2, id %in% ids4)
head(df2_subset2)

table(df2_subset2$treatment, df2_subset2$response)

# No-separation -> df2그대로 사용

#-----------------------------------------------------------------------------------------
#df2_subset2$tr=as.numeric(df2_subset2$tr)

### simulation code
PGEE.xch <- geefirth(response ~ hospital + treatment + time, id=id, data=df2, corstr = "exchangeable")
PGEE.xch
# PGEE.ar1 <- geefirth(response ~ treatment + time, id=id, data=df2_subset2, corstr = "ar1")

GEE.xch <- summary(geeglm(response ~ hospital + treatment + time, id=id, data=df2, family=binomial, corstr = "exchangeable"))
GEE.xch
# GEE.ar1 <- summary(geeglm(response ~ tr + time, id=id, data=df2_subset, family=binomial, corstr = "ar1"))

# 전체 데이터 hospital 포함
PGEE.xch_hp <- geefirth(y=df2$response, x=df2[, c(2,3,5)], id=df2$id, ar=F)
PGEE.xch_hp

GEE.xch_hp <- summary(geeglm(response ~ hospital + tr + time, id=id, data=df2, family=binomial, corstr = "exchangeable"))
GEE.xch_hp
