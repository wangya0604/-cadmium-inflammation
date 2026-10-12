#### cancer survivors####
library(foreign)
library(dplyr)

MCQ_1<-read.xport("MCQ.XPT")
str(MCQ_1)
write.csv(MCQ_1,file = "MCQ_1.csv")
MCQ_2<-MCQ_1[,c("SEQN","MCQ220","MCQ230A","MCQ053","MCQ160B","MCQ160C"
                ,"MCQ160D","MCQ160E","MCQ160F","MCQ240O","MCQ240L","MCQ240B")]
####1==: Example/key-step code ####
####2007-2008####
library(foreign)
library(dplyr)
MCQE_1<-read.xport("MCQ_E.XPT")
str(MCQE_1)
write.csv(MCQE_1,file = "MCQE_1.csv")
MCQE_2<-MCQE_1[,c("SEQN","MCQ220","MCQ230A")]
MCQE_2$year<-"2007-2008"
str(MCQE_2)
head(MCQE_2)

MCQE_3<-rename(.data=MCQE_2,
               ca=MCQ220,
               type=MCQ230A)
table(MCQE_3$ca,useNA = "ifany") 
MCQE_4 <- subset(MCQE_3,MCQE_3$ca==1)
write.csv(MCQE_4,file = "MCQE_4.csv")
####Variables####
####2007-2008####
####——CBC####
library(foreign)
library(dplyr)
CBC_E<-read.xport("CBC_E.XPT")
str(L25_B)
CBC_E_1<-CBC_E[,c("SEQN","LBXWBCSI","LBDLYMNO","LBDMONO","LBDNENO","LBXRBCSI",
                  "LBXHGB","LBXPLTSI")]

str(L25_B_2)
CBC_E_2<-rename(.data=CBC_E_1,
                WBC=LBXWBCSI,
                L=LBDLYMNO,
                M=LBDMONO,
                N=LBDNENO,
                RBC=LBXRBCSI,
                HGB=LBXHGB,
                PLT=LBXPLTSI,)
LABDATAE3<-merge(MCQE_4,CBC_E_2,by="SEQN", all.x = T)
####——Cd####
library(foreign)
library(dplyr)

PBCD_E<-read.xport("PBCD_E.XPT")
str(PBCD_E)
PBCD_E_1<-PBCD_E[,c("SEQN","LBDBPBSI","LBDBCDSI","LBDTHGSI")]

str(L06_B_1)
PBCD_E_2<-rename(.data=PBCD_E_1,
                 Pb=LBDBPBSI,
                 Cd=LBDBCDSI,
                 Hg=LBDTHGSI,)
LABDATAE4<-merge(LABDATAE3,PBCD_E_2,by="SEQN", all.x = T)
####——BMI####
library(foreign)
library(dplyr)

BMX_E<-read.xport("BMX_E.XPT")

BMX_E_1<-BMX_E[,c("SEQN","BMXBMI")]

BMX_E_2<-rename(.data=BMX_E_1,
                BMI=BMXBMI,)
LABDATAE5-merge(LABDATAE4,BMX_E_2,by="SEQN", all.x = T)
####——Smoke####
library(foreign)
library(dplyr)
SMQ_E<-read.xport("SMQ_E.XPT")

SMQ_E_1<-SMQ_E[,c("SEQN","SMQ020","SMQ040")]

SMQ_E_2<-rename(.data=SMQ_E_1,
                Smoke_history=SMQ020, 
                Smoke_now=SMQ040,)
table(SMQ_E_2$Smoke_history,useNA = "ifany") 
SMQ_E_2$Smoke_history <- ifelse(is.na(SMQ_E_2$Smoke_history)|SMQ_E_2$Smoke_history%in% c(7, 9),999,SMQ_E_2$Smoke_history)
table(SMQ_E_2$Smoke_history,useNA = "ifany") 
# Smoke_history
# 1	Yes
# 2	No
# 7	Refused	
# 9	Don't know
# .	Missing
table(SMQ_E_2$Smoke_now,useNA = "ifany") 

SMQ_E_2$Smoke_now <- ifelse(SMQ_E_2$Smoke_now %in% c(7, 9) | is.na(SMQ_E_2$Smoke_now), 999, SMQ_E_2$Smoke_now)
table(SMQ_E_2$Smoke_now,useNA = "ifany") 
# smoke_now
# 1	Every day
# 2	Some days
# 3	Not at all
# 7	Refused
# 9	Don't know
# .	Missing

SMQ_E_2$Smoke <- ifelse(SMQ_E_2$Smoke_history == 2, 0,  # Never
                        ifelse(SMQ_E_2$Smoke_history == 1 & SMQ_E_2$Smoke_now == 3, 1,  # Former
                               ifelse((SMQ_E_2$Smoke_history == 1 & SMQ_E_2$Smoke_now %in% c(1, 2)) |
                                        (SMQ_E_2$Smoke_history == 2 & SMQ_E_2$Smoke_now %in% c(1, 2)), 2, 999)))

LABDATAE6<-merge(LABDATAE5,SMQ_E_2,by="SEQN", all.x = T)


####——Alcohol####
library(foreign)
library(dplyr)
alq_e<-read.xport("alq_e.XPT")
alq_e <- alq_e[,c("SEQN","ALQ110","ALQ120Q")]
LABDATAE7<-merge(LABDATAE6,alq_e,by="SEQN", all.x = T)

#####sleepdis####
library(foreign)
library(dplyr)

DPQ_E<-read.xport("DPQ_E.XPT")

DPQ_E_1<-DPQ_E[,c("SEQN","DPQ030")]

DPQ_E_2<-rename(.data=DPQ_E_1,sleepdis=DPQ030)
DPQ_E_2<- DPQ_E_2 %>%
  mutate(across(everything(), ~ ifelse(. %in% c(7, 9, NA), NA, .))) # 将值为 "7"、"9" 或缺失值（NA）的数据替换为 NA，保留其他值。

DPQ_E_3 <- DPQ_E_2 %>%
  mutate(sleep_disorder = ifelse(sleepdis == 0, 0, 
                                 ifelse(sleepdis %in% c(1, 2, 3), 1, 999)))
#0为“0”Not at all，1为“1/2/3”有睡眠障碍
table(DPQ_E_3$sleep_disorder)      

LABDATAE8<-merge(LABDATAE7,DPQ_E_3,by="SEQN", all.x = T)


####——Hypertension####
library(foreign)
library(dplyr)

BPQ_E<-read.xport("BPQ_E.XPT")

BPQ_E_1<-BPQ_E[,c("SEQN","BPQ020")]

BPQ_E_2<-rename(.data=BPQ_E_1,hypter=BPQ020)

LABDATAE13<-merge(LABDATAE12,BPQ_E_2,by="SEQN", all.x = T)


library(foreign)
library(dplyr)

BPQ_1<-BPQ_E[,c("SEQN","BPQ020","BPQ040A","BPQ080","BPQ090D")]
BPQ_2<-rename(.data=BPQ_1,
              hypter=BPQ020,
              Taking_hypter=BPQ040A,
              high_cho=BPQ080,
              Taking_cho=BPQ090D,)
table(BPQ_2$hypter,useNA = "ifany") 
BPQ_2$hypter <- ifelse(BPQ_2$hypter %in% c(7, 9) | is.na(BPQ_2$hypter), 999, BPQ_2$hypter)
table(BPQ_2$hypter,useNA = "ifany") 

table(BPQ_2$Taking_hypter,useNA = "ifany") 
BPQ_2$Taking_hypter <- ifelse(BPQ_2$Taking_hypter %in% c(7, 9) | is.na(BPQ_2$Taking_hypter), 999, BPQ_2$Taking_hypter)
table(BPQ_2$Taking_hypter,useNA = "ifany") 

table(BPQ_2$high_cho,useNA = "ifany") 
BPQ_2$high_cho <- ifelse(BPQ_2$high_cho %in% c(7, 9) | is.na(BPQ_2$high_cho), 999, BPQ_2$high_cho)
table(BPQ_2$high_cho,useNA = "ifany") 

table(BPQ_2$Taking_cho,useNA = "ifany") 
BPQ_2$Taking_cho <- ifelse(BPQ_2$Taking_cho %in% c(7, 9) | is.na(BPQ_2$Taking_cho), 999, BPQ_2$Taking_cho)
table(BPQ_2$Taking_cho,useNA = "ifany") 
J2<-merge(LABDATAE7,BPQ_2,by="SEQN", all.x = T)

#血压测量
library(foreign)
library(dplyr)
bpx_e<-read.xport("bpx_e.XPT")
bpx_1<-bpx_e[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")]

####__________SBP ####
bpx_1$SBP1 <- ifelse(is.na(bpx_1$BPXSY1),0,bpx_1$BPXSY1)
bpx_1$SBP2 <- ifelse(is.na(bpx_1$BPXSY2),0,bpx_1$BPXSY2)
bpx_1$SBP3 <- ifelse(is.na(bpx_1$BPXSY3),0,bpx_1$BPXSY3)
bpx_1$SBP4 <- ifelse(is.na(bpx_1$BPXSY4),0,bpx_1$BPXSY4)
#
bpx_1$SBPn1 <- ifelse(is.na(bpx_1$BPXSY1),0,1)
bpx_1$SBPn2 <- ifelse(is.na(bpx_1$BPXSY2),0,1)
bpx_1$SBPn3 <- ifelse(is.na(bpx_1$BPXSY3),0,1)
bpx_1$SBPn4 <- ifelse(is.na(bpx_1$BPXSY4),0,1)
bpx_1$SBPn <- bpx_1$SBPn1 + bpx_1$SBPn2 + bpx_1$SBPn3 + bpx_1$SBPn4
#
bpx_1$SBP <- (bpx_1$SBP1 + bpx_1$SBP2 + bpx_1$SBP3 + bpx_1$SBP4)/bpx_1$SBPn
summary(bpx_1$SBP) #

sbp_temp <- subset(bpx_1,is.na(bpx_1$BPXSY1) & 
                     is.na(bpx_1$BPXSY2) & 
                     is.na(bpx_1$BPXSY3) & 
                     is.na(bpx_1$BPXSY4))
nrow(sbp_temp) #
#
bpx_1 <- subset(bpx_1, select=-c(SBP1,SBP2,SBP3,SBP4,
                                 BPXSY1,BPXSY2,BPXSY3,BPXSY4,
                                 SBPn1,SBPn2,SBPn3,SBPn4,SBPn
))

####__________DBP ####

#
bpx_1$DBP1 <- ifelse(is.na(bpx_1$BPXDI1),0,bpx_1$BPXDI1)
bpx_1$DBP2 <- ifelse(is.na(bpx_1$BPXDI2),0,bpx_1$BPXDI2)
bpx_1$DBP3 <- ifelse(is.na(bpx_1$BPXDI3),0,bpx_1$BPXDI3)
bpx_1$DBP4 <- ifelse(is.na(bpx_1$BPXDI4),0,bpx_1$BPXDI4)
#
bpx_1$DBPn1 <- ifelse(is.na(bpx_1$BPXDI1),0,1)
bpx_1$DBPn2 <- ifelse(is.na(bpx_1$BPXDI2),0,1)
bpx_1$DBPn3 <- ifelse(is.na(bpx_1$BPXDI3),0,1)
bpx_1$DBPn4 <- ifelse(is.na(bpx_1$BPXDI4),0,1)
bpx_1$DBPn <- bpx_1$DBPn1 + bpx_1$DBPn2 + bpx_1$DBPn3 + bpx_1$DBPn4
#
bpx_1$DBP <- (bpx_1$DBP1 + bpx_1$DBP2 + bpx_1$DBP3 + bpx_1$DBP4)/bpx_1$DBPn

summary(bpx_1$DBP) #

dbp_temp <- subset(bpx_1,is.na(bpx_1$BPXDI1) & 
                     is.na(bpx_1$BPXDI2) & 
                     is.na(bpx_1$BPXDI3) & 
                     is.na(bpx_1$BPXDI4))
nrow(dbp_temp) #
bpx_1 <- subset(bpx_1, 
                select=-c(DBP1,DBP2,DBP3,DBP4,
                          BPXDI1,BPXDI2,BPXDI3,BPXDI4,
                          DBPn1,DBPn2,DBPn3,DBPn4,DBPn
                ))

#

J2<-merge(J1,bpx_1,by="SEQN", all.x = T)


####__####
table(J2$hypter,useNA = "ifany")

summary(J2$SBP) #

summary(J2$DBP) #


J2$high_bp <- ifelse(J2$SBP>=140|J2$DBP>=90, 1, 0)

J2$Hypertension <- ifelse(J2$high_bp==1 | J2$hypter== 1| J2$Taking_hypter== 1, 1, 0)

table(J2$Hypertension ,useNA = "ifany")
#
write.csv(J2,file = "J2.csv")

J2$hyptersion <- factor(J2$hyptersion,levels = c(1,2),labels = c("Yes","No"))

####——Diabetes####
library(foreign)
library(dplyr)

diq_e<-read.xport("diq_e.XPT")
diq <- diq_e[,c("SEQN","DIQ010","DIQ050","DID070")]

diq_1 <- rename(.data=diq,Diabetes1=DIQ010,#
                TInsulin=DIQ050,  #
                Sugar=DID070) #
head(diq_1)

LABDATAE24<-merge(J2,diq_1,by="SEQN", all.x = T)

table(LABDATAE24$Diabetes1,useNA = "ifany")
LABDATAE24$Diabetes1 <- ifelse(is.na(LABDATAE24$Diabetes1) | LABDATAE24$Diabetes1 == 9| LABDATAE24$Diabetes1== 3, 999, LABDATAE24$Diabetes1)
table(LABDATAE24$Diabetes1,useNA = "ifany")
#  
table(LABDATAE24$TInsulin,useNA = "ifany")
LABDATAE24$TInsulin <- ifelse(is.na(LABDATAE24$TInsulin) ,
                              999,LABDATAH20$TInsulin)
table(LABDATAI23$TInsulin,useNA = "ifany")
#  
table(LABDATAE24$Sugar,useNA = "ifany") #
LABDATAE24$Sugar <- ifelse(is.na(LABDATAE24$Sugar) | LABDATAE24$Sugar == 9, 999, LABDATAE24$Sugar)
table(LABDATAE24$Sugar,useNA = "ifany")

table(LABDATAE24$GlyHb,useNA = "ifany")

table(LABDATAE24$Glu,useNA = "ifany")
LABDATAE24$diabetes <- ifelse(LABDATAE24$Diabetes1==1 |
                                LABDATAE24$TInsulin==1 | 
                                LABDATAE24$Sugar==1|
                                LABDATAE24$GlyHb>=6.5|
                                LABDATAE24$Glu>=7,1,2)
table(LABDATAE24$diabetes,useNA = "ifany")
write.csv(LABDATAE24,file = "LABDATAE24.csv")


####2==: Analysis code####
#### imputation####
library(foreign)
library(dplyr)
library(survey)
library(arsenal)
library(mitools)
library(mice)
CBC <- read.csv(
  "C:/Users/王娅/Desktop/镉与炎症指标投稿26.3.2/2026.10.8返修/2505插补前.csv",
  header = TRUE,        
  na.strings = c("NA")  
)

study_design <- svydesign(data=CBC, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~WT, nest=TRUE)

summary(CBC)

CBC <- CBC %>%
  mutate(
    Smoke = as.factor(Smoke),
    Alcohol = as.factor(Alcohol),
    sleep_disorder = as.factor(sleep_disorder),
    Marital = as.factor(Marital),
  )
summary(CBC)
continuous_vars <- c("PIR","BMI")
ordinal_vars <- c( "Smoke", "Alcohol","Marital")
non_imputed_vars <- c("SDMVSTRA", "SDMVPSU","WT")
binary_vars <- c("sleep_disorder") # 
summary(CBC)
md.pattern(CBC)
methods <- make.method(CBC)
methods[continuous_vars] <- "pmm"
methods[non_imputed_vars] <- ""
methods[binary_vars] <- "logreg"      
methods[ordinal_vars] <- "polr"        

pred_matrix <- make.predictorMatrix(CBC)
pred_matrix[, non_imputed_vars] <- 0 

imputed_data <- mice(
  CBC,
  method = methods,
  predictorMatrix = pred_matrix,
  m = 5,
  maxit = 10,
  seed = 123
)

complete_CBC <- complete(imputed_data, action = "all")

temp_dir <- tempdir()


for (i in 1:5) {
  write.csv(
    complete_CBC[[i]],
    file.path(temp_dir, paste0("imputed_CBC_", i, ".csv")),
    row.names = FALSE
  )
}


zip("imputed_CBC.zip", files = list.files(temp_dir, full.names = TRUE))




####DATA####

library(foreign)
library(dplyr)

DATA <- read.csv(
  "C:/Users/王娅/Desktop/镉与炎症指标投稿26.3.2/2026.10.8返修/DATA.csv")
library(survey)
library(arsenal)

options(survey.lonely.psu = "adjust")
study_design <- svydesign(data=DATA, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~WT, nest=TRUE)
####Cd_median####
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value
####Cd_median####
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value

####Cd_median=3.11####
Cd_median_value <- 3.11

DATA$Cd_binary <- ifelse(
  DATA$Cd < Cd_median_value,
  "Low",
  "High"
)

DATA$Cd_binary <- factor(
  DATA$Cd_binary,
  levels = c("Low", "High")
)
# quantile
Cd_Q1_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd < 3.11),
  quantiles = 0.50,
  na.rm = TRUE
)

# 
Cd_Q3_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd >= 3.11),
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_Q1_new <- as.numeric(coef(Cd_Q1_new))
Cd_Q3_new <- as.numeric(coef(Cd_Q3_new))

Cd_Q1_new
Cd_Q3_new
DATA$Cd_quartile <- cut(
  DATA$Cd,
  breaks = c(
    -Inf,
    Cd_Q1_new,
    3.11,
    Cd_Q3_new,
    Inf
  ),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,
  right = FALSE
)
table(DATA$Cd_binary, DATA$Cd_quartile, useNA = "ifany")
#### PIV_median#### 
PIV_median <- svyquantile(
  ~PIV,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

PIV_median_value <- coef(PIV_median)
PIV_median_value
####PIV_median=276 ####
PIV_median_value <- 276

DATA$PIV_binary <- ifelse(
  DATA$PIV < PIV_median_value,
  "Low",
  "High"
)

DATA$PIV_binary <- factor(
  DATA$PIV_binary,
  levels = c("Low", "High")
)
table(DATA$PIV_binary, useNA = "ifany")

#### SII_median#### 
SII_median <- svyquantile(
  ~SII,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

SII_median_value <- coef(SII_median)
SII_median_value
####SII_median=503.625  ####
SII_median_value <- 503.625 

DATA$SII_binary <- ifelse(
  DATA$SII < SII_median_value,
  "Low",
  "High"
)

DATA$SII_binary <- factor(
  DATA$SII_binary,
  levels = c("Low", "High")
)
table(DATA$SII_binary, useNA = "ifany")

#### NLR_median#### 
NLR_median <- svyquantile(
  ~NLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

NLR_median_value <- coef(NLR_median)
NLR_median_value
####NLR_median=2.2 ####
NLR_median_value <- 2.2

DATA$NLR_binary <- ifelse(
  DATA$NLR < NLR_median_value,
  "Low",
  "High"
)

DATA$NLR_binary <- factor(
  DATA$NLR_binary,
  levels = c("Low", "High")
)
table(DATA$NLR_binary, useNA = "ifany")
#### MLR_median#### 
MLR_median <- svyquantile(
  ~MLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

MLR_median_value <- coef(MLR_median)
MLR_median_value
####MLR_median=0.3 ####
MLR_median_value <- 0.3

DATA$MLR_binary <- ifelse(
  DATA$MLR < MLR_median_value,
  "Low",
  "High"
)

DATA$MLR_binary <- factor(
  DATA$MLR_binary,
  levels = c("Low", "High")
)
table(DATA$MLR_binary, useNA = "ifany")

#### PLR_median#### 
PLR_median <- svyquantile(
  ~PLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

PLR_median_value <- coef(PLR_median)
PLR_median_value
####PLR_median=126 ####
PLR_median_value <- 126

DATA$PLR_binary <- ifelse(
  DATA$PLR < PLR_median_value,
  "Low",
  "High"
)

DATA$PLR_binary <- factor(
  DATA$PLR_binary,
  levels = c("Low", "High")
)
table(DATA$PLR_binary, useNA = "ifany")
#### SIRI_median#### 
SIRI_median <- svyquantile(
  ~SIRI,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

SIRI_median_value <- coef(SIRI_median)
SIRI_median_value
####SIRI_median=1.2####
SIRI_median_value <- 1.2

DATA$SIRI_binary <- ifelse(
  DATA$SIRI < SIRI_median_value,
  "Low",
  "High"
)

DATA$SIRI_binary <- factor(
  DATA$SIRI_binary,
  levels = c("Low", "High")
)
table(DATA$SIRI_binary, useNA = "ifany")
write.csv(DATA,file = "DATA.csv")
####Table 2####

library(survey)
library(survival)
library(dplyr)

# 1. 六个暴露指标
indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)

# 2. Model 2 协变量
covars_model2 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1"
)

# 3. Model 3 协变量
covars_model3 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1",
  "BMI1",
  "Smoke",
  "Alcohol",
  "sleepdisorder",
  "Diabetes",
  "Hypertension"
)

# 4. 结果提取函数
extract_cox <- function(fit, indicator, model_name) {
  
  coef_table <- summary(fit)$coefficients
  
  # 兼容不同版本返回的标准误列名
  se_col <- intersect(
    c("se(coef)", "SE", "se"),
    colnames(coef_table)
  )
  
  if (length(se_col) == 0) {
    stop(
      paste(
        "未找到标准误列，实际列名：",
        paste(colnames(coef_table), collapse = ", ")
      )
    )
  }
  
  # 提取该暴露指标的全部系数
  idx <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  if (length(idx) == 0) {
    warning(
      paste(indicator, model_name, "未找到对应系数")
    )
    return(data.frame())
  }
  
  tab <- coef_table[idx, , drop = FALSE]
  
  beta <- tab[, "coef"]
  se <- tab[, se_col[1]]
  
  data.frame(
    Model = model_name,
    Indicator = indicator,
    Variable = rownames(tab),
    HR = exp(beta),
    Lower95 = exp(beta - 1.96 * se),
    Upper95 = exp(beta + 1.96 * se),
    P = tab[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
}

# 5. 循环拟合全部 18 个模型
all_results <- list()
model_errors <- list()

for (v in indicators) {
  
  # Model 1：未调整
  formula1 <- as.formula(
    paste("Surv(permthint, mortstat) ~", v)
  )
  
  # Model 2：调整人口学及社会经济学变量
  formula2 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      paste(c(v, covars_model2), collapse = " + ")
    )
  )
  
  # Model 3：进一步调整生活方式及合并症
  formula3 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      paste(c(v, covars_model3), collapse = " + ")
    )
  )
  
  formulas <- list(
    "Model 1" = formula1,
    "Model 2" = formula2,
    "Model 3" = formula3
  )
  
  for (m in names(formulas)) {
    
    result_name <- paste(v, m, sep = "_")
    
    tryCatch({
      
      fit <- svycoxph(
        formula = formulas[[m]],
        design = study_design
      )
      
      all_results[[result_name]] <- extract_cox(
        fit = fit,
        indicator = v,
        model_name = m
      )
      
    }, error = function(e) {
      
      model_errors[[result_name]] <<- conditionMessage(e)
      
      message(
        "模型失败：", result_name,
        "；原因：", conditionMessage(e)
      )
    })
  }
}

# 6. 合并结果

# 6. Combine results
results_all <- dplyr::bind_rows(all_results)

# Reset the NA printing option
options(na.print = "NA")

# Print all results
if (nrow(results_all) > 0) {
  print(as.data.frame(results_all), row.names = FALSE)
} else {
  message("No valid results were extracted.")
}

# 检查 na.print 当前设置
getOption("na.print")

# 恢复默认设置
options(na.print = "NA")

# 7. 整理并格式化
if (nrow(results_all) > 0) {
  
  results_all <- results_all %>%
    mutate(
      # HR 和 95% CI 保留两位小数
      HR_CI = sprintf(
        "%.2f (%.2f–%.2f)",
        HR, Lower95, Upper95
      ),
      
      # P 值保留三位小数
      P_value = case_when(
        is.na(P) ~ NA_character_,
        P < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", P)
      )
    ) %>%
    arrange(
      match(Indicator, indicators),
      match(Model, c("Model 1", "Model 2", "Model 3"))
    ) %>%
    select(
      Model,
      Indicator,
      Variable,
      HR,
      Lower95,
      Upper95,
      P,
      HR_CI,
      P_value
    )
  
  # 输出全部结果
  print(as.data.frame(results_all), row.names = FALSE)
  
  # 导出完整结果
  write.csv(
    results_all,
    "Cd_inflammation_Cox_All_Results.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  # 输出论文展示用简表
  results_table <- results_all %>%
    select(
      Model,
      Indicator,
      Variable,
      HR_CI,
      P_value
    )
  
  print(as.data.frame(results_table), row.names = FALSE)
  
  # 导出论文展示用简表
  write.csv(
    results_table,
    "Cd_inflammation_Cox_Publication_Table.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
} else {
  
  message("没有成功提取的结果，请检查模型错误信息。")
}


####Table2_final-output####
Model Indicator     Variable            HR_CI P_value
Model 1     CdPIV  CdPIVGroup2 1.68 (1.21–2.32)  <0.001
Model 1     CdPIV  CdPIVGroup3 1.95 (1.43–2.65)  <0.001
Model 1     CdPIV  CdPIVGroup4 2.68 (2.01–3.59)  <0.001
Model 2     CdPIV  CdPIVGroup2 1.62 (1.17–2.24)   0.001
Model 2     CdPIV  CdPIVGroup3 1.80 (1.32–2.45)  <0.001
Model 2     CdPIV  CdPIVGroup4 2.20 (1.64–2.94)  <0.001
Model 3     CdPIV  CdPIVGroup2 1.45 (1.05–2.01)   0.014
Model 3     CdPIV  CdPIVGroup3 1.76 (1.28–2.43)  <0.001
Model 3     CdPIV  CdPIVGroup4 2.09 (1.53–2.84)  <0.001
Model 1     CdPLR  CdPLRGroup2 1.45 (1.05–2.00)   0.024
Model 1     CdPLR  CdPLRGroup3 1.91 (1.41–2.59)  <0.001
Model 1     CdPLR  CdPLRGroup4 2.47 (1.84–3.33)  <0.001
Model 2     CdPLR  CdPLRGroup2 1.46 (1.06–2.02)   0.018
Model 2     CdPLR  CdPLRGroup3 1.77 (1.30–2.41)  <0.001
Model 2     CdPLR  CdPLRGroup4 2.08 (1.54–2.80)  <0.001
Model 3     CdPLR  CdPLRGroup2 1.38 (1.00–1.92)   0.057
Model 3     CdPLR  CdPLRGroup3 1.76 (1.28–2.42)  <0.001
Model 3     CdPLR  CdPLRGroup4 2.02 (1.48–2.77)  <0.001
Model 1     CdMLR  CdMLRGroup2 3.27 (2.29–4.66)  <0.001
Model 1     CdMLR  CdMLRGroup3 2.22 (1.55–3.19)  <0.001
Model 1     CdMLR  CdMLRGroup4 5.12 (3.67–7.15)  <0.001
Model 2     CdMLR  CdMLRGroup2 2.92 (2.04–4.16)  <0.001
Model 2     CdMLR  CdMLRGroup3 2.10 (1.46–3.02)  <0.001
Model 2     CdMLR  CdMLRGroup4 3.65 (2.61–5.09)  <0.001
Model 3     CdMLR  CdMLRGroup2 2.75 (1.92–3.93)  <0.001
Model 3     CdMLR  CdMLRGroup3 2.11 (1.45–3.08)  <0.001
Model 3     CdMLR  CdMLRGroup4 3.51 (2.48–4.96)  <0.001
Model 1     CdNLR  CdNLRGroup2 1.96 (1.40–2.73)   0.002
Model 1     CdNLR  CdNLRGroup3 1.91 (1.37–2.65)   0.001
Model 1     CdNLR  CdNLRGroup4 3.37 (2.48–4.59)  <0.001
Model 2     CdNLR  CdNLRGroup2 1.74 (1.25–2.43)   0.007
Model 2     CdNLR  CdNLRGroup3 1.71 (1.23–2.39)   0.009
Model 2     CdNLR  CdNLRGroup4 2.56 (1.87–3.48)  <0.001
Model 3     CdNLR  CdNLRGroup2 1.54 (1.10–2.16)   0.035
Model 3     CdNLR  CdNLRGroup3 1.68 (1.20–2.37)   0.014
Model 3     CdNLR  CdNLRGroup4 2.37 (1.71–3.27)  <0.001
Model 1     CdSII  CdSIIGroup2 1.25 (0.91–1.72)   0.192
Model 1     CdSII  CdSIIGroup3 1.68 (1.25–2.27)   0.002
Model 1     CdSII  CdSIIGroup4 2.30 (1.73–3.05)  <0.001
Model 2     CdSII  CdSIIGroup2 1.21 (0.88–1.67)   0.246
Model 2     CdSII  CdSIIGroup3 1.53 (1.13–2.06)   0.015
Model 2     CdSII  CdSIIGroup4 1.91 (1.43–2.54)  <0.001
Model 3     CdSII  CdSIIGroup2 1.08 (0.78–1.49)   0.642
Model 3     CdSII  CdSIIGroup3 1.49 (1.09–2.03)   0.029
Model 3     CdSII  CdSIIGroup4 1.80 (1.33–2.43)   0.002
Model 1    CdSIRI CdSIRIGroup2 2.46 (1.77–3.43)  <0.001
Model 1    CdSIRI CdSIRIGroup3 1.96 (1.41–2.73)  <0.001
Model 1    CdSIRI CdSIRIGroup4 3.81 (2.81–5.15)  <0.001
Model 2    CdSIRI CdSIRIGroup2 2.20 (1.57–3.07)  <0.001
Model 2    CdSIRI CdSIRIGroup3 1.80 (1.29–2.52)  <0.001
Model 2    CdSIRI CdSIRIGroup4 2.87 (2.11–3.89)  <0.001
Model 3    CdSIRI CdSIRIGroup2 2.05 (1.47–2.87)  <0.001
Model 3    CdSIRI CdSIRIGroup3 1.86 (1.32–2.62)   0.001
Model 3    CdSIRI CdSIRIGroup4 2.75 (2.00–3.78)  <0.001

#8. 计算各指标组别的未加权 Deaths/Participants 

# 使用 survey design 中实际纳入的数据
DATA_count <- study_design$variables

# 各模型对应的协变量
covars_by_model <- list(
  "Model 1" = character(0),
  "Model 2" = covars_model2,
  "Model 3" = covars_model3
)

death_results <- list()

for (v in indicators) {
  
  for (m in names(covars_by_model)) {
    
    # 当前模型所需的全部变量
    required_vars <- c(
      "permthint",
      "mortstat",
      v,
      covars_by_model[[m]]
    )
    
    # 按当前模型的完整病例标准筛选
    dat <- DATA_count[
      complete.cases(DATA_count[, required_vars, drop = FALSE]),
      ,
      drop = FALSE
    ]
    
    # 排除无效的生存时间和结局编码
    dat <- dat[
      !is.na(dat$permthint) &
        dat$permthint > 0 &
        dat$mortstat %in% c(0, 1),
      ,
      drop = FALSE
    ]
    
    # 按指标组别统计实际人数和死亡人数
    if (nrow(dat) > 0) {
      
      tab <- dat %>%
        mutate(
          Group = as.character(.data[[v]])
        ) %>%
        filter(!is.na(Group)) %>%
        group_by(Group) %>%
        summarise(
          Participants = n(),
          Deaths = sum(mortstat == 1),
          .groups = "drop"
        ) %>%
        mutate(
          Death_percent = round(
            Deaths / Participants * 100, 1
          ),
          `Deaths/Participants` = paste0(
            Deaths, "/", Participants
          ),
          Indicator = v,
          Model = m
        ) %>%
        select(
          Indicator,
          Model,
          Group,
          Participants,
          Deaths,
          Death_percent,
          `Deaths/Participants`
        )
      
      death_results[[paste(v, m, sep = "_")]] <- tab
    }
  }
}

# 合并所有指标和模型的统计结果
death_table_all <- bind_rows(death_results)

# 按指标及模型顺序排列
if (nrow(death_table_all) > 0) {
  
  death_table_all <- death_table_all %>%
    mutate(
      Indicator = factor(
        Indicator,
        levels = indicators
      ),
      Model = factor(
        Model,
        levels = c("Model 1", "Model 2", "Model 3")
      )
    ) %>%
    arrange(Indicator, Model, Group) %>%
    mutate(
      Indicator = as.character(Indicator),
      Model = as.character(Model)
    )
  
  # 查看所有结果
  print(
    as.data.frame(death_table_all),
    row.names = FALSE
  )
  
  # 导出 CSV 文件
  write.csv(
    death_table_all,
    "Table2_Unweighted_Deaths_Participants.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
} else {
  message("未生成死亡人数/参与人数统计结果，请检查指标分组及数据。")
}

Indicator   Model  Group Participants Deaths Death_percent Deaths/Participants
CdPIV Model 1 Group1          574     81          14.1              81/574
CdPIV Model 1 Group2          541    114          21.1             114/541
CdPIV Model 1 Group3          660    175          26.5             175/660
CdPIV Model 1 Group4          729    249          34.2             249/729
CdPIV Model 2 Group1          574     81          14.1              81/574
CdPIV Model 2 Group2          541    114          21.1             114/541
CdPIV Model 2 Group3          660    175          26.5             175/660
CdPIV Model 2 Group4          729    249          34.2             249/729
CdPIV Model 3 Group1          574     81          14.1              81/574
CdPIV Model 3 Group2          541    114          21.1             114/541
CdPIV Model 3 Group3          660    175          26.5             175/660
CdPIV Model 3 Group4          729    249          34.2             249/729
CdPLR Model 1 Group1          557     85          15.3              85/557
CdPLR Model 1 Group2          558    110          19.7             110/558
CdPLR Model 1 Group3          718    192          26.7             192/718
CdPLR Model 1 Group4          671    232          34.6             232/671
CdPLR Model 2 Group1          557     85          15.3              85/557
CdPLR Model 2 Group2          558    110          19.7             110/558
CdPLR Model 2 Group3          718    192          26.7             192/718
CdPLR Model 2 Group4          671    232          34.6             232/671
CdPLR Model 3 Group1          557     85          15.3              85/557
CdPLR Model 3 Group2          558    110          19.7             110/558
CdPLR Model 3 Group3          718    192          26.7             192/718
CdPLR Model 3 Group4          671    232          34.6             232/671
CdMLR Model 1 Group1          567     67          11.8              67/567
CdMLR Model 1 Group2          548    128          23.4             128/548
CdMLR Model 1 Group3          676    150          22.2             150/676
CdMLR Model 1 Group4          713    274          38.4             274/713
CdMLR Model 2 Group1          567     67          11.8              67/567
CdMLR Model 2 Group2          548    128          23.4             128/548
CdMLR Model 2 Group3          676    150          22.2             150/676
CdMLR Model 2 Group4          713    274          38.4             274/713
CdMLR Model 3 Group1          567     67          11.8              67/567
CdMLR Model 3 Group2          548    128          23.4             128/548
CdMLR Model 3 Group3          676    150          22.2             150/676
CdMLR Model 3 Group4          713    274          38.4             274/713
CdNLR Model 1 Group1          551     72          13.1              72/551
CdNLR Model 1 Group2          564    123          21.8             123/564
CdNLR Model 1 Group3          665    161          24.2             161/665
CdNLR Model 1 Group4          724    263          36.3             263/724
CdNLR Model 2 Group1          551     72          13.1              72/551
CdNLR Model 2 Group2          564    123          21.8             123/564
CdNLR Model 2 Group3          665    161          24.2             161/665
CdNLR Model 2 Group4          724    263          36.3             263/724
CdNLR Model 3 Group1          551     72          13.1              72/551
CdNLR Model 3 Group2          564    123          21.8             123/564
CdNLR Model 3 Group3          665    161          24.2             161/665
CdNLR Model 3 Group4          724    263          36.3             263/724
CdSII Model 1 Group1          554     85          15.3              85/554
CdSII Model 1 Group2          561    110          19.6             110/561
CdSII Model 1 Group3          683    180          26.4             180/683
CdSII Model 1 Group4          706    244          34.6             244/706
CdSII Model 2 Group1          554     85          15.3              85/554
CdSII Model 2 Group2          561    110          19.6             110/561
CdSII Model 2 Group3          683    180          26.4             180/683
CdSII Model 2 Group4          706    244          34.6             244/706
CdSII Model 3 Group1          554     85          15.3              85/554
CdSII Model 3 Group2          561    110          19.6             110/561
CdSII Model 3 Group3          683    180          26.4             180/683
CdSII Model 3 Group4          706    244          34.6             244/706
CdSIRI Model 1 Group1          574     74          12.9              74/574
CdSIRI Model 1 Group2          541    121          22.4             121/541
CdSIRI Model 1 Group3          638    150          23.5             150/638
CdSIRI Model 1 Group4          751    274          36.5             274/751
CdSIRI Model 2 Group1          574     74          12.9              74/574
CdSIRI Model 2 Group2          541    121          22.4             121/541
CdSIRI Model 2 Group3          638    150          23.5             150/638
CdSIRI Model 2 Group4          751    274          36.5             274/751
CdSIRI Model 3 Group1          574     74          12.9              74/574
CdSIRI Model 3 Group2          541    121          22.4             121/541
CdSIRI Model 3 Group3          638    150          23.5             150/638
CdSIRI Model 3 Group4          751    274          36.5             274/751

####Schoenfeld test####
library(survival)
library(dplyr)

indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)

ph_test_indicator <- function(indicator) {
  
  # Model 1

  
  formula1 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator
    )
  )
  
  cox1 <- coxph(
    formula1,
    data = DATA
  )
  
  ph1 <- cox.zph(cox1)
  
  # Model 2
  
  formula2 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
  
  cox2 <- coxph(
    formula2,
    data = DATA
  )
  
  ph2 <- cox.zph(cox2)
  
  # Model 3
  
  formula3 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
  
  cox3 <- coxph(
    formula3,
    data = DATA
  )
  
  ph3 <- cox.zph(cox3)
  
  
  
  list(
    Model1 = ph1,
    Model2 = ph2,
    Model3 = ph3
  )
}


extract_global_ph <- function(ph_object) {
  
  tab <- ph_object$table
  
  global_row <- which(
    rownames(tab) == "GLOBAL"
  )
  
  if (length(global_row) == 0) {
    return(NA_real_)
  }
  
  tab[global_row, "p"]
}


PH_summary <- data.frame()

for (indicator in indicators) {
  
  ph <- ph_test_indicator(indicator)
  
  temp <- data.frame(
    Indicator = indicator,
    Model1_Global_P = extract_global_ph(ph$Model1),
    Model2_Global_P = extract_global_ph(ph$Model2),
    Model3_Global_P = extract_global_ph(ph$Model3)
  )
  
  PH_summary <- bind_rows(
    PH_summary,
    temp
  )
}


PH_summary
#### Schoenfeld test-outputt####
      Indicator Model1_Global_P Model2_Global_P Model3_Global_P
1     CdPIV       0.9260240       0.3866538       0.4881464
2     CdPLR       0.3985103       0.1939092       0.2873912
3     CdMLR       0.6114776       0.2830305       0.3780450
4     CdNLR       0.7079107       0.2936841       0.4060529
5     CdSII       0.7662614       0.3199650       0.4169861
6    CdSIRI       0.7193682       0.2797111       0.4289965


####Table S1####
# Table S1
# Outcome: CVD
# Survey-weighted Cox regression#

library(survey)
library(survival)
library(dplyr)

# 1. 六个暴露指标
indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)

# 2. Model 2 协变量
covars_model2 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1"
)

# 3. Model 3 协变量
covars_model3 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1",
  "BMI1",
  "Smoke",
  "Alcohol",
  "sleepdisorder",
  "Diabetes",
  "Hypertension"
)

# 4. 结果提取函数
extract_cox <- function(fit, indicator, model_name) {
  
  coef_table <- summary(fit)$coefficients
  
  # 兼容不同版本返回的标准误列名
  se_col <- intersect(
    c("se(coef)", "SE", "se"),
    colnames(coef_table)
  )
  
  if (length(se_col) == 0) {
    stop(
      paste(
        "未找到标准误列，实际列名：",
        paste(colnames(coef_table), collapse = ", ")
      )
    )
  }
  
  # 提取该暴露指标的全部系数
  idx <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  if (length(idx) == 0) {
    warning(
      paste(indicator, model_name, "未找到对应系数")
    )
    return(data.frame())
  }
  
  tab <- coef_table[idx, , drop = FALSE]
  
  beta <- tab[, "coef"]
  se <- tab[, se_col[1]]
  
  data.frame(
    Model = model_name,
    Indicator = indicator,
    Variable = rownames(tab),
    HR = exp(beta),
    Lower95 = exp(beta - 1.96 * se),
    Upper95 = exp(beta + 1.96 * se),
    P = tab[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
}

# 5. 循环拟合全部 18 个模型
all_results <- list()
model_errors <- list()

for (v in indicators) {
  
  # Model 1：未调整
  formula1 <- as.formula(
    paste("Surv(permthint, CVD) ~", v)
  )
  
  # Model 2：调整人口学及社会经济学变量
  formula2 <- as.formula(
    paste(
      "Surv(permthint, CVD) ~",
      paste(c(v, covars_model2), collapse = " + ")
    )
  )
  
  # Model 3：进一步调整生活方式及合并症
  formula3 <- as.formula(
    paste(
      "Surv(permthint, CVD) ~",
      paste(c(v, covars_model3), collapse = " + ")
    )
  )
  
  formulas <- list(
    "Model 1" = formula1,
    "Model 2" = formula2,
    "Model 3" = formula3
  )
  
  for (m in names(formulas)) {
    
    result_name <- paste(v, m, sep = "_")
    
    tryCatch({
      
      fit <- svycoxph(
        formula = formulas[[m]],
        design = study_design
      )
      
      all_results[[result_name]] <- extract_cox(
        fit = fit,
        indicator = v,
        model_name = m
      )
      
    }, error = function(e) {
      
      model_errors[[result_name]] <<- conditionMessage(e)
      
      message(
        "模型失败：", result_name,
        "；原因：", conditionMessage(e)
      )
    })
  }
}

# 6. 合并结果

# 6. Combine results
results_all <- dplyr::bind_rows(all_results)

# Reset the NA printing option
options(na.print = "NA")

# Print all results
if (nrow(results_all) > 0) {
  print(as.data.frame(results_all), row.names = FALSE)
} else {
  message("No valid results were extracted.")
}

# 检查 na.print 当前设置
getOption("na.print")

# 恢复默认设置
options(na.print = "NA")

# 7. 整理并格式化
if (nrow(results_all) > 0) {
  
  results_all <- results_all %>%
    mutate(
      # HR 和 95% CI 保留两位小数
      HR_CI = sprintf(
        "%.2f (%.2f–%.2f)",
        HR, Lower95, Upper95
      ),
      
      # P 值保留三位小数
      P_value = case_when(
        is.na(P) ~ NA_character_,
        P < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", P)
      )
    ) %>%
    arrange(
      match(Indicator, indicators),
      match(Model, c("Model 1", "Model 2", "Model 3"))
    ) %>%
    select(
      Model,
      Indicator,
      Variable,
      HR,
      Lower95,
      Upper95,
      P,
      HR_CI,
      P_value
    )
  
  # 输出全部结果
  print(as.data.frame(results_all), row.names = FALSE)
  
  # 导出完整结果
  write.csv(
    results_all,
    "Cd_CVDCox_All_Results1.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  # 输出论文展示用简表
  results_table <- results_all %>%
    select(
      Model,
      Indicator,
      Variable,
      HR_CI,
      P_value
    )
  
  print(as.data.frame(results_table), row.names = FALSE)
  
  # 导出论文展示用简表
  write.csv(
    results_table,
    "Cd_CVD_Publication_Table1.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
} else {
  
  message("没有成功提取的结果，请检查模型错误信息。")
}

#### TableS1-output####
Model Indicator     Variable             HR_CI P_value
Model 1     CdPIV  CdPIVGroup2  3.54 (1.72–7.27)   0.011
Model 1     CdPIV  CdPIVGroup3  3.13 (1.52–6.45)   0.012
Model 1     CdPIV  CdPIVGroup4  4.34 (2.17–8.67)   0.001
Model 2     CdPIV  CdPIVGroup2  3.35 (1.63–6.90)   0.016
Model 2     CdPIV  CdPIVGroup3  2.96 (1.43–6.12)   0.016
Model 2     CdPIV  CdPIVGroup4  3.55 (1.77–7.12)   0.005
Model 3     CdPIV  CdPIVGroup2  2.84 (1.37–5.89)   0.043
Model 3     CdPIV  CdPIVGroup3  3.15 (1.50–6.59)   0.023
Model 3     CdPIV  CdPIVGroup4  3.74 (1.82–7.67)   0.011
Model 1     CdPLR  CdPLRGroup2  1.33 (0.71–2.49)   0.403
Model 1     CdPLR  CdPLRGroup3  1.59 (0.88–2.90)   0.138
Model 1     CdPLR  CdPLRGroup4  2.47 (1.40–4.36)   0.002
Model 2     CdPLR  CdPLRGroup2  1.31 (0.70–2.46)   0.435
Model 2     CdPLR  CdPLRGroup3  1.49 (0.82–2.73)   0.200
Model 2     CdPLR  CdPLRGroup4  2.11 (1.19–3.73)   0.014
Model 3     CdPLR  CdPLRGroup2  1.27 (0.67–2.41)   0.473
Model 3     CdPLR  CdPLRGroup3  1.71 (0.92–3.18)   0.122
Model 3     CdPLR  CdPLRGroup4  2.35 (1.29–4.29)   0.013
Model 1     CdMLR  CdMLRGroup2 5.52 (2.49–12.24)  <0.001
Model 1     CdMLR  CdMLRGroup3  3.11 (1.37–7.08)  <0.001
Model 1     CdMLR  CdMLRGroup4 7.58 (3.51–16.39)  <0.001
Model 2     CdMLR  CdMLRGroup2 4.72 (2.12–10.51)  <0.001
Model 2     CdMLR  CdMLRGroup3  3.00 (1.32–6.85)  <0.001
Model 2     CdMLR  CdMLRGroup4 5.28 (2.44–11.43)  <0.001
Model 3     CdMLR  CdMLRGroup2  4.31 (1.93–9.63)  <0.001
Model 3     CdMLR  CdMLRGroup3  3.43 (1.47–7.98)  <0.001
Model 3     CdMLR  CdMLRGroup4 5.60 (2.54–12.32)  <0.001
Model 1     CdNLR  CdNLRGroup2  2.68 (1.34–5.37)   0.031
Model 1     CdNLR  CdNLRGroup3  1.89 (0.92–3.87)   0.128
Model 1     CdNLR  CdNLRGroup4  4.56 (2.38–8.77)  <0.001
Model 2     CdNLR  CdNLRGroup2  2.34 (1.17–4.70)   0.065
Model 2     CdNLR  CdNLRGroup3  1.77 (0.86–3.64)   0.167
Model 2     CdNLR  CdNLRGroup4  3.37 (1.75–6.49)   0.004
Model 3     CdNLR  CdNLRGroup2  1.97 (0.98–3.96)   0.140
Model 3     CdNLR  CdNLRGroup3  1.93 (0.92–4.05)   0.147
Model 3     CdNLR  CdNLRGroup4  3.41 (1.73–6.71)   0.010
Model 1     CdSII  CdSIIGroup2  1.94 (1.00–3.75)   0.079
Model 1     CdSII  CdSIIGroup3  2.07 (1.08–3.95)   0.054
Model 1     CdSII  CdSIIGroup4  3.02 (1.63–5.57)   0.002
Model 2     CdSII  CdSIIGroup2  1.83 (0.94–3.54)   0.113
Model 2     CdSII  CdSIIGroup3  1.92 (1.00–3.69)   0.074
Model 2     CdSII  CdSIIGroup4  2.48 (1.34–4.60)   0.012
Model 3     CdSII  CdSIIGroup2  1.57 (0.81–3.04)   0.249
Model 3     CdSII  CdSIIGroup3  2.05 (1.06–3.99)   0.076
Model 3     CdSII  CdSIIGroup4  2.59 (1.36–4.93)   0.026
Model 1    CdSIRI CdSIRIGroup2  3.41 (1.71–6.79)   0.007
Model 1    CdSIRI CdSIRIGroup3  2.29 (1.13–4.64)   0.039
Model 1    CdSIRI CdSIRIGroup4  4.71 (2.47–8.99)  <0.001
Model 2    CdSIRI CdSIRIGroup2  2.90 (1.45–5.80)   0.020
Model 2    CdSIRI CdSIRIGroup3  2.18 (1.07–4.44)   0.054
Model 2    CdSIRI CdSIRIGroup4  3.45 (1.80–6.60)   0.002
Model 3    CdSIRI CdSIRIGroup2  2.59 (1.29–5.19)   0.051
Model 3    CdSIRI CdSIRIGroup3  2.51 (1.21–5.19)   0.054
Model 3    CdSIRI CdSIRIGroup4  3.62 (1.85–7.08)   0.007


#### 8. Table S1：CVD Events/Participants ####

#
DATA_count <- study_design$variables

# 
covars_by_model <- list(
  "Model 1" = character(0),
  "Model 2" = covars_model2,
  "Model 3" = covars_model3
)

cvd_count_results <- list()

for (v in indicators) {
  
  for (m in names(covars_by_model)) {
    
    # 
    required_vars <- c(
      "permthint",
      "CVD",
      v,
      covars_by_model[[m]]
    )
    
    # 
    dat <- DATA_count[
      complete.cases(
        DATA_count[, required_vars, drop = FALSE]
      ),
      ,
      drop = FALSE
    ]
    
    # 
    dat <- dat[
      dat$permthint > 0 &
        dat$CVD %in% c(0, 1),
      ,
      drop = FALSE
    ]
    
    if (nrow(dat) > 0) {
      
      tab <- dat %>%
        mutate(
          Group = as.character(.data[[v]])
        ) %>%
        filter(!is.na(Group)) %>%
        group_by(Group) %>%
        summarise(
          Participants = n(),
          CVD_Events = sum(CVD == 1),
          Event_percent = round(
            CVD_Events / Participants * 100,
            1
          ),
          `Events/Participants` = paste0(
            CVD_Events, "/", Participants
          ),
          .groups = "drop"
        ) %>%
        mutate(
          Indicator = v,
          Model = m
        ) %>%
        select(
          Indicator,
          Model,
          Group,
          Participants,
          CVD_Events,
          Event_percent,
          `Events/Participants`
        )
      
      cvd_count_results[[paste(v, m, sep = "_")]] <- tab
    }
  }
}

# 
cvd_count_table <- dplyr::bind_rows(cvd_count_results)

if (nrow(cvd_count_table) > 0) {
  
  cvd_count_table <- cvd_count_table %>%
    mutate(
      Indicator = factor(
        Indicator,
        levels = indicators
      ),
      Model = factor(
        Model,
        levels = c("Model 1", "Model 2", "Model 3")
      )
    ) %>%
    arrange(Indicator, Model, Group) %>%
    mutate(
      Indicator = as.character(Indicator),
      Model = as.character(Model)
    )
  
  # 
  print(
    as.data.frame(cvd_count_table),
    row.names = FALSE
  )
  
  # 
  write.csv(
    cvd_count_table,
    "Table_S1_CVD_Unweighted_Events_Participants.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
} else {
  message(
    "CVD Events/Participants"
  )
}


Indicator   Model  Group Participants CVD_Events Event_percent Events/Participants
CdPIV Model 1 Group1          574         13           2.3              13/574
CdPIV Model 1 Group2          541         38           7.0              38/541
CdPIV Model 1 Group3          660         44           6.7              44/660
CdPIV Model 1 Group4          729         62           8.5              62/729
CdPIV Model 2 Group1          574         13           2.3              13/574
CdPIV Model 2 Group2          541         38           7.0              38/541
CdPIV Model 2 Group3          660         44           6.7              44/660
CdPIV Model 2 Group4          729         62           8.5              62/729
CdPIV Model 3 Group1          574         13           2.3              13/574
CdPIV Model 3 Group2          541         38           7.0              38/541
CdPIV Model 3 Group3          660         44           6.7              44/660
CdPIV Model 3 Group4          729         62           8.5              62/729
CdPLR Model 1 Group1          557         23           4.1              23/557
CdPLR Model 1 Group2          558         28           5.0              28/558
CdPLR Model 1 Group3          718         43           6.0              43/718
CdPLR Model 1 Group4          671         63           9.4              63/671
CdPLR Model 2 Group1          557         23           4.1              23/557
CdPLR Model 2 Group2          558         28           5.0              28/558
CdPLR Model 2 Group3          718         43           6.0              43/718
CdPLR Model 2 Group4          671         63           9.4              63/671
CdPLR Model 3 Group1          557         23           4.1              23/557
CdPLR Model 3 Group2          558         28           5.0              28/558
CdPLR Model 3 Group3          718         43           6.0              43/718
CdPLR Model 3 Group4          671         63           9.4              63/671
CdMLR Model 1 Group1          567         16           2.8              16/567
CdMLR Model 1 Group2          548         35           6.4              35/548
CdMLR Model 1 Group3          676         38           5.6              38/676
CdMLR Model 1 Group4          713         68           9.5              68/713
CdMLR Model 2 Group1          567         16           2.8              16/567
CdMLR Model 2 Group2          548         35           6.4              35/548
CdMLR Model 2 Group3          676         38           5.6              38/676
CdMLR Model 2 Group4          713         68           9.5              68/713
CdMLR Model 3 Group1          567         16           2.8              16/567
CdMLR Model 3 Group2          548         35           6.4              35/548
CdMLR Model 3 Group3          676         38           5.6              38/676
CdMLR Model 3 Group4          713         68           9.5              68/713
CdNLR Model 1 Group1          551         16           2.9              16/551
CdNLR Model 1 Group2          564         35           6.2              35/564
CdNLR Model 1 Group3          665         37           5.6              37/665
CdNLR Model 1 Group4          724         69           9.5              69/724
CdNLR Model 2 Group1          551         16           2.9              16/551
CdNLR Model 2 Group2          564         35           6.2              35/564
CdNLR Model 2 Group3          665         37           5.6              37/665
CdNLR Model 2 Group4          724         69           9.5              69/724
CdNLR Model 3 Group1          551         16           2.9              16/551
CdNLR Model 3 Group2          564         35           6.2              35/564
CdNLR Model 3 Group3          665         37           5.6              37/665
CdNLR Model 3 Group4          724         69           9.5              69/724
CdSII Model 1 Group1          554         17           3.1              17/554
CdSII Model 1 Group2          561         34           6.1              34/561
CdSII Model 1 Group3          683         43           6.3              43/683
CdSII Model 1 Group4          706         63           8.9              63/706
CdSII Model 2 Group1          554         17           3.1              17/554
CdSII Model 2 Group2          561         34           6.1              34/561
CdSII Model 2 Group3          683         43           6.3              43/683
CdSII Model 2 Group4          706         63           8.9              63/706
CdSII Model 3 Group1          554         17           3.1              17/554
CdSII Model 3 Group2          561         34           6.1              34/561
CdSII Model 3 Group3          683         43           6.3              43/683
CdSII Model 3 Group4          706         63           8.9              63/706
CdSIRI Model 1 Group1          574         16           2.8              16/574
CdSIRI Model 1 Group2          541         35           6.5              35/541
CdSIRI Model 1 Group3          638         37           5.8              37/638
CdSIRI Model 1 Group4          751         69           9.2              69/751
CdSIRI Model 2 Group1          574         16           2.8              16/574
CdSIRI Model 2 Group2          541         35           6.5              35/541
CdSIRI Model 2 Group3          638         37           5.8              37/638
CdSIRI Model 2 Group4          751         69           9.2              69/751
CdSIRI Model 3 Group1          574         16           2.8              16/574
CdSIRI Model 3 Group2          541         35           6.5              35/541
CdSIRI Model 3 Group3          638         37           5.8              37/638
CdSIRI Model 3 Group4          751         69           9.2              69/751

####Table S2####
# Table S2
# Outcome: Cancer mortality (CA)
# Survey-weighted Cox regression


library(survey)
library(survival)
library(dplyr)

# 1. 
indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)

# 2. Model 2 
covars_model2 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1"
)

# 3. Model 3 
covars_model3 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1",
  "BMI1",
  "Smoke",
  "Alcohol",
  "sleepdisorder",
  "Diabetes",
  "Hypertension"
)

# 4. 
extract_cox <- function(fit, indicator, model_name) {
  
  coef_table <- summary(fit)$coefficients
  
  # 
  se_col <- intersect(
    c("se(coef)", "SE", "se"),
    colnames(coef_table)
  )
  
  if (length(se_col) == 0) {
    stop(
      paste(
        " ",
        paste(colnames(coef_table), collapse = ", ")
      )
    )
  }
  
  
  idx <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  if (length(idx) == 0) {
    warning(
      paste(indicator, model_name, "未找到对应系数")
    )
    return(data.frame())
  }
  
  tab <- coef_table[idx, , drop = FALSE]
  
  beta <- tab[, "coef"]
  se <- tab[, se_col[1]]
  
  data.frame(
    Model = model_name,
    Indicator = indicator,
    Variable = rownames(tab),
    HR = exp(beta),
    Lower95 = exp(beta - 1.96 * se),
    Upper95 = exp(beta + 1.96 * se),
    P = tab[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
}

# 5. 
all_results <- list()
model_errors <- list()

for (v in indicators) {
  
  # Model 1：
  formula1 <- as.formula(
    paste("Surv(permthint, CA) ~", v)
  )
  
  # Model 2：
  formula2 <- as.formula(
    paste(
      "Surv(permthint, CA) ~",
      paste(c(v, covars_model2), collapse = " + ")
    )
  )
  
  # Model 3：
  formula3 <- as.formula(
    paste(
      "Surv(permthint, CA) ~",
      paste(c(v, covars_model3), collapse = " + ")
    )
  )
  
  formulas <- list(
    "Model 1" = formula1,
    "Model 2" = formula2,
    "Model 3" = formula3
  )
  
  for (m in names(formulas)) {
    
    result_name <- paste(v, m, sep = "_")
    
    tryCatch({
      
      fit <- svycoxph(
        formula = formulas[[m]],
        design = study_design
      )
      
      all_results[[result_name]] <- extract_cox(
        fit = fit,
        indicator = v,
        model_name = m
      )
      
    }, error = function(e) {
      
      model_errors[[result_name]] <<- conditionMessage(e)
      
      message(
        " ", result_name,
        " ", conditionMessage(e)
      )
    })
  }
}

# 6. 

# 6. Combine results
results_all <- dplyr::bind_rows(all_results)

# Reset the NA printing option
options(na.print = "NA")

# Print all results
if (nrow(results_all) > 0) {
  print(as.data.frame(results_all), row.names = FALSE)
} else {
  message("No valid results were extracted.")
}

# 
getOption("na.print")

# 
options(na.print = "NA")

# 7. 
if (nrow(results_all) > 0) {
  
  results_all <- results_all %>%
    mutate(
      
      HR_CI = sprintf(
        "%.2f (%.2f–%.2f)",
        HR, Lower95, Upper95
      ),
      
      
      P_value = case_when(
        is.na(P) ~ NA_character_,
        P < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", P)
      )
    ) %>%
    arrange(
      match(Indicator, indicators),
      match(Model, c("Model 1", "Model 2", "Model 3"))
    ) %>%
    select(
      Model,
      Indicator,
      Variable,
      HR,
      Lower95,
      Upper95,
      P,
      HR_CI,
      P_value
    )
  
  # 
  print(as.data.frame(results_all), row.names = FALSE)
  
  # 
  write.csv(
    results_all,
    "Cd_CA_Cox_All_Results.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  # 
  results_table <- results_all %>%
    select(
      Model,
      Indicator,
      Variable,
      HR_CI,
      P_value
    )
  
  print(as.data.frame(results_table), row.names = FALSE)
  
  # 
  write.csv(
    results_table,
    "Cd_CA_Publication_Table.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
} 


####TableS2-output####
Model Indicator     Variable            HR_CI P_value
Model 1     CdPIV  CdPIVGroup2 0.84 (0.50–1.40)   0.529
Model 1     CdPIV  CdPIVGroup3 1.13 (0.72–1.79)   0.666
Model 1     CdPIV  CdPIVGroup4 1.46 (0.95–2.24)   0.177
Model 2     CdPIV  CdPIVGroup2 0.81 (0.48–1.35)   0.444
Model 2     CdPIV  CdPIVGroup3 1.11 (0.70–1.78)   0.708
Model 2     CdPIV  CdPIVGroup4 1.26 (0.82–1.95)   0.417
Model 3     CdPIV  CdPIVGroup2 0.76 (0.46–1.28)   0.341
Model 3     CdPIV  CdPIVGroup3 1.06 (0.65–1.72)   0.853
Model 3     CdPIV  CdPIVGroup4 1.16 (0.72–1.85)   0.611
Model 1     CdPLR  CdPLRGroup2 1.54 (0.91–2.58)   0.122
Model 1     CdPLR  CdPLRGroup3 1.70 (1.03–2.81)   0.065
Model 1     CdPLR  CdPLRGroup4 1.90 (1.15–3.13)   0.018
Model 2     CdPLR  CdPLRGroup2 1.55 (0.92–2.61)   0.104
Model 2     CdPLR  CdPLRGroup3 1.65 (0.99–2.74)   0.090
Model 2     CdPLR  CdPLRGroup4 1.72 (1.04–2.85)   0.046
Model 3     CdPLR  CdPLRGroup2 1.48 (0.88–2.50)   0.167
Model 3     CdPLR  CdPLRGroup3 1.52 (0.89–2.59)   0.150
Model 3     CdPLR  CdPLRGroup4 1.62 (0.95–2.77)   0.088
Model 1     CdMLR  CdMLRGroup2 2.25 (1.33–3.82)   0.012
Model 1     CdMLR  CdMLRGroup3 1.28 (0.73–2.25)   0.409
Model 1     CdMLR  CdMLRGroup4 3.27 (2.00–5.35)  <0.001
Model 2     CdMLR  CdMLRGroup2 1.95 (1.15–3.32)   0.041
Model 2     CdMLR  CdMLRGroup3 1.27 (0.72–2.24)   0.438
Model 2     CdMLR  CdMLRGroup4 2.43 (1.48–4.00)   0.003
Model 3     CdMLR  CdMLRGroup2 1.88 (1.10–3.21)   0.063
Model 3     CdMLR  CdMLRGroup3 1.18 (0.65–2.15)   0.597
Model 3     CdMLR  CdMLRGroup4 2.26 (1.35–3.81)   0.012
Model 1     CdNLR  CdNLRGroup2 1.52 (0.91–2.54)   0.216
Model 1     CdNLR  CdNLRGroup3 1.55 (0.94–2.57)   0.133
Model 1     CdNLR  CdNLRGroup4 2.01 (1.23–3.27)   0.010
Model 2     CdNLR  CdNLRGroup2 1.32 (0.79–2.21)   0.379
Model 2     CdNLR  CdNLRGroup3 1.47 (0.88–2.44)   0.198
Model 2     CdNLR  CdNLRGroup4 1.60 (0.98–2.61)   0.089
Model 3     CdNLR  CdNLRGroup2 1.22 (0.73–2.06)   0.532
Model 3     CdNLR  CdNLRGroup3 1.38 (0.81–2.33)   0.268
Model 3     CdNLR  CdNLRGroup4 1.44 (0.86–2.42)   0.204
Model 1     CdSII  CdSIIGroup2 0.76 (0.46–1.27)   0.310
Model 1     CdSII  CdSIIGroup3 1.17 (0.75–1.84)   0.585
Model 1     CdSII  CdSIIGroup4 1.30 (0.84–2.02)   0.349
Model 2     CdSII  CdSIIGroup2 0.74 (0.45–1.23)   0.267
Model 2     CdSII  CdSIIGroup3 1.12 (0.71–1.78)   0.696
Model 2     CdSII  CdSIIGroup4 1.16 (0.74–1.81)   0.612
Model 3     CdSII  CdSIIGroup2 0.69 (0.41–1.15)   0.176
Model 3     CdSII  CdSIIGroup3 1.04 (0.65–1.68)   0.890
Model 3     CdSII  CdSIIGroup4 1.05 (0.65–1.70)   0.867
Model 1    CdSIRI CdSIRIGroup2 1.45 (0.87–2.40)   0.247
Model 1    CdSIRI CdSIRIGroup3 1.28 (0.78–2.09)   0.334
Model 1    CdSIRI CdSIRIGroup4 2.08 (1.33–3.26)   0.010
Model 2    CdSIRI CdSIRIGroup2 1.27 (0.77–2.11)   0.460
Model 2    CdSIRI CdSIRIGroup3 1.26 (0.76–2.08)   0.371
Model 2    CdSIRI CdSIRIGroup4 1.64 (1.04–2.59)   0.094
Model 3    CdSIRI CdSIRIGroup2 1.23 (0.74–2.05)   0.530
Model 3    CdSIRI CdSIRIGroup3 1.23 (0.73–2.08)   0.444
Model 3    CdSIRI CdSIRIGroup4 1.52 (0.93–2.47)   0.187


#### 8. Table S2：Cancer Deaths/Participants ####

DATA_count <- study_design$variables

covars_by_model <- list(
  "Model 1" = character(0),
  "Model 2" = covars_model2,
  "Model 3" = covars_model3
)

ca_count_results <- list()

for (v in indicators) {
  
  for (m in names(covars_by_model)) {
    
    # 
    required_vars <- c(
      "permthint",
      "CA",
      v,
      covars_by_model[[m]]
    )
    
    # 
    dat <- DATA_count[
      complete.cases(
        DATA_count[, required_vars, drop = FALSE]
      ),
      ,
      drop = FALSE
    ]
    
    # 
    dat <- dat[
      dat$permthint > 0 &
        dat$CA %in% c(0, 1),
      ,
      drop = FALSE
    ]
    
    if (nrow(dat) > 0) {
      
      tab <- dat %>%
        mutate(
          Group = as.character(.data[[v]])
        ) %>%
        filter(!is.na(Group)) %>%
        group_by(Group) %>%
        summarise(
          Participants = n(),
          Deaths = sum(CA == 1),
          Death_percent = round(
            Deaths / Participants * 100,
            1
          ),
          `Deaths/Participants` = paste0(
            Deaths, "/", Participants
          ),
          .groups = "drop"
        ) %>%
        mutate(
          Indicator = v,
          Model = m
        ) %>%
        select(
          Indicator,
          Model,
          Group,
          Participants,
          Deaths,
          Death_percent,
          `Deaths/Participants`
        )
      
      ca_count_results[[paste(v, m, sep = "_")]] <- tab
    }
  }
}

# 
ca_count_table <- dplyr::bind_rows(ca_count_results)

if (nrow(ca_count_table) > 0) {
  
  ca_count_table <- ca_count_table %>%
    mutate(
      Indicator = factor(
        Indicator,
        levels = indicators
      ),
      Model = factor(
        Model,
        levels = c("Model 1", "Model 2", "Model 3")
      )
    ) %>%
    arrange(Indicator, Model, Group) %>%
    mutate(
      Indicator = as.character(Indicator),
      Model = as.character(Model)
    )
  
  # 
  print(
    as.data.frame(ca_count_table),
    row.names = FALSE
  )
  
  # 
  write.csv(
    ca_count_table,
    "Table_S2_CA_Unweighted_Deaths_Participants.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
} else {
  message(
    ""
  )
}

#### 9.  ####

if (length(model_errors) > 0) {
  cat("\n以下模型拟合失败：\n")
  print(model_errors)
}

####Table S3####
# Table S3
# Outcome: All-cause mortality
# Survey-weighted Cox regression
# Indicators: PIV, PLR, MLR, NLR, SII, SIRI, Cd
# Reference: Low

library(survey)
library(dplyr)
library(tidyr)

options(survey.lonely.psu = "adjust")

indicators <- c(
  "PIV_binary",
  "PLR_binary",
  "MLR_binary",
  "NLR_binary",
  "SII_binary",
  "SIRI_binary",
  "Cd_binary"
)

for (indicator in indicators) {
  
  study_design$variables[[indicator]] <- relevel(
    factor(study_design$variables[[indicator]]),
    ref = "Low"
  )
}

formula_model1 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator
    )
  )
}


formula_model2 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
}


formula_model3 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
}

extract_cox <- function(model, indicator, model_name) {
  
  coef_table <- summary(model)$coefficients
  
  # 
  rows <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  coef_table2 <- coef_table[
    rows,
    ,
    drop = FALSE
  ]
  
  # 95% CI
  ci <- suppressWarnings(
    confint(model)
  )
  
  ci2 <- ci[
    rows,
    ,
    drop = FALSE
  ]
  
  result <- data.frame(
    
    Variable = rownames(coef_table2),
    
    HR = exp(
      coef_table2[, "coef"]
    ),
    
    Lower95 = exp(
      ci2[, 1]
    ),
    
    Upper95 = exp(
      ci2[, 2]
    ),
    
    P = coef_table2[
      ,
      "Pr(>|z|)"
    ],
    
    Model = model_name,
    
    stringsAsFactors = FALSE
  )
  
  return(result)
}

all_results <- list()


for (indicator in indicators) {
  
  cat(
    "\n====================================\n"
  )
  
  cat(
    "Running:",
    indicator,
    "\n"
  )
  
  cat(
    "====================================\n"
  )
  
  # Model 1
  model1 <- svycoxph(
    formula_model1(indicator),
    design = study_design
  )
  
  # Model 2
  
  model2 <- svycoxph(
    formula_model2(indicator),
    design = study_design
  )

  # Model 3
  model3 <- svycoxph(
    formula_model3(indicator),
    design = study_design
  )

  
  res1 <- extract_cox(
    model1,
    indicator,
    "Model 1"
  )
  
  res2 <- extract_cox(
    model2,
    indicator,
    "Model 2"
  )
  
  res3 <- extract_cox(
    model3,
    indicator,
    "Model 3"
  )
  
  result <- bind_rows(
    res1,
    res2,
    res3
  )
  
  
  result$Indicator <- indicator
  
  all_results[[indicator]] <- result
}


TableS3_results <- bind_rows(
  all_results
)

TableS3_results <- TableS3_results %>%
  
  mutate(
    
    HR_95CI = sprintf(
      "%.2f (%.2f–%.2f)",
      HR,
      Lower95,
      Upper95
    ),
    
    P_value = ifelse(
      P < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P
      )
    )
  )

TableS3_Cox <- TableS3_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  )


cat(
  "\n====================================\n"
)

cat(
  "Table S3: Cox regression results\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS3_Cox
)


TableS3_wide <- TableS3_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  ) %>%
  
  pivot_wider(
    
    names_from = Model,
    
    values_from = c(
      HR_95CI,
      P_value
    )
  )

TableS3_final <- TableS3_wide

cat(
  "\n====================================\n"
)

cat(
  "FINAL Table S3\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS3_final
)
####TableS3-output####
  Indicator   Variable        `HR_95CI_Model 1` `HR_95CI_Model 2` `HR_95CI_Model 3`
1 PIV_binary  PIV_binaryHigh  1.52 (1.23–1.86)  1.39 (1.14–1.69)  1.28 (1.05–1.56) 
2 PLR_binary  PLR_binaryHigh  1.31 (1.04–1.66)  1.26 (1.01–1.58)  1.22 (0.96–1.54) 
3 MLR_binary  MLR_binaryHigh  2.61 (2.08–3.28)  2.13 (1.68–2.70)  2.03 (1.60–2.57) 
4 NLR_binary  NLR_binaryHigh  1.81 (1.46–2.25)  1.58 (1.26–1.97)  1.44 (1.15–1.80) 
5 SII_binary  SII_binaryHigh  1.32 (1.07–1.62)  1.25 (1.00–1.55)  1.15 (0.93–1.42) 
6 SIRI_binary SIRI_binaryHigh 2.19 (1.74–2.75)  1.86 (1.48–2.33)  1.71 (1.36–2.14) 
7 Cd_binary   Cd_binaryHigh   1.77 (1.45–2.16)  1.56 (1.25–1.95)  1.57 (1.24–2.00) 

#### 8. Table S3： Deaths/Participants ####

# 使用 survey design 中的数据
DATA_count <- study_design$variables

# 各模型对应的协变量
covars_by_model <- list(
  "Model 1" = character(0),
  "Model 2" = c(
    "Gender", "Race", "Edu", "Marital1", "Age2", "PIR1"
  ),
  "Model 3" = c(
    "Gender", "Race", "Edu", "Marital1", "Age2", "PIR1",
    "BMI1", "Smoke", "Alcohol", "sleepdisorder",
    "Diabetes", "Hypertension"
  )
)

death_count_results <- list()

for (indicator in indicators) {
  
  for (m in names(covars_by_model)) {
    
    # 当前模型所需变量
    required_vars <- c(
      "permthint",
      "mortstat",
      indicator,
      covars_by_model[[m]]
    )
    
    # 按当前模型完整病例标准筛选
    dat <- DATA_count[
      complete.cases(
        DATA_count[, required_vars, drop = FALSE]
      ),
      ,
      drop = FALSE
    ]
    
    # 排除无效随访时间及结局编码
    dat <- dat[
      dat$permthint > 0 &
        dat$mortstat %in% c(0, 1),
      ,
      drop = FALSE
    ]
    
    if (nrow(dat) > 0) {
      
      tab <- dat %>%
        mutate(
          Group = as.character(.data[[indicator]])
        ) %>%
        filter(Group %in% c("Low", "High")) %>%
        group_by(Group) %>%
        summarise(
          Participants = n(),
          Deaths = sum(mortstat == 1),
          Death_percent = round(
            Deaths / Participants * 100, 1
          ),
          `Deaths/Participants` = paste0(
            Deaths, "/", Participants
          ),
          .groups = "drop"
        ) %>%
        mutate(
          Indicator = indicator,
          Model = m
        ) %>%
        select(
          Indicator,
          Model,
          Group,
          Participants,
          Deaths,
          Death_percent,
          `Deaths/Participants`
        )
      
      death_count_results[[
        paste(indicator, m, sep = "_")
      ]] <- tab
    }
  }
}

# 合并所有统计结果
TableS3_death_counts <- dplyr::bind_rows(
  death_count_results
)

if (nrow(TableS3_death_counts) > 0) {
  
  TableS3_death_counts <- TableS3_death_counts %>%
    mutate(
      Indicator = factor(
        Indicator,
        levels = indicators
      ),
      Model = factor(
        Model,
        levels = c("Model 1", "Model 2", "Model 3")
      ),
      Group = factor(
        Group,
        levels = c("Low", "High")
      )
    ) %>%
    arrange(Indicator, Model, Group) %>%
    mutate(
      Indicator = as.character(Indicator),
      Model = as.character(Model),
      Group = as.character(Group)
    )
  
  # 输出结果
  print(
    as.data.frame(TableS3_death_counts),
    row.names = FALSE
  )
  
  # 导出 CSV
  write.csv(
    TableS3_death_counts,
    "Table_S3_Unweighted_Deaths_Participants.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
} else {
  message(
    "未生成死亡人数/参与人数统计结果，请检查分组变量及结局编码。"
  )
}

Indicator   Model Group Participants Deaths Death_percent Deaths/Participants
PIV_binary Model 1   Low         1234    256          20.7            256/1234
PIV_binary Model 1  High         1270    363          28.6            363/1270
PIV_binary Model 2   Low         1234    256          20.7            256/1234
PIV_binary Model 2  High         1270    363          28.6            363/1270
PIV_binary Model 3   Low         1234    256          20.7            256/1234
PIV_binary Model 3  High         1270    363          28.6            363/1270
PLR_binary Model 1   Low         1275    277          21.7            277/1275
PLR_binary Model 1  High         1229    342          27.8            342/1229
PLR_binary Model 2   Low         1275    277          21.7            277/1275
PLR_binary Model 2  High         1229    342          27.8            342/1229
PLR_binary Model 3   Low         1275    277          21.7            277/1275
PLR_binary Model 3  High         1229    342          27.8            342/1229
MLR_binary Model 1   Low         1243    217          17.5            217/1243
MLR_binary Model 1  High         1261    402          31.9            402/1261
MLR_binary Model 2   Low         1243    217          17.5            217/1243
MLR_binary Model 2  High         1261    402          31.9            402/1261
MLR_binary Model 3   Low         1243    217          17.5            217/1243
MLR_binary Model 3  High         1261    402          31.9            402/1261
NLR_binary Model 1   Low         1216    233          19.2            233/1216
NLR_binary Model 1  High         1288    386          30.0            386/1288
NLR_binary Model 2   Low         1216    233          19.2            233/1216
NLR_binary Model 2  High         1288    386          30.0            386/1288
NLR_binary Model 3   Low         1216    233          19.2            233/1216
NLR_binary Model 3  High         1288    386          30.0            386/1288
SII_binary Model 1   Low         1237    265          21.4            265/1237
SII_binary Model 1  High         1267    354          27.9            354/1267
SII_binary Model 2   Low         1237    265          21.4            265/1237
SII_binary Model 2  High         1267    354          27.9            354/1267
SII_binary Model 3   Low         1237    265          21.4            265/1237
SII_binary Model 3  High         1267    354          27.9            354/1267
SIRI_binary Model 1   Low         1212    224          18.5            224/1212
SIRI_binary Model 1  High         1292    395          30.6            395/1292
SIRI_binary Model 2   Low         1212    224          18.5            224/1212
SIRI_binary Model 2  High         1292    395          30.6            395/1292
SIRI_binary Model 3   Low         1212    224          18.5            224/1212
SIRI_binary Model 3  High         1292    395          30.6            395/1292
Cd_binary Model 1   Low         1115    195          17.5            195/1115
Cd_binary Model 1  High         1389    424          30.5            424/1389
Cd_binary Model 2   Low         1115    195          17.5            195/1115
Cd_binary Model 2  High         1389    424          30.5            424/1389
Cd_binary Model 3   Low         1115    195          17.5            195/1115
Cd_binary Model 3  High         1389    424          30.5            424/1389
#### Table S4 ####
options(survey.lonely.psu = "adjust")
study_design <- svydesign(data=DATA, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~WT, nest=TRUE)
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value
####Cd_median=3.11####
Cd_median_value <- 3.11

DATA$Cd_binary <- ifelse(
  DATA$Cd < Cd_median_value,
  "Low",
  "High"
)

DATA$Cd_binary <- factor(
  DATA$Cd_binary,
  levels = c("Low", "High")
)
# quantile
Cd_Q1_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd < 3.11),
  quantiles = 0.50,
  na.rm = TRUE
)

# 
Cd_Q3_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd >= 3.11),
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_Q1_new <- as.numeric(coef(Cd_Q1_new))
Cd_Q3_new <- as.numeric(coef(Cd_Q3_new))

Cd_Q1_new
Cd_Q3_new
DATA$Cd_quartile <- cut(
  DATA$Cd,
  breaks = c(
    -Inf,
    Cd_Q1_new,
    3.11,
    Cd_Q3_new,
    Inf
  ),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,
  right = FALSE
)
table(DATA$Cd_binary, DATA$Cd_quartile, useNA = "ifany")

#Q4
model1 <- svycoxph(Surv(permthint, mortstat) ~ Cd_quartile , design = study_design)
model2 <- svycoxph(Surv(permthint, mortstat) ~  Cd_quartile+Gender+Race+Edu+Marital1+Age2+PIR1 , design = study_design)
model3 <- svycoxph(Surv(permthint, mortstat) ~ Cd_quartile+Gender+Race+Edu+Marital1+Age2+PIR1+BMI1+Smoke+Alcohol+sleepdisorder+Diabetes+Hypertension , design = study_design)
summary(model1)
summary(model2)
summary(model3)
   
####Deaths/Participants####

death_table <- DATA %>%
  filter(
    !is.na(Cd_quartile),
    !is.na(mortstat)
  ) %>%
  group_by(Cd_quartile) %>%
  summarise(
    Participants = n(),
    Deaths = sum(mortstat == 1, na.rm = TRUE),
    Death_percent = round(Deaths / Participants * 100, 1),
    `Deaths/Participants` = paste0(
      Deaths, "/", Participants
    ),
    .groups = "drop"
  )

print(death_table)
####TableS4-output####
#n= 2504, number of events= 619  
model1
coef exp(coef) se(coef) robust se     z Pr(>|z|)    
Cd_quartileQ2 0.4674    1.5958   0.1678    0.2152 2.172   0.0298 *  
Cd_quartileQ3 0.7823    2.1864   0.1593    0.1695 4.615 3.94e-06 ***
Cd_quartileQ4 0.8929    2.4423   0.1558    0.1618 5.518 3.42e-08 ***
model2
coef exp(coef) se(coef) robust se      z Pr(>|z|)    
Cd_quartileQ2  0.38959   1.47638  0.16821   0.21409  1.820 0.068803 .  
Cd_quartileQ3  0.59711   1.81686  0.16091   0.18057  3.307 0.000943 ***
  Cd_quartileQ4  0.75363   2.12469  0.15903   0.17746  4.247 2.17e-05 ***
  model3
coef exp(coef) se(coef) robust se      z Pr(>|z|)    
Cd_quartileQ2  0.30289   1.35376  0.16901   0.21495  1.409 0.158810    
Cd_quartileQ3  0.62336   1.86518  0.16417   0.17260  3.612 0.000304 ***
Cd_quartileQ4  0.84526   2.32858  0.17995   0.18625  4.538 5.67e-06 ***

Cd_quartile Participants Deaths Death_percent `Deaths/Participants`
<fct>              <int>  <int>         <dbl> <chr>                
1 Q1                   505     74          14.7 74/505               
2 Q2                   610    121          19.8 121/610              
3 Q3                   671    193          28.8 193/671              
4 Q4                   718    231          32.2 231/718   
####______P for trend####
DATA$Cd_quartile_trend <- as.numeric(DATA$Cd_quartile)

study_design <- update(
  study_design,
  Cd_quartile_trend = DATA$Cd_quartile_trend
)

model1_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend,
  design = study_design
)

summary(model1_trend)

model2_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend+Gender + Race + Edu + Marital1 + Age2 + PIR1,
  design = study_design
)

summary(model2_trend)

model3_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend+Gender + Race + Edu + Marital1 + Age2 + PIR1
  + BMI1 + Smoke + Alcohol + sleepdisorder +
    Diabetes + Hypertension,
  design = study_design
)

summary(model3_trend)
#                   coef      exp(coef) se(coef)  robust se z   Pr(>|z|)    
#Cd_quartile_trend  0.27481   1.31628  0.04444   0.04291  6.405 1.51e-10 ***
#Cd_quartile_trend  0.23103   1.25990  0.04642   0.05141  4.494 7.00e-06 ***  
#Cd_quartile_trend  0.26973   1.30961  0.05498   0.06075  4.440 9.00e-06 ***

####Table S11####
####1year landmark####
library(survey)
library(survival)


# 
landmark1 <- DATA %>%
  filter(
    mortstat == 0 | permthint > 12
  )


# 
landmark1$time_landmark <- landmark1$permthint - 12


# 
landmark1$death_landmark <- ifelse(
  landmark1$mortstat == 1 &
    landmark1$permthint > 12,
  1,
  0
)


design_landmark1 <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = landmark1
)

#COX1
cox1_landmark1 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR,
  design = design_landmark1
)
summary(cox1_landmark1)
#
extract_HR <- function(model){
  result <- summary(cox1_landmark1)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
summary(cox1_landmark1)
extract_HR(cox1_landmark1)
####modle1-output####
              HR        CI        P
CdMLRGroup2 3.30  2.1-5.19 2.19e-07
CdMLRGroup3 2.14 1.46-3.14 1.05e-04
CdMLRGroup4 5.06  3.28-7.8 2.06e-13

cox2_landmark1 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR+
    Age2+
    Gender+
    Race+
    PIR1+
    Edu,
  design = design_landmark1
)
#
extract_HR <- function(model){
  result <- summary(cox2_landmark1)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
extract_HR(cox2_landmark1)
summary(cox2_landmark1)
####modle2-output####
              HR         CI        P
CdMLRGroup2 2.85   1.81-4.5 6.58e-06
CdMLRGroup3 2.05  1.41-2.97 1.60e-04
CdMLRGroup4 3.67   2.4-5.63 2.33e-09
  
cox3_landmark1 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR+
    Age2+
    Gender+
    Race+
    PIR1+
    Edu+
    BMI1+
    Marital1+
    Smoke+
    Alcohol+
    Diabetes+
    Hypertension+
    sleepdisorder,
  design = design_landmark1
)
summary(cox3_landmark1)
#
extract_HR <- function(model){
  result <- summary(cox3_landmark1)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
extract_HR(cox3_landmark1)
               HR         CI        P
CdMLRGroup2   2.77  1.75-4.39 1.28e-05
CdMLRGroup3   2.07  1.34-3.19 9.90e-04
CdMLRGroup4   3.54  2.22-5.63 9.60e-08
####2years ####
landmark2 <- DATA %>%
  filter(mortstat == 0 | permthint > 24) %>%
  mutate(
    time_landmark = permthint - 24,
    death_landmark = ifelse(mortstat == 1 & permthint > 24, 1, 0)
  )

design_landmark2 <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = landmark2
)

cox1_landmark2 <- svycoxph(
  Surv(time_landmark, death_landmark) ~ CdMLR,
  design = design_landmark2
)
summary(cox1_landmark2)

#
extract_HR <- function(cox1_landmark2){
  result <- summary(cox1_landmark2)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}

extract_HR(cox1_landmark2)
####modle1-output####
             HR        CI        P
CdMLRGroup2 3.30 2.04-5.33 1.12e-06
CdMLRGroup3 2.10 1.39-3.18 4.38e-04
CdMLRGroup4 4.87 3.11-7.61 3.96e-12

cox2_landmark2 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR+
    Age2+
    Gender+
    Race+
    PIR1+
    Edu,
  design = design_landmark2
)
summary(cox2_landmark2)
#
extract_HR <- function(cox2_landmark2){
  result <- summary(cox2_landmark2)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
extract_HR(cox2_landmark2)
####modle2-output####
               HR        CI        P
CdMLRGroup2  2.87  1.75-4.71 2.79e-05
CdMLRGroup3  2.01  1.36-2.98 4.62e-04
CdMLRGroup4  3.49  2.25-5.42 2.31e-08

cox3_landmark2 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR+
    Age2+
    Gender+
    Race+
    PIR1+
    Edu+
    BMI1+
    Marital1+
    Smoke+
    Alcohol+
    Diabetes+
    Hypertension+
    sleepdisorder,
  design = design_landmark2
)
summary(cox3_landmark2)
#
extract_HR <- function(cox3_landmark2){
  result <- summary(cox3_landmark2)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
extract_HR(cox3_landmark2)

####modle3-output####
                  HR         CI        P
CdMLRGroup2    2.77  1.68-4.56 6.16e-05
CdMLRGroup3    1.98  1.24-3.16 4.41e-03
CdMLRGroup4    3.32  2.05-5.37 1.02e-06

####
library(dplyr)

event_table_1yr <- landmark1 %>%
  group_by(CdMLR) %>%
  summarise(
    Participants = n(),
    Deaths = sum(death_landmark == 1, na.rm = TRUE),
    Death_percent = round(
      Deaths / Participants * 100,
      1
    )
  )

event_table_1yr

CdMLR  Participants Deaths Death_percent
<chr>         <int>  <int>         <dbl>
1 Group1          560     60          10.7
2 Group2          535    115          21.5
3 Group3          662    136          20.5
4 Group4          688    249          36.2
#2years
event_table_2yr <- landmark2 %>%
  group_by(CdMLR) %>%
  summarise(
    Participants = n(),
    Deaths = sum(death_landmark == 1, na.rm = TRUE),
    Death_percent = round(
      Deaths / Participants * 100,
      1
    )
  )

event_table_2yr

CdMLR  Participants Deaths Death_percent
<chr>         <int>  <int>         <dbl>
1 Group1          551     51           9.3
2 Group2          522    102          19.5
3 Group3          648    122          18.8
4 Group4          651    212          32.6
####Fig S4-Spearman correlation heatmap####

inflammation_markers <- DATA[, c(
  "PIV",
  "SII",
  "PLR",
  "MLR",
  "NLR",
  "SIRI"
)]
# Spearman correlation matrix

cor_matrix <- cor(
  inflammation_markers,
  method = "spearman",
  use = "complete.obs"
)

####output####
round(cor_matrix, 3)
     PIV   SII   PLR   MLR   NLR  SIRI
PIV  1.000 0.856 0.491 0.631 0.714 0.899
SII  0.856 1.000 0.729 0.471 0.847 0.727
PLR  0.491 0.729 1.000 0.500 0.586 0.356
MLR  0.631 0.471 0.500 1.000 0.635 0.789
NLR  0.714 0.847 0.586 0.635 1.000 0.818
SIRI 0.899 0.727 0.356 0.789 0.818 1.000
library(corrplot)
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "original",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.8
)


####RCS-output####
#PIV
Name	    Chi_Square	      Df	Pvalue
PIV	      51.7083397082003	2	  5.91116045001172e-12
Nonlinear	0.934966165869864	1	  0.333575786187208

#PLR	
Name	    Chi_Square        Df	 Pvalue
PLR	      21.163954505016	  2	   2.53691357056107e-05
Nonlinear	11.8407736059555	1	   0.000579477468983458

#MLR
Name     	Chi_Square        Df	Pvalue
MLR	      65.4747961934177	2	  6.10622663543836e-15
Nonlinear	2.86867921040549	1	  0.0903189639291842

#NLR
Name    	Chi_Square	      Df	Pvalue
NLR     	45.7926888781201	2	  1.13826614800416e-10
Nonlinear	0.424540839972219	1	  0.514679905911131

#SII
Name	    Chi_Square	      Df	Pvalue
SII       42.6089750465732	2	  5.5921478558929e-10
Nonlinear	8.02492327069234	1	  0.00461379804444295

#SIRI
Name	    Chi_Square	     Df	  Pvalue
SIRI	    65.3558013604289	2	  6.43929354282591e-15
Nonlinear	5.35807238439301	1	  0.0206265606664265




library(foreign)
library(dplyr)
library(survey)
library(survival)
library(rms)
library(ggplot2)

options(survey.lonely.psu = "adjust")

# =========================================================
# 1. 读取数据
# =========================================================

DATA <- read.csv(
  "C:/Users/王娅/Desktop/DATA.csv",
  check.names = FALSE
)

# =========================================================
# 2. 建立NHANES复杂抽样设计
# =========================================================

study_design <- svydesign(
  data = DATA,
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE
)

# =========================================================
# 3. 协变量
# =========================================================

covars_model3 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1",
  "BMI1",
  "Smoke",
  "Alcohol",
  "sleepdisorder",
  "Diabetes",
  "Hypertension"
)

# =========================================================
# 4. 检查关键变量
# =========================================================

summary(DATA$Cd)
table(DATA$mortstat, useNA = "ifany")
summary(DATA$permthint)

# =========================================================
# 5. 删除关键变量缺失
# =========================================================

vars_needed <- c(
  "Cd",
  "mortstat",
  "permthint",
  covars_model3,
  "SDMVPSU",
  "SDMVSTRA",
  "WT"
)

DATA_RCS <- DATA %>%
  filter(
    complete.cases(
      across(all_of(vars_needed))
    )
  )

cat("RCS分析最终样本量：", nrow(DATA_RCS), "\n")

# =========================================================
# 6. 重新建立survey design
# =========================================================

study_design_rcs <- svydesign(
  data = DATA_RCS,
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE
)

# =========================================================
# 7. 确定3个RCS节点
#
# 这里采用5%、50%、95%分位数
# =========================================================

cd_knots <- as.numeric(
  quantile(
    DATA_RCS$Cd,
    probs = c(0.05, 0.50, 0.95),
    na.rm = TRUE
  )
)

cat("Cd的3个RCS节点：\n")
print(cd_knots)

# =========================================================
# 8. 使用rms::rcs建立RCS变量
# =========================================================

rcs_basis <- rms::rcs(
  DATA_RCS$Cd,
  knots = cd_knots
)

# 添加到数据
DATA_RCS$Cd_RCS1 <- rcs_basis[, 1]
DATA_RCS$Cd_RCS2 <- rcs_basis[, 2]

# =========================================================
# 9. 重新建立survey design
# =========================================================

study_design_rcs <- svydesign(
  data = DATA_RCS,
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE
)

# =========================================================
# 10. Cox RCS模型
# =========================================================

formula_rcs <- as.formula(
  paste(
    "Surv(permthint, mortstat) ~ Cd_RCS1 + Cd_RCS2 +",
    paste(covars_model3, collapse = " + ")
  )
)

model_rcs <- svycoxph(
  formula_rcs,
  design = study_design_rcs
)

summary(model_rcs)
# =========================================================
# 11. P for overall
# =========================================================

p_overall <- regTermTest(
  model_rcs,
  ~ Cd_RCS1 + Cd_RCS2
)

print(p_overall)
# =========================================================
# 12. 建立线性Cd模型
# =========================================================

formula_linear <- as.formula(
  paste(
    "Surv(permthint, mortstat) ~ Cd +",
    paste(covars_model3, collapse = " + ")
  )
)

model_linear <- svycoxph(
  formula_linear,
  design = study_design_rcs
)

summary(model_linear)
# =========================================================
# 13. 非线性检验
# =========================================================

anova_rcs <- regTermTest(
  model_rcs,
  ~ Cd_RCS1 + Cd_RCS2
)

anova_rcs
# =========================================================
# 14. 建立预测Cd序列
# =========================================================

cd_seq <- seq(
  quantile(DATA_RCS$Cd, 0.01, na.rm = TRUE),
  quantile(DATA_RCS$Cd, 0.99, na.rm = TRUE),
  length.out = 300
)

# =========================================================
# 15. 建立预测数据
# =========================================================

newdata <- data.frame(
  Cd = cd_seq
)

# =========================================================
# 16. 协变量固定
#     数值变量：中位数
#     分类变量：众数
# =========================================================

for (v in covars_model3) {
  
  if (is.factor(DATA_RCS[[v]])) {
    
    mode_value <- names(
      sort(
        table(DATA_RCS[[v]]),
        decreasing = TRUE
      )
    )[1]
    
    newdata[[v]] <- factor(
      mode_value,
      levels = levels(DATA_RCS[[v]])
    )
    
  } else {
    
    newdata[[v]] <- median(
      DATA_RCS[[v]],
      na.rm = TRUE
    )
  }
}

# =========================================================
# 17. 生成预测数据中的RCS变量
# =========================================================

rcs_new <- rms::rcs(
  newdata$Cd,
  knots = cd_knots
)

newdata$Cd_RCS1 <- rcs_new[, 1]
newdata$Cd_RCS2 <- rcs_new[, 2]

####Deaths_Participants####
library(dplyr)


# 
subgroup_vars <- c(
  "Gender",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1",
  "Diabetes",
  "Hypertension"
)


# 
death_count_list <- list()


for(var in subgroup_vars){
  
  
  tmp <- DATA %>%
    
    group_by(
      CdSIRI,
      .data[[var]]
    ) %>%
    
    summarise(
      
      N = n(),
      
      Deaths = sum(
        mortstat,
        na.rm = TRUE
      ),
      
      Death_percent = round(
        Deaths/N*100,
        2
      ),
      
      .groups = "drop"
    )
  
  
  # 
  tmp$Subgroup <- var
  
  
  # 
  tmp$Level <- as.character(
    tmp[[var]]
  )
  
  
  death_count_list[[var]] <- tmp
  
}



# 
death_count_table <- bind_rows(
  death_count_list
)



# 
death_count_table <- death_count_table %>%
  
  select(
    Subgroup,
    Level,
    CdSIRI,
    N,
    Deaths,
    Death_percent
  )


# 
print(death_count_table, n = Inf)
write.csv(
  death_count_table,
  "CdSIRI_subgroup_death_count.csv",
  row.names = FALSE
)
# 

####KM  numbers at risk ####
times_to_check <- c(0, 40, 80, 120, 160)

# =================  1: mortstat  =================

# Calculate unweighted numbers at risk
get_unweighted_risk <- function(design, exposure, times) {
  
  df <- design$variables
  
  if (!exposure %in% names(df)) {
    stop("Exposure variable not found in design.")
  }
  
  if (!all(c("permthint", "mortstat") %in% names(df))) {
    stop("Survival variables not found in design.")
  }
  
  group_var <- df[[exposure]]
  
  if (is.null(group_var)) {
    stop("Invalid exposure variable.")
  }
  
  groups <- unique(as.character(group_var[!is.na(group_var)]))
  
  result <- expand.grid(
    Time = times,
    Group = groups,
    stringsAsFactors = FALSE
  )
  
  result$At_Risk <- mapply(
    function(t, g) {
      sum(
        as.character(group_var) == g &
          !is.na(df$permthint) &
          !is.na(df$mortstat) &
          df$permthint >= t
      )
    },
    result$Time,
    result$Group
  )
  
  result
}


# Fit weighted Kaplan-Meier curves
svy_km_mort <- svykm(
  Surv(permthint, mortstat) ~ CdSIRI,
  design = study_design
)

# Calculate numbers at risk
risk_mort <- get_unweighted_risk(
  study_design,
  "CdSIRI",
  times_to_check
)

# Display the risk table
print(risk_mort)

# =================  2: Ca ================
# KM
svy_km_ca <- svykm(Surv(permthint, CA) ~ CdPIV, design = study_design)
# 
risk_ca <- get_unweighted_risk(study_design, "CdPIV", times_to_check)
print("=== Ca  ===")
print(risk_ca)

# ================= 3: CVD  =================
# KM
svy_km_cvd <- svykm(Surv(permthint, CVD) ~ CdPIV, design = study_design)
# 
risk_cvd <- get_unweighted_risk(study_design, "CdPIV", times_to_check)
print("===  CVD  ===")
print(risk_cvd)
