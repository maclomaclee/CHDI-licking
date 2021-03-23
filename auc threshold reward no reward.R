library(Hmisc)
library(ggplot2)

lickcount_rawData_mj <- read_excel("lickcount_rawData_mj.xlsx", col_names = FALSE)
auROC_Data_mj <- read_excel("auROC_Data_mj.xlsx", col_names = FALSE)
observed <- as.data.frame(auROC_Data_mj)
colnames(observed) <- c("mouse", "genotype", "sex", "training_type", "training_session", "AUC1","AUC2")

#Column 1: subject ID
#Column 2: Genotype 1: HD, 2: WT
#Column 3: Sex 1: M, 2: F
#Column 4: Training type 1: regular cue, 2: extended cue
#Column 5: Training session 1-30
#AUC1 is auROC value of licks between precue and cue period
#AUC2 is auROC value of licks for cue period between reward and no reward

observed <- as.data.frame(auROC_Data_mj)
colnames(observed) <- c("mouse", "genotype", "sex", "training_type", "training_session", "AUC1","AUC2")

  for (i in 1:2010)  {

  ifelse (observed[i,7] > 0.55,observed[i,7] <- 1, observed[i,7] <- 0)
}

#extended cue training
obs_ext <- subset(observed, training_type == 2)
obs_reg <- subset(observed, training_type == 1)



mice_ext <- unique(obs_ext[,1])
mice_reg <- unique(obs_reg[,1])

mice_ext_hd <- subset(obs_ext, genotype == 1)
mice_ext_wt <- subset(obs_ext, genotype == 2)
mice_reg_hd <- subset(obs_reg, genotype == 1)
mice_reg_wt <- subset(obs_reg, genotype == 2)


m_e_h_epoch <- data.frame(matrix(nrow = 30, ncol = 3))
colnames(m_e_h_epoch) <- (c("session", "AUC", "type"))

for (i in 1:30) {
  epoch <- subset(mice_ext_hd, training_session == i)
m_e_h_epoch[i,2] <- mean(epoch[,7])
m_e_h_epoch[i,1] <- i
m_e_h_epoch[i,3] <- "HD, Extended"
}

for (i in 31:60)  {
   epoch <- subset(mice_ext_wt, training_session == i-30)
m_e_h_epoch[i,2] <- mean(epoch[,7])
m_e_h_epoch[i,1] <- i-30
m_e_h_epoch[i,3] <- "WT, extended"
    }

for (i in 61:90) {
    epoch <- subset(mice_reg_hd, training_session == i-60)
    m_e_h_epoch[i,2] <- mean(epoch[,7])
    m_e_h_epoch[i,1] <- i-60
    m_e_h_epoch[i,3] <- "HD, Regular"
    }  
      for (i in 91:120) {
        epoch <- subset(mice_reg_wt, training_session == i-90)
        m_e_h_epoch[i,2] <- mean(epoch[,7])
        m_e_h_epoch[i,1] <- i-90
        m_e_h_epoch[i,3] <- "WT, Regular"
    }


ggplot(data = m_e_h_epoch, mapping = aes(x = session, y = AUC, group = type))  + geom_line(aes(color=type)) + labs(title = 0.55)
  

