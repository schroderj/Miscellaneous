# Import.
data = read.table("Data/Robotic Vacuums/Robotic Vacuums Final Data 03-08-16.txt",header=T,sep="\t")

# # Censor respondents who are responding at random (llike < 0.35).
# load(file="Data/Robotic Vacuums/lowlike.RData")
# resp_cen = which(lowlike==TRUE)
# data = data[-resp_cen,]

# Process.
N = dim(data)[1] # Number of respondents.
H = 16           # Number of choice tasks.
P = 4            # Number of alternatives (excluding the outside option).
L = 7            # Number of attributes.

# Setup choice data, each respondent with their own list composed of y and X.
choice_data = NULL
for (n in 1:N) {
  # Pull the choices.
  y = cbind(data$CBC1_Choice1[n],data$CBC1_Choice2[n],data$CBC1_Choice3[n],data$CBC1_Choice4[n],
            data$CBC1_Choice5[n],data$CBC1_Choice6[n],data$CBC1_Choice7[n],data$CBC1_Choice8[n],
            data$CBC1_Choice9[n],data$CBC1_Choice10[n],data$CBC1_Choice11[n],data$CBC1_Choice12[n],
            data$CBC1_Choice13[n],data$CBC1_Choice14[n],data$CBC1_Choice15[n],data$CBC1_Choice16[n])
  
  # Pull the design.
  X_raw = as.character(data$sys_CBC_CBC1_design[n])         # Convert concatenated items to character.
  X_raw = gsub("[",X_raw,fixed=TRUE,replacement="")         # Remove "[" brackets.
  X_raw = gsub("]",X_raw,fixed=TRUE,replacement="")         # Remove "]" brackets.
  X_raw = strsplit(X_raw,split="[,]")                       # Split by commas ",".
  X_raw = matrix(as.numeric(X_raw[[1]]),byrow=TRUE,nrow=64) # Create X matrix.
  X_raw = X_raw[,-1]                                        # Remove alternative numbering column.
  
  # Dummy-code the design and include the outside good for each choice task.
  X = NULL; X_ncol = c(4,2,2,2,2,2,4)
  for (h in 1:H) {
    X_h = NULL
    X_raw_h = X_raw[(h*P-P+1):(h*P),]
    for (p in 1:P) {
      X_p = NULL
      for (l in 1:L) {
        X_pl = matrix(0,nrow=1,ncol=X_ncol[l])
        X_pl[X_raw_h[p,l]] = 1
        if (l!=1) X_pl = matrix(X_pl[-1],nrow=1)
        X_p = cbind(X_p,X_pl)
      }
      X_h = rbind(X_h,X_p)
    }
    X = rbind(X,rbind(X_h,matrix(0,nrow=1,ncol=ncol(X_h))))
  }
  
  # Save out choice data.
  choice_data[[n]] = list(y=y,X=X)
}

# Save out covariates.
attitudes = cbind(data$CleaningAttitudes_1,data$CleaningAttitudes_2,data$CleaningAttitudes_3,data$CleaningAttitudes_4,
                  data$CleaningAttitudes_5,data$CleaningAttitudes_6,data$CleaningAttitudes_7,data$CleaningAttitudes_8,
                  data$CleaningAttitudes_9,data$CleaningAttitudes_10,data$CleaningAttitudes_11)
sentiment = cbind(data$RoboVacuumSentiment_1,data$RoboVacuumSentiment_2,data$RoboVacuumSentiment_3,data$RoboVacuumSentiment_4,
                  data$RoboVacuumSentiment_5,data$RoboVacuumSentiment_6,data$RoboVacuumSentiment_7)
covariates = list(attitudes=attitudes,sentiment=sentiment)

# Save out brand beliefs.
brand_beliefs = NULL
for (n in 1:N) {
  brand1_bb = c(data$BrandBeliefs_r1_c1[n],data$BrandBeliefs_r2_c1[n],data$BrandBeliefs_r3_c1[n],data$BrandBeliefs_r4_c1[n],
                data$BrandBeliefs_r5_c1[n],data$BrandBeliefs_r6_c1[n],data$BrandBeliefs_r7_c1[n],data$BrandBeliefs_r8_c1[n],
                data$BrandBeliefs_r9_c1[n],data$BrandBeliefs_r10_c1[n])
  brand2_bb = c(data$BrandBeliefs_r1_c2[n],data$BrandBeliefs_r2_c2[n],data$BrandBeliefs_r3_c2[n],data$BrandBeliefs_r4_c2[n],
                data$BrandBeliefs_r5_c2[n],data$BrandBeliefs_r6_c2[n],data$BrandBeliefs_r7_c2[n],data$BrandBeliefs_r8_c2[n],
                data$BrandBeliefs_r9_c2[n],data$BrandBeliefs_r10_c2[n])
  brand3_bb = c(data$BrandBeliefs_r1_c3[n],data$BrandBeliefs_r2_c3[n],data$BrandBeliefs_r3_c3[n],data$BrandBeliefs_r4_c3[n],
                data$BrandBeliefs_r5_c3[n],data$BrandBeliefs_r6_c3[n],data$BrandBeliefs_r7_c3[n],data$BrandBeliefs_r8_c3[n],
                data$BrandBeliefs_r9_c3[n],data$BrandBeliefs_r10_c3[n])
  brand4_bb = c(data$BrandBeliefs_r1_c4[n],data$BrandBeliefs_r2_c4[n],data$BrandBeliefs_r3_c4[n],data$BrandBeliefs_r4_c4[n],
                data$BrandBeliefs_r5_c4[n],data$BrandBeliefs_r6_c4[n],data$BrandBeliefs_r7_c4[n],data$BrandBeliefs_r8_c4[n],
                data$BrandBeliefs_r9_c4[n],data$BrandBeliefs_r10_c4[n])
  brand_beliefs[[n]] = matrix(rbind(brand1_bb,brand2_bb,brand3_bb,brand4_bb),byrow=TRUE,nrow=4)
}

save(choice_data,file="Data/Robotic Vacuums/choice_data.RData")
save(covariates,file="Data/Robotic Vacuums/covariates.RData")
save(brand_beliefs,file="Data/Robotic Vacuums/brand_beliefs.RData")
