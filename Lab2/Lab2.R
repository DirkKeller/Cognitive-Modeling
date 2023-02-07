##### Load packages and Data
library("ggpubr") #install.packages("ggpubr")
library("raster") #install.packages("raster")
library("viridis") #install.packages("viridis"); install.packages("viridisLite")

NeuralResponses <- read.table("~/Cognitive Modeling/Lab2/NeuralResponses", quote="\"", comment.char="")
CategoryVectors <- read.table("~/Cognitive Modeling/Lab2/CategoryVectors", quote="\"", comment.char="")
CategoryLabels <- read.csv("~/Cognitive Modeling/Lab2/CategoryLabels", sep="")

###################################   RDM correlations for the original data ################################ 
SimDataPure_RDM <- data.frame(matrix(c(1:92),ncol=92,nrow=92))

for(row in c(1:nrow(NeuralResponses))){
  for(next_row in c(1:nrow(NeuralResponses))){
    RSM <- cor.test(as.numeric(NeuralResponses[row,]),as.numeric(NeuralResponses[next_row,]),method="pearson")$estimate
    current_col <- paste("X",next_row,sep="")
    SimDataPure_RDM[row,current_col] <- round(1-abs(RSM),4)
  }
}

###################### RDM correlations for simulated 12 participants (including noise) ###################### 
NumSimPart <- 12
SimDataNoise_RDM <- data.frame(matrix(c(1:92),ncol=92,nrow=92))
LabelMatrix <- data.frame(matrix(NaN,ncol=92,nrow=92))

ParList<- list(matrix(NaN,ncol=92,nrow=92)) #(rep(NaN,NumSimPart))

for(participant in c(1:NumSimPart)){
  SimData <- NeuralResponses + rnorm(nrow(NeuralResponses)*ncol(NeuralResponses),0,1)
  
  for(row in c(1:nrow(SimData))){
    for(next_row in c(1:nrow(SimData))){
      RSM <- cor.test(as.numeric(SimData[row,]),as.numeric(SimData[next_row,]),method="pearson")$estimate
      current_col <- paste("X",next_row,sep="")
      SimDataNoise_RDM[row,current_col] <- round(1-abs(RSM), 4)
    }
  }
  ParList[[participant]] <- SimDataNoise_RDM
}

######################  Averaged RDM correlations for simulated 12 participants (including noise)

SimDataNoiseAverage_RDM <- matrix(0, ncol=92,nrow=92)
for (participant in c(1:NumSimPart)) {
  SimDataNoiseAverage_RDM <- SimDataNoiseAverage_RDM + ParList[[participant]]
}
SimDataNoiseAverage_RDM <- SimDataNoiseAverage_RDM/NumSimPart

##### GRAPH: original data RDM correlations
cuts <- c(0:100/100)
plot(raster(data.matrix(SimDataPure_RDM)),main="RDM original data",breaks=cuts,col=viridis(100),xlab="Correlation between pictures 0 to 92",ylab="Correlation between pictures 0 to 92")

##### GRAPH: model subject S3 RDM correlations
plot(raster(data.matrix(ParList[[3]])),main="RDM model participant 3",breaks=cuts,col=viridis(100),xlab="Correlation between pictures 0 to 92",ylab="Correlation between pictures 0 to 92")

##### GRAPH: averaged model subjects RDM correlations
plot(raster(data.matrix(SimDataNoiseAverage_RDM)),main="RDM averaged model data",breaks=cuts,col=viridis(100),xlab="Correlation between pictures 0 to 92",ylab="Correlation between pictures 0 to 92")
