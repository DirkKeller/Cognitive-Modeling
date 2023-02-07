################################################################################    
############################### Optional #######################################  
############################## Exercise 0 ######################################   

# Group 14, Juijn, G. (Guusje), Roijen, A. (Andrea), Garzoni di Adorgnano, M. (Massimiliano), Keller (Dirk)
# using 50 simulations, but it will take a long time to compute them

library(dplyr)
library(ggplot2)
source(file.path("C:\\Users\\DK\\Master. Artificial Intelligence\\Cognitive Modeling\\Lab1\\drivingModel.R")) 

# load the chunk combination data set (from python)
chunk_combinations <- read.csv("C:\\Users\\DK\\Master. Artificial Intelligence\\Cognitive Modeling\\Lab1\\chunk_combinations.csv", header=TRUE, sep = ",", colClasses = 'character')
chunk_combinations <- subset(chunk_combinations, select = -X)
chunk_combinations <- rbind(c('07854325698','','','','','','','','','',''), chunk_combinations)
colnames(chunk_combinations) <- c('1','2','3','4','5','6','7','8','9','10','11')

# calcualtes the chunk position, based on the chunking structure (from chunk_combinations)
get_chunk_pos <- function(chunk_combinations, strat){
  normalPhoneStructure <- c()
  for (chunk in 1:ncol(chunk_combinations)){
    
    # if the next chunk contains no digits end 
    if(identical(chunk_combinations[strat, chunk + 1],'')){
      break
    }
    # determine the start-chunk-position of the first chunk, current or next chunk should not be empty
    if (!identical(chunk_combinations[strat, chunk],'') && chunk == 1 && !identical(chunk_combinations[strat, chunk + 1],'')){
      normalPhoneStructure <- c(normalPhoneStructure, nchar(chunk_combinations[strat, chunk]))
    }
    # determine the start-chunk-position of the next chunk, current chunk should not be empty
    if (!identical(chunk_combinations[strat, chunk],'') && chunk > 1){
      normalPhoneStructure <- c(normalPhoneStructure, nchar(chunk_combinations[strat, chunk]) + normalPhoneStructure[length(normalPhoneStructure)])
    }
  }
  return(normalPhoneStructure)
}

# save the chunk positions in a dataframe
BreakPoints <- list() #as.data.frame(matrix(nrow = nrow(chunk_combinations), ncol = ncol(chunk_combinations)))
for (chunkComb in 1:nrow(chunk_combinations)){
  #BreakPoints[chunkComb,] <- rbind(BreakPoints, get_chunk_pos(chunk_combinations, chunkComb))
  chunkstructure <- get_chunk_pos(chunk_combinations, chunkComb)
  BreakPoints <- c(BreakPoints, list(chunkstructure)) 
}

############# Adjusted version of runAll SimpleStrategies ######################
runAllComplexStrategies <- function(nrSimulations, phoneNumber, chunk_combinations)
{
  agrFinalResultsMeanDrift <- data.frame()
  for (chunkComb in 1:nrow(chunk_combinations)){
    normalPhoneStructure <- get_chunk_pos(chunk_combinations, chunkComb)  ### indicate at what digit positions a chunk needs to be retrieved (1st and 6th digit)
    phoneStringLength <- 11   ### how many digits does the number have?
    
    
    ### vectors that will contain output of the simulation. These are later used to create 1 table with all values
    keypresses <- c()
    times <- c()
    deviations <- c()
    strats <- c()
    steers <- c()	
    
    ### iterate through all strategies
    ## in this simple model we assume that a participant uses a consistent strategy throughout the trial. That is, they only type each time 1 digit, or type 2 digits at a time, or type 3 digits at a time (i.e., all possible ways of 1:phoneStringLength: 1, 2,3,4, ...11)
    for (nrDigitsPerTime in 1: phoneStringLength)
    {
      ## quick way of calculating positions to interleave: repeat strategy & multiply with position in vector (e.g., 333*123 = 369 this means: you interleave BEFORE every 3rd digit (333), and there are 3 positions to interleave (1st, 2nd, 3rd, or 123). Therefore you interleave BEFORE digits 3 (3*1), 6 (3*2), and 9 (3*3))
      
      if (nrDigitsPerTime != 11)
      {
        strategy <- rep(nrDigitsPerTime ,floor(phoneStringLength/nrDigitsPerTime))  ### stores at which positions the number is interleaved
        positions <- 1:length(strategy)
        strategy <- strategy * positions
        
        ### remove last digit, as driver does not interleave after typing the last digit (they are done with the trial :-)  )
        strategy <- strategy[strategy != phoneStringLength]
      }
      else
      {
        strategy <- c()	
        
      }
      
      
      locSteerTimeOptions <- steeringTimeOptions
      if (length(strategy) == 0)
      {
        locSteerTimeOptions <- c(0)
      }
      
      
      
      ### now run a trial (runOneTrial) for all combinations of how frequently you update the steering when you are steering (locSteerTimeOptions) and for the nuber of simulations that you want to run for each strategy (nrSimulations)
      for (steerTimes in locSteerTimeOptions)
      {
        for (i in 1:nrSimulations)
        {
          
          ### run the simulation and store the output in a table
          locTab <- runOneTrial(strategy, steerTimes,normalPhoneStructure,phoneStringLength,phoneNumber)
          
          ##only look at rows where there is a keypress
          locTab <- locTab[locTab$events == "keypress",]
          
          ### add the relevant data points to variables that are stored in a final table
          keypresses <- c(keypresses,1:nrow(locTab))
          times <- c(times,locTab$times)
          deviations <- c(deviations,locTab$drifts)
          strats <- c(strats,rep(nrDigitsPerTime,nrow(locTab)))
          steers <- c(steers,rep(steerTimes,nrow(locTab)))
          
        }
      }#end of for steerTimes	
      
    }##end of for nr strategies
    
    
    ### now make a new table based on all the data that was collected
    tableAllSamples <- data.frame(keypresses,times,deviations,strats,steers)
    
    
    #### In the table we collected data for multiple simulations per strategy. Now we want to know the average performane of each strategy.
    #### These aspects are calculated using the "aggregate" function
    
    
    ## calculate average deviation at each keypress (keypresses), for each unique strategy variation (strats and steers)
    agrResults <- with(tableAllSamples,aggregate(deviations,list(keypresses=keypresses, strats= strats, steers= steers),mean))
    agrResults$dev <- agrResults$x
    
    
    ### also calculate the time interval
    agrResults$times <- with(tableAllSamples,aggregate(times,list(keypresses=keypresses, strats= strats, steers= steers),mean))$x
    
    
    ###now calculate mean drift across the trial
    agrResultsMeanDrift <-  with(agrResults,aggregate(dev,list(strats= strats, steers= steers),mean))
    agrResultsMeanDrift$dev <- agrResultsMeanDrift$x
    
    ### and mean trial time
    agrResultsMeanDrift$TrialTime <-  with(agrResults[agrResults$keypresses ==11,],aggregate(times,list( strats= strats, steers= steers),mean))$x	
    
    ### add the interleaving strategy
    agrResultsMeanDrift <- cbind(IntleavStrat = rep(toString(normalPhoneStructure), each=nrow(agrResultsMeanDrift)), agrResultsMeanDrift)
    
    ### build the final table with all strategies
    agrFinalResultsMeanDrift <- rbind(agrFinalResultsMeanDrift, agrResultsMeanDrift)
  }
  return(agrFinalResultsMeanDrift)
}

# run the complex simulation that calls get_chunk_pos to get all combinations of chunk positions 
SimDataStrat <- runAllComplexStrategies(nrSimulations = 50, phoneNumber = '07854325698', chunk_combinations)
SimDataStrat$NatBreakPoint <- c('Other strategies')
SimDataStrat$NatBreakPoint[SimDataStrat$IntleavStrat == '1, 6'] <- c('Chunk interleaving Strategy only')

############# Plotting the simulated data for all strategies ###################
Fig_SimDataStrat <- ggplot2::ggplot(data = SimDataStrat,
                                    mapping = aes(x = TrialTime/1000,
                                                  y = abs(dev))) +
  geom_point() +
  geom_point(data = subset(SimDataStrat, SimDataStrat$NatBreakPoint == 'Chunk interleaving Strategy only'),
             mapping = aes(x = TrialTime/1000,
                           y = abs(dev)),
             shape = 4,
             color ='blue') +
  labs(title = "Simulated dialling-time and RMSE lateral deviation for modeled strategies") +
  labs(x = 'Dialing Time (sec)',
       y = 'RMSE  Lateral Lane Deviation (m)') +
  theme_classic() + 
  theme(axis.title.x = element_text(color = 'black',
                                    size = 12, face = 'bold'),
        axis.title.y = element_text(color = 'black',
                                    size = 12, face = 'bold'),
        legend.position = NaN,
        panel.grid.major.x = element_line(color = 'gray90',
                                          size = 0.3,
                                          linetype = 1),
        panel.grid.major.y = element_line(color = 'gray90',
                                          size = 0.3,
                                          linetype = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = 'gray99')) 
Fig_SimDataStrat