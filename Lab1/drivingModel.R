## Cognitive Model of dialing while driving task for multiple phone number representations
## Developed by Christian P. Janssen of Utrecht University
## This model is described in more detail in Janssen & Brumby (2010, Cognitive Science)
## However, the model is meant as an exercise in class, so not all components are included (specifically: the part that explores *all* strategies)
## If you use this model, please cite the paper: Janssen, C. P., & Brumby, D. P. (2010). Strategic Adaptation to Performance Objectives in a Dual‐Task Setting. Cognitive Science, 34(8), 1548–1560. http://doi.org/10.1111/j.1551-6709.2010.01124.x

## for questions, please contact Christian P. Janssen: c.p.janssen@uu.nl
## www.cpjanssen.nl


require(gtools)




##global parameters


### parameters related to steering
steeringTimeOptions <- c(1,2,3,4,5,6,7,8,9,10,11,12)    #list op options for how many steering corrections can be made each time that attention is paid to steering (of steeringUpdateTime sec each) (this influences the strategy alternatives)
steeringUpdateTime <- 250    #in milliseconds
startingPositionInLane <- 0.27 			#assume that car starts already away from lane centre (in meters)


#parameters for deviations in car drift due the simulator environment: See Janssen & Brumby (2010) page 1555
gaussDeviateMean <- 0
gaussDeviateSD <- 0.13

#When the car is actively contorlled, we calculate a value using equation (1) in Janssen & Brumby (2010). However, some noise is added on top of this equation to account for variation in human behavior. See Janssen & Brumby (2010) page 1555. Also see function "updateSteering" on how this function is used
gaussDriveNoiseMean <- 0
gaussDriveNoiseSD <- 0.1	#in meter/sec

timeStepPerDriftUpdate <- 50 ### msec: what is the time interval between two updates of lateral position?


maxLateralVelocity <- 1.7	#maximum lateral velocity: what is the maximum that you can steer?
minLateralVelocity <- -1* maxLateralVelocity

startvelocity <- 0 	#a global parameter used to store the lateral velocity of the car


### all times in milliseconds

## times for dialing
singleTaskKeyPressTimes <- c(400,400,400,400,400,400,400,400,400,400,400) #c(228.8625, 228.8625, 228.8625, 228.8625, 228.8625, 228.8625, 228.8625, 228.8625, 228.8625, 228.8625, 228.8625)   #digit times needed per keypress at that specific position (note: normalized for chunk retrieval time at digits 1 and 6 --- a retrieval cost would come on top of this)


digitTypeUK <- c("chunk","oth","oth","oth","oth","chunk","oth","oth","oth","oth","oth")  ### is each digit either the start of a chunk or some other digit?


#### parameters related to task switching: see Janssen & Brumby page 1556
chunkRetrievalTime <- 100    ## extra time needed to retrieve first digit of a chunk (100 msec in Janssen & Brumby 2010). This time cost is ALWAYS incurred
stateInformationRetrievalTime <- 100 #time required to retrieved state information if the FIRST digit of a sequence of keypresses is not at chunk boundary (100 in Janssen & Brumby paper). This cost is only incurred when you switch at a position that differs from the chunk boundary
switchCost <- 200      ### Janssen & Brumby 2010: time needed when you switch back to dialing after driving (always incurred when switching)

####### startTime <- 500 	#msec; time before starting to retrieve digit after start of trial (i.e., after car has been driven for a while)


simpleSetOfStrategyVariations <- TRUE		#set to true if only steeringTimeOptions in the form {2,2,2,2,2}, {4,4,4,4,4},etc are used and not {2,4,2,2,4},etc. That is: if a consistent number of digits is dialed each moment when attention is paid to dialing.

giveDetailedOuput <- FALSE  ## how detailed is the output; at the individual keypress level, or average per trial?


### use this function to analyze the human data if you want to
analysisOfHumanData <- function()
{
	### load the relevant data files
	
	
	### calculate the necessary averages and SDs
	

	
}




##new function
runOneTrial <- function(strategy,nrSteeringUpdates,normalPhoneStructure,phoneStringLength,phoneNumber)   #### strategy stores where participant interleaves
{
	
	#first make vector variables to store detailed output of the model. Note that each vector has a starting value
	times <- c(0)  
	events <- c("none")  ### events will log what happens, for example a keypress or nothing at all ("none"). Later these can be used to filter out specific events
	drifts <- c(startingPositionInLane)   ### where does the car start in the lane? At the start of a trial at the startingPositionInLan position
	newVelocity <- startvelocity         ### what is the start velocity of the car?
	
	
	##calculate basic dialing times, to be used later
	dialTimes <- singleTaskKeyPressTimes  ##if there is no interleaving, this is the single-task interkeypress interval time
	
	### at chunk positions: add a chunk retrieval cost time to the dial times, as (assumption) this is a time where you are not paying attention to the driving
	for (chunkPosition in normalPhoneStructure)
	{
		dialTimes[chunkPosition] <- dialTimes[chunkPosition] + chunkRetrievalTime
	}	
		
		
	### now go through the various numbers
	for (digitindex in 1:length(dialTimes))
	{

		### determine dial time, so additional costs can be added later
		locDialTime <- dialTimes[digitindex]
			
			
		if (length(which(strategy== digitindex)))  ### if this is a position where you switch, then switch
		{
			## experience switch cost
			time <- switchCost          #switching between dialing & driving
			times <- updateTimestampslist(times, time)   #### Run a function that determines at what time points a drift update is made. Note that for the first digit, "times" only contains the value 0
			driftOutput <- calculateLaneDrift(drifts[length(drifts)], newVelocity, time)   ### now also calculate the drift for these time intervals
			newVelocity <- driftOutput[1]  ## determine the new velocity
			drifts <- c(drifts,driftOutput[2:length(driftOutput)])   ### add these drifts to the table
			events <- c(events,rep("switch1",(length(driftOutput)-1)))  ### also update the events
			
			### after switching you perform corrective driving. For this we use the "updateSteering" function
			lastDrift <- drifts[length(drifts)]
			steerOutput <- updateSteering(newVelocity,nrSteeringUpdates,lastDrift)
			newVelocity <- steerOutput[1]
			drifts <- c(drifts,steerOutput[2:length(steerOutput)])
			events <- c(events,rep("steer",(length(steerOutput)-1)))
			times <- updateTimestampslist(times,(nrSteeringUpdates* steeringUpdateTime))
			

				
			### now switch back to dialing the number (using the drift parameters for distracted driving). First, you incur some time due to switching from driving to dialing
			time <- switchCost          #first: incur a switch cost for switching between dialing to driving
			times <- updateTimestampslist(times, time)
			driftOutput <- calculateLaneDrift(drifts[length(drifts)], newVelocity, time)
			newVelocity <- driftOutput[1]
			drifts <- c(drifts,driftOutput[2:length(driftOutput)])
			events <- c(events,rep("switch2",(length(driftOutput)-1)))
			
			
			
			
			#### if you are NOT switching at a chunk boundary (i.e., at one of the indexes of normalPhoneStructure), then experience additional retrieval cost. This is again time that you are distracted
			if(length(which(normalPhoneStructure == digitindex)) ==0)
			{
				locDialTime <- locDialTime + stateInformationRetrievalTime
			}	
			
			
			
		}
	
		##now calculate drift for typing a digit (NOTE: this is always done for every digit that is typed, regardless of whether you were retrieving a chunk or not)
		time <- locDialTime
		times <- updateTimestampslist(times, time)
		driftOutput <- calculateLaneDrift(drifts[length(drifts)], newVelocity, time)
		newVelocity <- driftOutput[1]
		drifts <- c(drifts,driftOutput[2:length(driftOutput)])
		events <- c(events,rep("none",(length(driftOutput)-2)))
		events <- c(events,"keypress")
		

		
	}  #end for digit index

	
	table <- data.frame(times,events,drifts)
	
	#with(table[table$events == "keypress",],plot(times,drifts,ylim=c(-2,2)))

	table ### return the table

}





#### Main function to run the code. For example: runAllSimpleStrategies(5,"07854325698") will run 5 simulations for each (simple) strategy on the phone number to the right. The default assumption is that the chunk boundary is between the 5th and 6th digit

runAllSimpleStrategies <- function(nrSimulations,phoneNumber)
{
	
	
	normalPhoneStructure <- c(1,6)  ### indicate at what digit positions a chunk needs to be retrieved (1st and 6th digit)
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
				locTab <- runOneTrial(, steerTimes,normalPhoneStructure,phoneStringLength,phoneNumber)
	
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
	
	
	#### make a plot that visualizes all the strategies: note that trial time is divided by 1000 to get the time in seconds
	with(agrResultsMeanDrift,plot(TrialTime/1000,abs(dev),pch=21,bg="dark grey",col="dark grey",log="x",main=paste('Steering-Dialing-Trade-off:', nrSimulations, 'Simulation(s)', '\n', 'for all possible Interleaving Strategies'),xlab="Dial time (s)",ylab="Average Lateral Deviation (m)"))
	
	
	### give a summary of the data	
	summary(agrResultsMeanDrift$TrialTime)
}



	
### function that generates the points at which car data should be collected (specifically: if you know that a keypress happens after a specific time, then find out at what points a drift update occurs, this depends on the ength of "timeStepPerDriftUpdate" (50 msec by default))	
updateTimestampslist <- function(timestampsList, totalTime)
{
	lastTime <- timestampsList[length(timestampsList)]
	newTimes <- cumsum(c(lastTime,rep(timeStepPerDriftUpdate ,trunc(totalTime/timeStepPerDriftUpdate))))[-1]
		
	timestampsList <- c(timestampsList, newTimes)
		
	if (totalTime%%timeStepPerDriftUpdate > 0)
	{
		newTime <- timestampsList[length(timestampsList)] + totalTime%%timeStepPerDriftUpdate
		timestampsList <- c(timestampsList, newTime)
	}
	timestampsList
	
}
	
	
	
	
	
	
	
	
### This function calculates how much the car drifts during episodes where the driver/model is not actively driving
calculateLaneDrift <- function(startPositionOfDrift, startVelocityOfDrift, driftTimeInMilliSeconds)
{
	laneDriftList <- c()  ### keep a list of lane positions
	#locVelocity <- velocity	#velocity is a global variable
	
	locVelocity <- startVelocityOfDrift
	
	lastLaneDrift <- startPositionOfDrift
	
	
	for (i in 1:(trunc(driftTimeInMilliSeconds/timeStepPerDriftUpdate)))
	{
		locVelocity <- locVelocity + rnorm(1,gaussDeviateMean,gaussDeviateSD)
		
		### make sure velocity is not higher than max
		locVelocity <- velocityCheck(locVelocity)
				
		lastLaneDrift <- lastLaneDrift + locVelocity* timeStepPerDriftUpdate / 1000     #velocity is in m/second
				
		
		
		#laneDriftList <- c(laneDriftList, lastLaneDrift)
		laneDriftList <- c(laneDriftList, abs(lastLaneDrift))    ### only absolute values
		
	}
	

	
	#now do drift for last few milliseconds (using modulo function)
	locVelocity <-locVelocity + rnorm(1,gaussDeviateMean,gaussDeviateSD)
	### make sure velocity is not higher than max
	locVelocity <- velocityCheck(locVelocity)
	
	if (driftTimeInMilliSeconds%% timeStepPerDriftUpdate > 0)
	{
	
		lastLaneDrift <- lastLaneDrift + locVelocity*(driftTimeInMilliSeconds%% timeStepPerDriftUpdate)/1000
		
		
		#laneDriftList <- c(laneDriftList, lastLaneDrift)
		laneDriftList <- c(laneDriftList, abs(lastLaneDrift))  ### only absolute values
		
	}
	#velocity <<- locVelocity
	#laneDrift

	returnValues <- c(locVelocity, laneDriftList) 
	returnValues
}





##calculates if the car is not accelerating more than it should (maxLateralVelocity) or less than it should (minLateralVelocity)
velocityCheck <- function(localVelocity)
{
	localVelocity <- min(localVelocity, maxLateralVelocity)
	localVelocity <- max(localVelocity, minLateralVelocity)
	
	localVelocity
	
}


##calculates if the car is not accelerating more than it should (maxLateralVelocity) or less than it should (minLateralVelocity)  (done for a vector of numbers)
velocityCheckForVectors <- function(velocityVectors)
{
	
	velocityVectors[which(velocityVectors > maxLateralVelocity)] <- maxLateralVelocity
	velocityVectors[which(velocityVectors < minLateralVelocity)] <- minLateralVelocity
	
	velocityVectors
	
}




### this function is used to update the velocity (and in effect lateral lane position) when the driver/model is actively driving
updateSteering <- function(velocity,nrUpdates,startPosLane)
{
	locDrifts <- c()
	
	localVelocity <- velocity
	
	for (steers in 1: nrUpdates)
	{
		localLanePos <- startPosLane
		
		if (steers > 1)
		{
			localLanePos <- locDrifts[length(locDrifts)]
		}
		
		
		### update direction every 250 milliseconds. Following equation (1) in Janssen & Brumby (2010)
		updateVelocity <- 0.2617 * localLanePos ^2 + 0.0233* localLanePos - 0.022  #velocity in meter/sec
		updateVelocity <- updateVelocity + rnorm(1, gaussDriveNoiseMean, gaussDriveNoiseSD)    ### a noise value is added for driving (only done once)
		updateVelocity <- velocityCheck(updateVelocity)
		
		###calculate updates locally (i.e., add some noise to updateVelocity, but do not make it transfer to other values)
		## calculate using cumsum to save computer time :-)

		nrUpdatesOf50Msec <- steeringUpdateTime/timeStepPerDriftUpdate
		velocityVector <- rnorm(nrUpdatesOf50Msec,(updateVelocity + gaussDeviateMean), gaussDeviateSD)
		velocityVector  <- velocityCheckForVectors(velocityVector)

		
		directionUpdates <- -1 * velocityVector * 0.050    ##only driving for 0.050 seconds
		newDrifts <- cumsum(c(localLanePos, directionUpdates))
		newDrifts  <- newDrifts[2:length(newDrifts)]
		
		locDrifts <- c(locDrifts , abs(newDrifts))   #### only absolute values
					
	}
	returnValues <- c(updateVelocity,locDrifts)
	
	
}
