## First things first, we need to create a forecasting function
Forecaster <- function(CorpsData, OpenClass=F) {
	#First of all, we want to know which days we are running the model for
	if (OpenClass == F) {
		#This is a world class corps, so we care about days 50-52
		OutputDays = c(50,51,52)
	} else {
		#This is an open class corps, so we care about days 47,48,50,51
		OutputDays = c(47,48,50,51)
	}
	
	
	#First, compile all the data to forecast with
	VanillaDays = which(!is.na(CorpsData))
	VanillaScores = CorpsData[VanillaDays]
	MomentumDays = VanillaDays[which(VanillaDays > 30)]
	MomentumScores = VanillaScores[which(VanillaDays > 30)]
	
	#Run the vanilla model and get scores and ranges for days 50-52
	VanillaModel = lm(VanillaScores ~ VanillaDays) #make the model
	VanillaSum = summary(VanillaModel) #extract the summary
	FitValues = VanillaSum[[4]] #Get the summary elements we care about
	Van_b = FitValues[2] #Slope of the line
	Van_a = FitValues[1] #Intercept of the line
	Van_r2 = VanillaSum[[9]] #adjusted r^2 for the data
	VanSDE = 1 - sqrt(1-Van_r2); VanSDR = 1 - VanSDE
	#Find the standard deviation of the residuals
	Vsigma = sd(Reduce(c,VanillaSum[[3]]))
	#Find the magnitude of expected random variation
	VRandVar = VanSDR*Vsigma
	#Now we can run the vanilla model for the days we care about
	VanillaMean = Van_a + Van_b*OutputDays; #print(VanillaMean)
	
	## Now we do the exact same thing, but with momentum data
	#We need to check to see if there's data for the momentum model (OC corps)
	if (length(MomentumScores > 2)) {
		MomentModel = lm(MomentumScores ~ MomentumDays)
		MomentSum = summary(MomentModel)
		FitValues = MomentSum[[4]]
		Mom_b = FitValues[2]; Mom_a = FitValues[1]
		Mom_r2 = MomentSum[[9]]
		MomSDE = 1 - sqrt(1-Mom_r2); MomSDR = 1-MomSDE
		Msigma = sd(Reduce(c,MomentSum[[3]]))
		MRandVar = MomSDR*Msigma
		#Run the momentum model for the days we care about
		MomentumMean = Mom_a + Mom_b*OutputDays; #print(MomentumMean)
	} else {
		MomentumMean = VanillaMean
		MRandVar = VRandVar
	}
	#Clear the NA and Nan from the error
	if (is.nan(MRandVar) == T | is.na(MRandVar) == T) {MRandVar = VRandVar}
	
	#Now we output the results
	MeanVec = c(VanillaMean,MomentumMean); #print(MeanVec)
	RandVec = c(rep(VRandVar,length(OutputDays)),rep(MRandVar,length(OutputDays)))
	#Combine the vectors into a data frame
	FrameOut = data.frame(MeanVec,RandVec)
	names(FrameOut) = c('Projection','Interval')
	#return the output
	return(FrameOut);
}

##First, we need to read in the files
Music = read.csv('DCI_2016_Music.csv', row.names=1)
Visual = read.csv('DCI_2016_Visual.csv', row.names=1)
Effect = read.csv('DCI_2016_Effect.csv', row.names=1)
CorpsNames = row.names(Music)

#Create a list of indices that are open class
OCInds = c(1,2,10,13,14,15,16,18,19,22,27,28,30,31,32)
WCInds = c(3,4,5,6,7,8,9,11,12,17,20,21,23,24,25,26,29,33,34,35,36,37)
OCNames = CorpsNames[OCInds]
WCNames = CorpsNames[WCInds]

MusicResults = vector(mode='list',length=37); names(MusicResults) = CorpsNames
VisualResults = vector(mode='list',length=37); names(VisualResults) = CorpsNames
EffectResults = vector(mode='list',length=37); names(EffectResults) = CorpsNames
FullResults = vector(mode='list',length=37); names(FullResults) = CorpsNames

#Compile the forecasts for the Open Class corps
for (i in OCInds) {
	#Get the data for the corps
	CorpsMusic = Reduce(c,Music[i,])
	CorpsVisual = Reduce(c,Visual[i,])
	CorpsEffect = Reduce(c,Effect[i,])
	
	#Run the simulations (OC = TRUE)
	MusicFrame = Forecaster(CorpsMusic, OpenClass=T)
	VisualFrame = Forecaster(CorpsVisual, OpenClass=T)
	EffectFrame = Forecaster(CorpsEffect, OpenClass=T)
	ScoreFrame = MusicFrame+VisualFrame+EffectFrame
	
	#Add the frames to the corps list
	MusicResults[[i]] = MusicFrame
	VisualResults[[i]] = VisualFrame
	EffectResults[[i]] = EffectFrame
	FullResults[[i]] = ScoreFrame
}

#Compile the forecasts for the world class corps
for (i in WCInds) {
	#Get the data
	CorpsMusic = Reduce(c,Music[i,])
	CorpsVisual = Reduce(c,Visual[i,])
	CorpsEffect = Reduce(c,Effect[i,])
	
	#Run the simulations (OC = FALSE)
	MusicFrame = Forecaster(CorpsMusic)
	VisualFrame = Forecaster(CorpsVisual)
	EffectFrame = Forecaster(CorpsEffect)
	ScoreFrame = MusicFrame+VisualFrame+EffectFrame
	
	#Add the frames to the corps list
	MusicResults[[i]] = MusicFrame
	VisualResults[[i]] = VisualFrame
	EffectResults[[i]] = EffectFrame
	FullResults[[i]] = ScoreFrame
}

##Now we just have to compare all the scores for the shows we care about
	#We'll have 3 forecasts per show - lots to digest!
#Day 47 - OCP - for all Open Class corps
#Day 48 - OCF - for top 12 from Day 47
#Day 50 - PRE - for all corps
#Day 51 - SEM - for the top 25 from Day 50
#Day 52 - FIN - for the top 12 from Day 51

#establish common name vectors to use later
ColNames = c('Score','Error')

#Start by forecasting Day 47 - the first day for all the Open Class corps
OCP_V = matrix(ncol=2,nrow=length(OCInds))
OCP_M = matrix(ncol=2,nrow=length(OCInds))
RelevantFrames = FullResults[OCInds]
for (N in 1:length(RelevantFrames)) {
	OCP_V[N,] = Reduce(c,RelevantFrames[[N]][1,])
	OCP_M[N,] = Reduce(c,RelevantFrames[[N]][5,])
}
#Now we add the names to the predictions so we can see them, and sort by score
OCP_VM = (OCP_V + OCP_M)/2

colnames(OCP_V) = ColNames; rownames(OCP_V) = OCNames
colnames(OCP_M) = ColNames; rownames(OCP_M) = OCNames
colnames(OCP_VM) = ColNames; rownames(OCP_VM) = OCNames
OCP_V = OCP_V[order(OCP_V[,1], decreasing=T),]
OCP_M = OCP_M[order(OCP_M[,1],decreasing=T),]
OCP_VM = OCP_VM[order(OCP_VM[,1],decreasing=T),]

#Now we forecast Day 48 - chosen by the top 12 from OCP
	#Luckily the models don't disagree on who will be in top12
OCPOrder = row.names(OCP_VM)
OCFinalists = OCPOrder[1:12]
OCF_V = matrix(ncol=2,nrow=12)
OCF_M = matrix(ncol=2,nrow=12)
#Loop through each one and find their score for Day 48
for (N in 1:12) {
	Name = OCPOrder[N]
	Frame = RelevantFrames[[which(names(RelevantFrames) == Name)]]
	#Find the data
	OCF_V[N,] = Reduce(c,Frame[2,])
	OCF_M[N,] = Reduce(c,Frame[6,])
}
OCF_VM = (OCF_V + OCF_M)/2

colnames(OCF_V) = ColNames; rownames(OCF_V) = OCFinalists
colnames(OCF_M) = ColNames; rownames(OCF_M) = OCFinalists
colnames(OCF_VM) = ColNames; rownames(OCF_VM) = OCFinalists
OCF_V = OCF_V[order(OCF_V[,1], decreasing=T),]
OCF_M = OCF_M[order(OCF_M[,1],decreasing=T),]
OCF_VM = OCF_VM[order(OCF_VM[,1],decreasing=T),]

#Day 50 - everybody's in
PRE_V = matrix(ncol=2,nrow=37)
PRE_M = matrix(ncol=2,nrow=37)
#Start with open class (third show)
for (N in OCInds) {
	PRE_V[N,] = Reduce(c,FullResults[[N]][3,])
	PRE_M[N,] = Reduce(c,FullResults[[N]][7,])
}
#Now do the world class corps (first show)
for (N in WCInds) {
	PRE_V[N,] = Reduce(c,FullResults[[N]][1,])
	PRE_M[N,] = Reduce(c,FullResults[[N]][4,])
}
#Now name and order the frames
PRE_VM = (PRE_V + PRE_M)/2
colnames(PRE_V) = ColNames; rownames(PRE_V) = CorpsNames
colnames(PRE_M) = ColNames; rownames(PRE_M) = CorpsNames
colnames(PRE_VM) = ColNames; rownames(PRE_VM) = CorpsNames
PRE_V = PRE_V[order(PRE_V[,1], decreasing=T),]
PRE_M = PRE_M[order(PRE_M[,1], decreasing=T),]
PRE_VM = PRE_VM[order(PRE_VM[,1],decreasing=T),]

#Now we choose the top 25 and run the semifinals scores
	#All 3 models predict different things
#First, we'll get the top 25 for each one
VTop25 = rownames(PRE_V)[1:25]
MTop25 = rownames(PRE_M)[1:25]
VMTop25 = rownames(PRE_VM)[1:25]

#Loop through each of the top 25 and find the semis scores
SEM_V = matrix(ncol=2,nrow=25)
SEM_M = matrix(ncol=2,nrow=25)
SEM_VM = matrix(ncol=2,nrow=25)
for (N in 1:25) {
	#Get the corps name
	VName = VTop25[N]
	MName = MTop25[N]
	VMName = VMTop25[N]
	
	if (VName %in% OCNames == TRUE) {
		FRInd = which(names(FullResults) == VName)
		#Corps is an open class corps
		SEM_V[N,] = Reduce(c,FullResults[[FRInd]][4,])
	} else {
		FRInd = which(names(FullResults) == VName)
		#Corps in a world class corps
		SEM_V[N,] = Reduce(c,FullResults[[FRInd]][2,])
	}
	#Do the same loop structure for MName and VMName
	if (MName %in% OCNames == TRUE) {
		FRInd = which(names(FullResults) == MName)
		#Corps is an open class corps
		SEM_M[N,] = Reduce(c,FullResults[[FRInd]][8,])
	} else {
		FRInd = which(names(FullResults) == MName)
		#Corps in a world class corps
		SEM_M[N,] = Reduce(c,FullResults[[FRInd]][5,])
	}
	#We need to change the inside of the loop for the VM model scores
	if (VMName %in% OCNames == TRUE) {
		#Corps is an open class corps
		FRInd = which(names(FullResults) == VMName)
		Vscore = Reduce(c,FullResults[[FRInd]][4,])
		Mscore = Reduce(c,FullResults[[FRInd]][8,])
		#include the VM score
		SEM_VM[N,] = (Vscore+Mscore)/2
	} else {
		#Corps in a world class corps
		FRInd = which(names(FullResults) == VMName)
		Vscore = Reduce(c,FullResults[[FRInd]][2,])
		Mscore = Reduce(c,FullResults[[FRInd]][5,])
		SEM_VM[N,] = (Vscore+Mscore)/2
	}
}
#Now we have to order the semis scores
colnames(SEM_V) = ColNames; rownames(SEM_V) = VTop25
SEM_V = SEM_V[order(SEM_V[,1],decreasing=T),]
colnames(SEM_M) = ColNames; rownames(SEM_M) = MTop25
SEM_M = SEM_M[order(SEM_M[,1],decreasing=T),]
colnames(SEM_VM) = ColNames; rownames(SEM_VM) = VMTop25
SEM_VM = SEM_VM[order(SEM_VM[,1],decreasing=T),]

#On to finals
	#Vanilla and VM agree, but not M
VTop12 = rownames(SEM_V)[1:12]
MTop12 = rownames(SEM_M)[1:12]
FIN_V = matrix(ncol=2,nrow=12)
FIN_M = matrix(ncol=2,nrow=12)
FIN_VM = matrix(ncol=2,nrow=12)
#Loop through the 12 names in each one and get the scores
for (N in 1:12) {
	VName = VTop12[N]
	FRInd = which(names(FullResults) == VName)
	FIN_V[N,] = Reduce(c,FullResults[[FRInd]][3,])
	Mscores = Reduce(c,FullResults[[FRInd]][6,])
	FIN_VM[N,] = (FIN_V[N,] + Mscores)/2
	
	MName = MTop12[N]
	FRInd = which(names(FullResults) == MName)
	FIN_M[N,] = Reduce(c,FullResults[[FRInd]][6,])
}
#Now we have to order the FINis scores
colnames(FIN_V) = ColNames; rownames(FIN_V) = VTop12
FIN_V = FIN_V[order(FIN_V[,1],decreasing=T),]
colnames(FIN_M) = ColNames; rownames(FIN_M) = MTop12
FIN_M = FIN_M[order(FIN_M[,1],decreasing=T),]
colnames(FIN_VM) = ColNames; rownames(FIN_VM) = VTop12
FIN_VM = FIN_VM[order(FIN_VM[,1],decreasing=T),]

##Write all the show tables to CSV files
write.csv(OCP_V,'DCI_OCP_V.csv')
write.csv(OCP_M,'DCI_OCP_M.csv')
write.csv(OCP_VM,'DCI_OCP_VM.csv')

write.csv(OCF_V,'DCI_OCF_V.csv')
write.csv(OCF_M,'DCI_OCF_M.csv')
write.csv(OCF_VM,'DCI_OCF_VM.csv')

write.csv(PRE_V,'DCI_PRE_V.csv')
write.csv(PRE_M,'DCI_PRE_M.csv')
write.csv(PRE_VM,'DCI_PRE_VM.csv')

write.csv(SEM_V,'DCI_SEM_V.csv')
write.csv(SEM_M,'DCI_SEM_M.csv')
write.csv(SEM_VM,'DCI_SEM_VM.csv')

write.csv(FIN_V,'DCI_FIN_V.csv')
write.csv(FIN_M,'DCI_FIN_M.csv')
write.csv(FIN_VM,'DCI_FIN_VM.csv')
	
	