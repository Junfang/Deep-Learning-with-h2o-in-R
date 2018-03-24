




## read con file
conFile = read.table('./random/random_con.txt', stringsAsFactors=F, header=TRUE)

str(conFile)
head(conFile)
str(unique(conFile[,1]))
conFileUniq = conFile[!duplicated(conFile[,1]), 1:3]
dim(conFileUniq)
head(conFileUniq)


txtFiles = system(paste0("ls *.txt"), intern=TRUE)
# txtFiles2 = txtFiles[-c(2:4)]
sampleIDs = c()
metric <- c()
metric.RT <- c()
for (i in txtFiles){

	print(i)
	sampleID = as.numeric(gsub("([0-9]+).*$", "\\1", i)) 

	txtFtmp = read.table(i, fill=TRUE, stringsAsFactors=F) 
	dim(txtFtmp)
	# head(txtFtmp)
	## remove first 3 lines 
	txtF = txtFtmp[-c(1:3),]
	### only extract 3 columns
	txtF3col = txtF[,c(3,4,5)]
	# head(txtF3col)
	## remove if rows with 'Pulse'
	txtF3colNOpulse = txtF3col[!is.element(txtF3col[,1],'Pulse'),]
	dim(txtF3colNOpulse)
	# head(txtF3colNOpulse,11)
	## remove if rows with 'fix'
	txtF3colNOpulseNOfix = txtF3colNOpulse[!is.element(txtF3colNOpulse[,2],'fix'),]
	dim(txtF3colNOpulseNOfix)
	# head(txtF3colNOpulseNOfix,11)

	## Add two other columns; ready for Ethnicity and Team; assign ZERO values.
	txtF3colNOpulseNOfix[,4] <- 0
	txtF3colNOpulseNOfix[,5] <- 0
	# head(txtF3colNOpulseNOfix)

	## get JPED row index
	index4JPEG = grep('JPEG', txtF3colNOpulseNOfix[,2])
	# str(index4JPEG)
	## find the intersection for the picture data 
	overlapPic = intersect(txtF3colNOpulseNOfix[index4JPEG,2], conFileUniq[,'Filename'])
	# str(overlapPic) ## 48  
	## If Pic is smaller >>
	conFileSub = conFileUniq[is.element(conFileUniq[,'Filename'], overlapPic), 1:3]
	dim(conFileSub)
	# head(conFileSub) 
	## get Ethnicity and Team by matching; 
	txtF3colNOpulseNOfix2match = txtF3colNOpulseNOfix
	for (p in index4JPEG){

		pic = txtF3colNOpulseNOfix[p,2]
		conFileSubTMP = conFileSub[is.element(conFileSub[,'Filename'], pic), 2:3]
		txtF3colNOpulseNOfix2match[p, 4:5] <- conFileSubTMP
	}

	## Add another column for 'response'
	txtF3colNOpulseNOfix2match[,6] <- 0
	txtF3tmp = txtF3colNOpulseNOfix2match
	dim(txtF3tmp)
	head(txtF3tmp)

	## get the index for 'response'
	index4reponse = grep('Response', txtF3tmp[,1])
	# str(index4reponse) 

	print(paste0('response: # ', length(index4reponse)))
	print(paste0('picture: # ', length(index4JPEG)))
	
	## data4analysis
	# write.table(txtF3tmp, file=paste0(i, '.data4analysis'), sep='\t', quote=F)

 	## keep only the first response if more than two repsones for one pic 
 	## index for 'repsone'
 	index4reponseAll = which(txtF3tmp[,1]=='Response') 
 	## length(index4reponseAll)
 	## remove the last index
 	index4reponseAllv1 = index4reponseAll[-length(index4reponseAll)]
 	index4reponseAllv2 = c(-2, index4reponseAllv1)
 	indexDiff = index4reponseAll-index4reponseAllv2
 	indexRedundant =which(indexDiff==1) 

 	if (length(indexRedundant)==0){ txtF4tmp=txtF3tmp} else (txtF4tmp = txtF3tmp[-index4reponseAll[indexRedundant],] )
 	
		## check 
	# str(txtF4tmp[which(txtF4tmp[,1]=='Response'), ])
	# str(txtF4tmp[grep('JPEG', txtF4tmp[,2]), ])

	## after removing unwanted 'response' index >> 
	index4reponse = which(txtF4tmp[,1]=='Response')
	## index for the pic; only keep the picture with its response
	index4jpegNEW = index4reponse-1
	## remove the index if there are more than 1 picture for one response
	index4JPEGall = grep('JPEG', txtF4tmp[,2])
	indexWrong4pic = setdiff(index4JPEGall, index4jpegNEW)
	if (length(indexWrong4pic)==0){ txtF5tmp=txtF4tmp} else (txtF5tmp = txtF4tmp[-indexWrong4pic,])
	
 
 	index4reponseV1 = grep('Response', txtF5tmp[,1])
 	index4jpegNEWv1 = grep('JPEG', txtF5tmp[,2])
 	length(index4reponseV1)
 	length(index4jpegNEWv1)
 	# if ( length(index4JPEG)==length(index4reponse) ){ 	
		## move 'response' value to a better position
		txtF5tmp[index4jpegNEWv1, 6] <- txtF5tmp[index4reponseV1,2]

	print('corrected: ')
		print(paste0('response: # ', length(index4reponseV1)))
		print(paste0('picture: # ', length(index4jpegNEWv1)))
	

		## assign column names 
		colnames(txtF5tmp) <- c('picRes', 'picTeam', 'RT', 'Ethnicity', 'Team', 'Response')


		experimentType = c('face_team', 'form_team', 'face_origin', 'form_form')
		index4experimentType = which(is.element(txtF5tmp[,'picTeam'], experimentType)==TRUE) 

		# str(index4experimentType)
		## assign group membership for all pictures
		dataTmp = txtF5tmp
		dataTmp[,7] <- 0
		colnames(dataTmp)[7] = 'group'
		for (i in 1:length(index4experimentType)){

			if (i!=length(index4experimentType)){

				j = i+1
				nextGroupIndex = index4experimentType[j]
				m = index4experimentType[i]
				n = nextGroupIndex-1
				dataTmp[m:n,7] = dataTmp[m, 'picTeam'] 
			} else if (i ==length(index4experimentType)){
				m = index4experimentType[i]
				n = nrow(dataTmp)
				dataTmp[m:n,7] = dataTmp[m, 'picTeam'] 
			}
		}

		## add another column for RT responose
	dataTmp0 = dataTmp
	dataTmp0[,8] = 0
	colnames(dataTmp0)[8] = 'RTresponse'
	head(dataTmp0)
	## assign response time to 'RTresponse'
	dataTmp0[grep('JPEG', dataTmp0[,'picTeam']),'RTresponse'] = dataTmp0[grep('Response', dataTmp0[,'picRes']), 'RT']

	## keep the matrix without redundant info
	dataTmp2v1 = dataTmp0[-which(dataTmp0[,'Ethnicity']==0), ]
	dim(dataTmp2v1)
	# head(dataTmp2)
 	dataTmp2 = cbind(dataTmp2v1, difRT=dataTmp2v1[,'RTresponse']-dataTmp2v1[,'RT'])
 

	## experimentType 1: 
	dataTyp1 = dataTmp2[which(dataTmp2[,'group']=='face_team'), ]
	# dataTyp1 = dataTmp2[which(dataTmp2[,'group']=='face_team'), 4:7]

	dim(dataTyp1)
	# head(dataTyp1)

	## If response == team 
	dataTyp1Response = dataTyp1[which(dataTyp1[,'Response']==dataTyp1[,'Team']),]
	dim(dataTyp1Response)
	# head(dataTyp1Response)
	team1_facetime = dataTyp1Response[which(dataTyp1Response[,'Team']==1),] 
	dim(team1_facetime)
	index4team1Eth1_facetime = which(team1_facetime[,'Ethnicity']==1)
	team1Eth1_facetime = length(index4team1Eth1_facetime)
	team1Eth1_facetime.RT = mean(team1_facetime[index4team1Eth1_facetime, 'difRT'])
	index4team1Eth2_facetime = which(team1_facetime[,'Ethnicity']==2)
 	team1Eth2_facetime = length(index4team1Eth2_facetime)
 	team1Eth2_facetime.RT = mean(team1_facetime[index4team1Eth2_facetime, 'difRT'])
 	team2_facetime = dataTyp1Response[which(dataTyp1Response[,'Team']==2),] 
	dim(team2_facetime)
	index4team2Eth1_facetime = which(team2_facetime[,'Ethnicity']==1)
	team2Eth1_facetime = length(index4team2Eth1_facetime)
	team2Eth1_facetime.RT = mean(team2_facetime[index4team2Eth1_facetime, 'difRT'])
	index4team2Eth2_facetime = which(team2_facetime[,'Ethnicity']==2)
 	team2Eth2_facetime = length(index4team2Eth2_facetime)
 	team2Eth2_facetime.RT = mean(team2_facetime[index4team2Eth2_facetime, 'difRT'])

 	dataTyp1_metric = c(team1Eth1_facetime, team1Eth2_facetime, team2Eth1_facetime, team2Eth2_facetime)
 	dataTyp1_metric.RT = c(team1Eth1_facetime.RT, team1Eth2_facetime.RT, team2Eth1_facetime.RT, team2Eth2_facetime.RT) 
 	dataTyp1_metric.RT

 	## experimentType 2: 
	dataTyp2 = dataTmp2[which(dataTmp2[,'group']=='form_team'), ]
	# dim(dataTyp2)
	# head(dataTyp2)

	index4reponseTeam = which(dataTyp2[,'Response']==dataTyp2[,'Team'])
	dataTyp2_metric = nrow(dataTyp2[index4reponseTeam,])
	# calculate RT 
	dataTyp2_metric.RT = mean(dataTyp2[index4reponseTeam,'difRT']) 

	## experimentType 3: 
	dataTyp3 = dataTmp2[which(dataTmp2[,'group']=='face_origin'),  ]
	# dim(dataTyp3)
	# head(dataTyp3)

	## If response == Ethnicity 
	dataTyp3Response = dataTyp3[which(dataTyp3[,'Response']==dataTyp3[,'Ethnicity']),]
	# dim(dataTyp3Response)
	# head(dataTyp3Response)  
	Eth1_faceOrig = dataTyp3Response[which(dataTyp3Response[,'Ethnicity']==1),] 
	dim(Eth1_faceOrig)
	index4eth1Team1_faceOrig = which(Eth1_faceOrig[,'Team']==1)
	eth1Team1_faceOrig = length(index4eth1Team1_faceOrig)
 	eth1Team1_faceOrig.RT = mean(Eth1_faceOrig[index4eth1Team1_faceOrig, 'difRT']) 
 	index4eth1Team2_faceOrig = which(Eth1_faceOrig[,'Team']==2)
	eth1Team2_faceOrig = length(index4eth1Team2_faceOrig)
 	eth1Team2_faceOrig.RT = mean(Eth1_faceOrig[index4eth1Team2_faceOrig, 'difRT'])

 	Eth2_faceOrig = dataTyp3Response[which(dataTyp3Response[,'Ethnicity']==2),]  
	dim(Eth2_faceOrig) 
	index4eth2Team1_faceOrig = which(Eth2_faceOrig[,'Team']==1)  
	eth2Team1_faceOrig = length(index4eth2Team1_faceOrig)
	eth2Team1_faceOrig.RT = mean(Eth2_faceOrig[index4eth2Team1_faceOrig, 'difRT']) 
	index4eth2Team2_faceOrig = which(Eth2_faceOrig[,'Team']==2)  
	eth2Team2_faceOrig = length(index4eth2Team2_faceOrig)
	eth2Team2_faceOrig.RT = mean(Eth2_faceOrig[index4eth2Team2_faceOrig, 'difRT']) 
 
 	dataTyp3_metric = c(eth1Team1_faceOrig, eth1Team2_faceOrig, eth2Team1_faceOrig, eth2Team2_faceOrig)
 	dataTyp3_metric.RT = c(eth1Team1_faceOrig.RT, eth1Team2_faceOrig.RT, eth2Team1_faceOrig.RT, eth2Team2_faceOrig.RT)
 	  

	## experimentType 4: 
	dataTyp4 = dataTmp2[which(dataTmp2[,'group']=='form_form'), ]
	# dim(dataTyp4)
	# head(dataTyp4)
	index4reponseTeamFormForm = which(dataTyp4[,'Response']==dataTyp4[,'Team'])
	dataTyp4_metric = nrow(dataTyp4[index4reponseTeamFormForm,])
	dataTyp4_metric.RT = mean(dataTyp4[index4reponseTeamFormForm,'difRT'])
 
	metricTmp = c(dataTyp1_metric, dataTyp2_metric, dataTyp3_metric, dataTyp4_metric)
	metricTmp.RT = c(dataTyp1_metric.RT, dataTyp2_metric.RT, dataTyp3_metric.RT, dataTyp4_metric.RT)
	metric <- rbind(metric, metricTmp)
	metric.RT <- rbind(metric.RT, metricTmp.RT)

	sampleIDs <- c(sampleIDs,  sampleID)
  

}
      
 
 

colnames(metric) = c("t1E1_ft", "t1E2_ft", "t2E1_ft", "t2E2_ft", "form_team", 
					 "E1t1_fo", "E1t2_fo", "E2t1_fo", "E2t2_fo", "form_form")
rownames(metric) = sampleIDs
print(metric)

colnames(metric.RT) = c("t1E1_ft.RT", "t1E2_ft.RT", "t2E1_ft.RT", "t2E2_ft.RT", "form_team.RT", 
					 "E1t1_fo.RT", "E1t2_fo.RT", "E2t1_fo.RT", "E2t2_fo.RT", "form_form.RT")
rownames(metric.RT) = sampleIDs
print(metric.RT)

metricFinaltmp = cbind(metric, metric.RT) 


metricFinal = metricFinaltmp

# write.csv2(metric, file='metric.csv')
# write.csv2(metric.RT, file='metric.RT.csv')
write.csv2(metricFinal, file='metricFinal.csv')


this.is.first.run <- FALSE

if(this.is.first.run){
  
  # The following two commands remove any previously installed H2O packages for R.
  if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
  if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
  
  # Next, we download packages that H2O depends on.
  if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
  if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
  if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
  if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
  if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
  if (! ("rjson" %in% rownames(installed.packages()))) { install.packages("rjson") }
  if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
  if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }
  
  # Now we download, install and initialize the H2O package for R.
  install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-simons/4/R")))
  
  
  install.packages("deepnet")
  
  install.packages("rvest")
  install.packages("XML")
  
  install.packages("data.table")
  
  install.packages("clusterGeneration")

}

library("deepnet")

library("rvest")
library("XML")
library("data.table")

library("hash")
library("ggplot2")

library("MASS")
library("clusterGeneration")
library("h2o")
 
library("dplyr")  


# change to your input directory and remote data directory
root.dir <- "/Users/mercicle/_DL/Deep-Learning-with-h2o-in-R/"
remote.data.root.dir <- "/Users/mercicle/datasets/uc-irvine-ml/"

helper.dir <- paste(root.dir,"helpers/", sep="")
source(paste(helper.dir, "helper-functions.r", sep="")) 

## local data
input.local.data.dir <- paste(root.dir,"_in-data-local/", sep="")
dir.create(file.path(input.local.data.dir), showWarnings = FALSE)

output.local.data.dir <- paste(root.dir,"_out-data-local/", sep="")
dir.create(file.path(output.local.data.dir), showWarnings = FALSE)

## datasets not saved in repository
input.data.dir <- paste(remote.data.root.dir,"in-data/", sep="")
dir.create(file.path(input.data.dir), showWarnings = FALSE)

output.data.dir <- paste(remote.data.root.dir,"out-data/", sep="")
dir.create(file.path(output.data.dir), showWarnings = FALSE)

downloaded.ml.data <- paste(input.data.dir, "ml-datasets/",sep="")
dir.create(file.path(downloaded.ml.data), showWarnings = FALSE)


 
