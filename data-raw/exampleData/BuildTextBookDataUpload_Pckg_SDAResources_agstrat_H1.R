#======Prepares textbook data as H1 upload file===========

# Info:
# Table VS and SA contain the core information of \code{data(agstrat)} used in Lohr examples 3.2 and 3.6. 
# Table VS is stratified with VSstratumName set to \code{agstrat$region}, and VSnumberSampled and VSnumberTotal set according to \code{agstrat}
# VSunitName is set to a combination of original \code{agstrat$county}, \code{agstrat$state}, \code{agstrat$region} and \code{agstrat$agstrat} row numbers
# Table SA contains the variable measured agstrat$acres92 in \code{SAtotalWeightMeasured}, \code{SAsampleWeightMeasured} and \code{SAconversionFactorMeasLive} set to 1.
# Table DE, SD, FT and FO are for the most dummy tables inserted to meet RDBES model requirements to be aggregated 
# during estimation tests. Values of mandatory fields have dummy values taken from an onboard programme, with exception 
# of  *\code{selectionMethod} - that is set to CENSUS - they should be ignored during estimation.  
# BV, FM, CL, and CE are not provided.
# SL and VD are subset to the essential rows


	rm(list=ls())
	library(data.table)

	# load textbook data
		library(SDAResources)
		data(agstrat)
		dataset<-agstrat
		target_var<-"acres92"

	# name your project (will be used in filenames for CS, SL and VD)
		project_name_outputs <- "Pckg_SDAResources_agstrat_H1"


	# select a year for upload
		DEyear<-1968
		SDinstitution <- 4484
		DEsamplingScheme<-"National Routine"
		baseDir <- "./data-raw/exampleData/textBooks/"
		#baseDir <- "BuildTextBookExamples/"
		VD_base <- readRDS(paste0(baseDir,"VD_base.rds"))
		SL_base <- readRDS(paste0(baseDir,"SL_base.rds"))

		#nameof the directory where the outputs are saved currently
		base_dir_outputs <- baseDir
		dir_outputs<-paste0(base_dir_outputs,
	                    project_name_outputs,"/")
		dir.create(dir_outputs, recursive=T, showWarnings=FALSE)
		filename_output_CS <- paste0(project_name_outputs,"_H1.csv")
		filename_output_SL <- paste0(project_name_outputs,"_HSL.csv")
		filename_output_VD <- paste0(project_name_outputs,"_HVD.csv")

#========Outline of Hierarchy 1================
	# Design
	# Sampling details
	# Vessel Selection
	# Fishing Trip
	# Fishing Operation
	# Species Selection
	# Sample
	# Length
		# Biological variables



#===DE============



# 1                                     DEid [] - int
# 2                         DErecordType [M] - string
# 3             DEsamplingScheme [M] - SamplingScheme
# 4     DEsamplingSchemeType [M] - SamplingSchemeType
# 5                                 DEyear [M] - Year
# 6               DEstratumName [M] - StringLength100
# 7              DEhierarchyCorrect [M] - YesNoFields
# 8             DEhierarchy [M] - RDBESUpperHierarchy
# 9                    DEsampled [DV,M] - YesNoFields
# 10 DEreasonNotSampled [DV,O] - ReasonForNotSampling

DE_df_base<-expand.grid(DEyear=DEyear,
						DEstratumName="U",stringsAsFactors=F)

DE_df<-data.frame(
		  DEid = 1:nrow(DE_df_base),
		  DErecordType = "DE",
		  DEsamplingScheme = DEsamplingScheme,
		  DEsamplingSchemeType = "NatRouCF",
		  DEyear = as.integer(DEyear),
		  DEstratumName = "U",
		  DEhierarchyCorrect = "Y",
		  DEhierarchy = 1,
		  DEsampled = "Y",
		  DEreasonNotSampled = ""
			)

#===SD============


                          # x
# 1            SDid [M] - int
# 2            DEid [M] - int
# 3 SDrecordType [M] - string
# 4  SDcountry [M] - ISO_3166
# 5  SDinstitution [M] - EDMO

SD_df<-data.frame(
  SDid=DE_df$DEid,
  DEid=DE_df$DEid,
  SDrecordType="SD",
  SDcountry="ZW",
  SDinstitution=as.integer(SDinstitution),
  stringsAsFactors=FALSE
)

#===VS============


                                                             # x
# 1                                               VSid [M] - int
# 2                                               SDid [M] - int
# 3                                               VDid [M] - int
# 4                                             TEid [M/O] - int
# 5                                    VSrecordType [M] - string
# 6                                   VSsequenceNumber [M] - int
# 7                  VSencryptedVesselCode [M] - StringLength100
# 8                        VSstratification [DV,M] - YesNoFields
# 9                       VSstratumName [DV,M] - StringLength100
# 10                            VSclustering [DV,M] - Clustering
# 11                      VSclusterName [DV,M] - StringLength100
# 12                                     VSsampler [O] - Sampler
# 13                                  VSnumberTotal [DV,O] - int
# 14                                VSnumberSampled [DV,O] - int
# 15        VSselectionProb [DV,O] - Decimal0.0000000000000001-1
# 16        VSinclusionProb [DV,O] - Decimal0.0000000000000001-1
# 17                  VSselectionMethod [DV,M] - SelectionMethod
# 18                         VSunitName [DV,M] - StringLength100
# 19           VSselectionMethodCluster [DV,O] - SelectionMethod
# 20                          VSnumberTotalClusters [DV,O] - int
# 21                        VSnumberSampledClusters [DV,O] - int
# 22 VSselectionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 23 VSinclusionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 24                              VSsampled [DV,M] - YesNoFields
# 25            VSreasonNotSampled [DV,O] - ReasonForNotSampling

#check_All_fields("VS")

# adds VSid to dataset
	dataset$VSid <- 1:nrow(dataset)

# creates a dummyVD and adds dataset
	# restricts VD_base to what is needed
	VD_base <- VD_base[1:nrow(dataset),]

	dataset$VSencryptedVesselCode<-VD_base$VDencryptedVesselCode
	# should be 0
	test<-sum(duplicated(dataset$VSencryptedVesselCode))==0
	if(!test) stop( "duplicated VSencryptedVesselCode")


VS_df <- data.frame(
  VSid = dataset$VSid,
  SDid = as.integer(1),
  VDid = "",
  TEid = "",
  VSrecordType = 'VS',
  VSsequenceNumber = 1:nrow(dataset),# M
  VSencryptedVesselCode = dataset$VSencryptedVesselCode, #M
  VSstratification = "Y",
  VSstratumName = dataset$region, #M
  VSclustering = "N", #M
  VSclusterName = "U", #M
  VSsampler = "Observer", #M
  VSnumberTotal = "",
  VSnumberSampled = "",
  VSselectionProb = "",
  VSinclusionProb = "",
  VSselectionMethod = "SRSWOR", #M
  VSunitName = dataset$region,#M
  VSselectionMethodCluster = "",
  VSnumberTotalClusters = "",
  VSnumberSampledClusters = "",
  VSselectionProbCluster = "",
  VSinclusionProbCluster = "",
  VSsampled = "Y",#M
  VSreasonNotSampled = "", stringsAsFactors=FALSE
  )

sampSize<-table(VS_df$VSstratumName)
VS_df$VSnumberSampled<-sampSize[VS_df$VSstratumName]
strataSize<-c('NC' = 1054, 'NE' = 220, 'S' = 1382, 'W' = 422)
VS_df$VSnumberTotal<-strataSize[VS_df$VSstratumName]

#====FT===========


# 1                                               FTid [M] - int
# 2                                             OSid [M/O] - int
# 3                                             VSid [M/O] - int
# 4                                               VDid [M] - int
# 5                                             SDid [M/O] - int
# 6                                             FOid [M/O] - int
# 7                                             TEid [M/O] - int
# 8                                    FTrecordType [M] - string
# 9                  FTencryptedVesselCode [M] - StringLength100
# 10                      FTsequenceNumber [M] - StringLength100
# 11                       FTstratification [DV,M] - YesNoFields
# 12                      FTstratumName [DV,M] - StringLength100
# 13                            FTclustering [DV,M] - Clustering
# 14                      FTclusterName [DV,M] - StringLength100
# 15                                     FTsampler [O] - Sampler
# 16                        FTsamplingType [M] - SamplingContext
# 17                  FTnumberOfHaulsOrSets [M/O] - IntZeroToMax
# 18                  FTdepartureLocation [M/O] - Harbour_LOCODE
# 19                                FTdepartureDate [M/O] - Date
# 20                                FTdepartureTime [M/O] - Time
# 21                      FTarrivalLocation [M] - Harbour_LOCODE
# 22                                    FTarrivalDate [M] - Date
# 23                                  FTarrivalTime [M/O] - Time
# 24                                  FTnumberTotal [DV,O] - int
# 25                                FTnumberSampled [DV,O] - int
# 26        FTselectionProb [DV,O] - Decimal0.0000000000000001-1
# 27        FTinclusionProb [DV,O] - Decimal0.0000000000000001-1
# 28                  FTselectionMethod [DV,M] - SelectionMethod
# 29                         FTunitName [DV,M] - StringLength100
# 30           FTselectionMethodCluster [DV,O] - SelectionMethod
# 31                          FTnumberTotalClusters [DV,O] - int
# 32                        FTnumberSampledClusters [DV,O] - int
# 33 FTselectionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 34 FTinclusionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 35                              FTsampled [DV,M] - YesNoFields
# 36            FTreasonNotSampled [DV,O] - ReasonForNotSampling


FT_df <- data.frame(
  FTid = dataset$VSid,#[M] - int
  OSid = "",#  [M/O] - int
  VSid = dataset$VSid, #[M/O]
  VDid = "", #[M] - int
  SDid = "", #[M/O] - int
  FOid = "", #[M/O] - int
  TEid = "", #[M/O] - int
  FTrecordType='FT', #[M] - string
  FTencryptedVesselCode = dataset$VSencryptedVesselCode, #[M]
  FTsequenceNumber = as.integer(1:nrow(dataset)), #[M] - string
  FTstratification = "N", #[DV,M] - RS_Stratification
  FTstratumName = "U", #[DV,M] - string
  FTclustering = "N", #[DV,M] - RS_Clustering
  FTclusterName = "U", #[DV,M] - string
  FTsampler = "Observer", #[M] - RS_Sampler
  FTsamplingType = "AtSea" , #[M] - RS_SamplingType
  FTnumberOfHaulsOrSets = 1, #[O] - int
  FTdepartureLocation="ZWHWN", #[O] - Harbour_LOCODE
  FTdepartureDate=seq(from = as.Date("1968-01-01", format='%Y-%m-%d'), by=1, length.out=nrow(dataset)), #[M/O] - date
  FTdepartureTime="", #[O] - time
  FTarrivalLocation = "ZWHWN", #[M] - Harbour_LOCODE
  FTarrivalDate=seq(from = as.Date("1968-01-01", format='%Y-%m-%d'), by=1, length.out=nrow(dataset)), #[M] - date
  FTarrivalTime="", #[O] - time
  FTnumberTotal= 1, #[DV,O] - int
  FTnumberSampled=1, #[DV,O] - int
  FTselectionProb=1, #[DV,O] - DecimalPrec10
  FTinclusionProb=1, #[DV,O] - DecimalPrec10
  FTselectionMethod="CENSUS", #[DV,M] - RS_SelectionMethod
  FTunitName = dataset$VSid, #[DV,M] - string
  FTselectionMethodCluster="", #[DV,O] - RS_SelectionMethod
  FTnumberTotalClusters="", #[DV,O] - int
  FTnumberSampledClusters="", #[DV,O] - int
  FTselectionProbCluster="", #[DV,O] - DecimalPrec10
  FTinclusionProbCluster="", #[DV,O] - DecimalPrec10
  FTsampled="Y", #[DV,M] - YesNoFields
  FTreasonNotSampled= "", #[DV,O] - RS_ReasonForNotSampling
 stringsAsFactors=FALSE)


#====FO===========


# 1                                                                       FOid [M] - int
# 2                                                                     FTid [M/O] - int
# 3                                                                     SDid [M/O] - int
# 4                                                            FOrecordType [M] - string
# 5                                                FOstratification [DV,M] - YesNoFields
# 6                                                           FOsequenceNumber [M] - int
# 7                                               FOstratumName [DV,M] - StringLength100
# 8                                                     FOclustering [DV,M] - Clustering
# 9                                               FOclusterName [DV,M] - StringLength100
# 10                                                             FOsampler [O] - Sampler
# 11                                           FOaggregationLevel [M] - AggregationLevel
# 12                                                       FOvalidity [M] - ValidityFlag
# 13                                                  FOcatchReg [M] - CatchRegistration
# 14                                                              FOstartDate [O] - Date
# 15                                                              FOstartTime [O] - Time
# 16                                                                FOendDate [M] - Date
# 17                                                              FOendTime [M/O] - Time
# 18                                                              FOduration [M/O] - int
# 19                                               FOdurationSource [M] - DurationSource
# 20                                                            FOhandlingTime [O] - int
# 21                                        FOstartLat [O] - Decimal-90.000000-90.000000
# 22                                      FOstartLon [O] - Decimal-180.000000-180.000000
# 23                                         FOstopLat [O] - Decimal-90.000000-90.000000
# 24                                       FOstopLon [O] - Decimal-180.000000-180.000000
# 25                                     FOexclusiveEconomicZoneIndicator [O] - ISO_3166
# 26                                                              FOarea [M] - ICES_Area
# 27                                                           FOrectangle [O] - StatRec
# 28                                                   FOgsaSubarea [M] - Areas_GFCM_GSA
# 29                                           FOjurisdictionArea [O] - JurisdictionArea
# 30                                                            FOfishingDepth [O] - int
# 31                                                              FOwaterDepth [O] - int
# 32                             FOnationalFishingActivity [O] - NationalFishingActivity
# 33                                             FOmetier5 [O] - Metier5_FishingActivity
# 34                                             FOmetier6 [M] - Metier6_FishingActivity
# 35                                                               FOgear [M] - GearType
# 36                                                                FOmeshSize [O] - int
# 37                                             FOselectionDevice [O] - SelectionDevice
# 38                                                 FOselectionDeviceMeshSize [O] - int
# 39                                                 FOtargetSpecies [O] - TargetSpecies
# 40              FOincidentalByCatchMitigationDeviceFirst [M] - BycatchMitigationDevice
# 41  FOincidentalByCatchMitigationDeviceTargetFirst [M] - BycatchMitigationDeviceTarget
# 42             FOincidentalByCatchMitigationDeviceSecond [M] - BycatchMitigationDevice
# 43 FOincidentalByCatchMitigationDeviceTargetSecond [M] - BycatchMitigationDeviceTarget
# 44                                                          FOgearDimensions [O] - int
# 45                                             FOobservationCode [M] - ObservationCode
# 46                                                          FOnumberTotal [DV,O] - int
# 47                                                        FOnumberSampled [DV,O] - int
# 48                                FOselectionProb [DV,O] - Decimal0.0000000000000001-1
# 49                                FOinclusionProb [DV,O] - Decimal0.0000000000000001-1
# 50                                          FOselectionMethod [DV,M] - SelectionMethod
# 51                                                 FOunitName [DV,M] - StringLength100
# 52                                   FOselectionMethodCluster [DV,O] - SelectionMethod
# 53                                                  FOnumberTotalClusters [DV,O] - int
# 54                                                FOnumberSampledClusters [DV,O] - int
# 55                         FOselectionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 56                         FOinclusionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 57                                                      FOsampled [DV,M] - YesNoFields
# 58                                    FOreasonNotSampled [DV,O] - ReasonForNotSampling

FO_df <- data.frame(
	FOid = dataset$VSid,
	FTid = dataset$VSid,
	SDid = "",
	FOrecordType = "FO",#M
	FOstratification = "N",#M
	FOsequenceNumber = as.integer(1:nrow(dataset)),#M
	FOstratumName = "U",#M
	FOclustering = "N",#M
	FOclusterName = "U",#M
	FOsampler = "Observer",
	FOaggregationLevel = "H",#M
	FOvalidity = "V",#M
	FOcatchReg = "Lan",#M
	FOstartDate = as.Date(FT_df$FTdepartureDate, format='%Y-%m-%d'),
	FOstartTime = "",
	FOendDate = as.Date(FT_df$FTdepartureDate, format='%Y-%m-%d'),#M
	FOendTime ="",#M
	FOduration = 60, #M ATTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT!!!!
	FOdurationSource = "Crew",
	FOhandlingTime = "",
	FOstartLat="", # ATT!
	FOstartLon="", # ATT!
	FOstopLat="",
	FOstopLon="",
	FOexclusiveEconomicZoneIndicator = "", # might differ!!
	FOarea = "27.3.a.21", #M
	FOrectangle = "",
	FOgsaSubarea = "NotApplicable", #M
	FOjurisdictionArea = "",
	FOfishingDepth = "",
	FOwaterDepth = "",
	FOnationalFishingActivity = "",
	FOmetier5 = "",
	FOmetier6 = "OTT_CRU_70-89_2_35",#M
	FOgear = "OTT",#M
	FOmeshSize = "",
	FOselectionDevice = "",
	FOselectionDeviceMeshSize = "",
	FOtargetSpecies = "",
	FOincidentalByCatchMitigationDeviceFirst = "NotRecorded",#M
	FOincidentalByCatchMitigationDeviceTargetFirst = "NotApplicable",# [M]
	FOincidentalByCatchMitigationDeviceSecond = "NotRecorded",#M
	FOincidentalByCatchMitigationDeviceTargetSecond = "NotApplicable",#M
	FOgearDimensions = "",
	FOobservationCode = 'So', #M
	FOnumberTotal = 10,
	FOnumberSampled = 10,
	FOselectionProb = 1,
	FOinclusionProb = 1,
	FOselectionMethod = "CENSUS", #M
	FOunitName = VS_df$VSid, #M
	FOselectionMethodCluster = "",
	FOnumberTotalClusters = "",
	FOnumberSampledClusters = "",
	FOselectionProbCluster = "",
	FOinclusionProbCluster = "",
	FOsampled = "Y", #M
	FOreasonNotSampled = "",
stringsAsFactors=FALSE
)


#===SS============

# 1                                                SSid [] - int
# 2                                             LEid [M/O] - int
# 3                                             FOid [M/O] - int
# 4                                             FTid [M/O] - int
# 5                                             OSid [M/O] - int
# 6                                             TEid [M/O] - int
# 7                                               SLid [M] - int
# 8                                    SSrecordType [M] - string
# 9                                   SSsequenceNumber [M] - int
# 10                       SSstratification [DV,M] - YesNoFields
# 11                      SSstratumName [DV,M] - StringLength100
# 12                            SSclustering [DV,M] - Clustering
# 13                      SSclusterName [DV,M] - StringLength100
# 14  SSobservationActivityType [DV,M] - ObservationActivityType
# 15                         SScatchFraction [M] - CatchFraction
# 16                SSobservationType [DV,M] - ObservationMethod
# 17                                     SSsampler [O] - Sampler
# 18                     SSspeciesListName [M] - StringLength100
# 19                     SSuseForCalculateZero [M] - YesNoFields
# 20                                       SStimeTotal [O] - int
# 21                                     SStimeSampled [O] - int
# 22                                  SSnumberTotal [DV,O] - int
# 23                                SSnumberSampled [DV,O] - int
# 24        SSselectionProb [DV,O] - Decimal0.0000000000000001-1
# 25        SSinclusionProb [DV,O] - Decimal0.0000000000000001-1
# 26                  SSselectionMethod [DV,M] - SelectionMethod
# 27                         SSunitName [DV,M] - StringLength100
# 28           SSselectionMethodCluster [DV,O] - SelectionMethod
# 29                          SSnumberTotalClusters [DV,O] - int
# 30                        SSnumberSampledClusters [DV,O] - int
# 31 SSselectionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 32 SSinclusionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 33                              SSsampled [DV,M] - YesNoFields
# 34            SSreasonNotSampled [DV,O] - ReasonForNotSampling

SS_df<-data.frame(
	SSid = dataset$VSid,
	LEid = "",
	FOid = dataset$VSid,
	FTid = "",
	OSid = "",
	TEid = "",
	SLid = "",
	SSrecordType = "SS", #M
	SSsequenceNumber = as.integer(dataset$VSid), #M
	SSstratification = "N", #M
	SSstratumName = "U", #M
	SSclustering = "N", #M
	SSclusterName = "U", #M
	SSobservationActivityType = "Sort", #M
	SScatchFraction = "Lan", #M
	SSobservationType = "Volume", #M
	SSsampler = "Observer", #M
	SSspeciesListName = project_name_outputs, #M
	SSuseForCalculateZero = "N", #M
	SStimeTotal = "",
	SStimeSampled = "",
	SSnumberTotal = 1,
	SSnumberSampled = 1,
	SSselectionProb = 1,
	SSinclusionProb = 1,
	SSselectionMethod = "CENSUS",
	SSunitName = dataset$VSid,
	SSselectionMethodCluster = "",
	SSnumberTotalClusters = "",
	SSnumberSampledClusters = "",
	SSselectionProbCluster = "",
	SSinclusionProbCluster = "",
	SSsampled = "Y", #M,
	SSreasonNotSampled = ""
)

#====SA===========


# 1                                            SAid [] - int
# 2                                            SSid [] - int
# 3                                SArecordType [M] - string
# 4                               SAsequenceNumber [M] - int
# 5                         SAparentSequenceNumber [O] - int
# 6                    SAstratification [DV,M] - YesNoFields
# 7                   SAstratumName [DV,M] - StringLength100
# 8                            SAspeciesCode [M] - SpecWoRMS
# 9                         SAspeciesCodeFAO [O] - SpecASFIS
# 10             SAstateOfProcessing [M] - StateOfProcessing
# 11                SApresentation [M] - ProductPresentation
# 12                   SAspecimensState [M] - SpecimensState
# 13                     SAcatchCategory [M] - CatchCategory
# 14                 SAlandingCategory [O] - LandingCategory
# 15    SAcommSizeCatScale [O] - CommercialSizeCategoryScale
# 16              SAcommSizeCat [O] - CommercialSizeCategory
# 17                                       SAsex [M] - SEXCO
# 18         SAexclusiveEconomicZoneIndicator [O] - ISO_3166
# 19                                  SAarea [O] - ICES_Area
# 20                               SArectangle [O] - StatRec
# 21                       SAgsaSubarea [M] - Areas_GFCM_GSA
# 22               SAjurisdictionArea [O] - JurisdictionArea
# 23 SAnationalFishingActivity [O] - NationalFishingActivity
# 24                 SAmetier5 [O] - Metier5_FishingActivity
# 25                 SAmetier6 [O] - Metier6_FishingActivity
# 26                                   SAgear [O] - GearType
# 27                                    SAmeshSize [O] - int
# 28                 SAselectionDevice [O] - SelectionDevice
# 29                     SAselectionDeviceMeshSize [O] - int
# 30                           SAunitType [M] - SamplingUnit
# 31                             SAtotalWeightLive [O] - int
# 32                            SAsampleWeightLive [O] - int
# 33             SAnumberTotal [DV,O] - Decimal0.1-999999999
# 34           SAnumberSampled [DV,O] - Decimal0.1-999999999
# 35    SAselectionProb [DV,O] - Decimal0.0000000000000001-1
# 36    SAinclusionProb [DV,O] - Decimal0.0000000000000001-1
# 37              SAselectionMethod [DV,M] - SelectionMethod
# 38                     SAunitName [DV,M] - StringLength100
# 39                 SAlowerHierarchy [M/O] - LowerHierarchy
# 40                                 SAsampler [O] - Sampler
# 41                          SAsampled [DV,M] - YesNoFields
# 42      SAreasonNotSampledFM [DV,O] - ReasonForNotSampling
# 43      SAreasonNotSampledBV [DV,O] - ReasonForNotSampling
# 44                         SAtotalWeightMeasured [O] - int
# 45                        SAsampleWeightMeasured [O] - int
# 46        SAconversionFactorMeasLive [O] - Decimal0.900-10

SA_df<-data.frame(
		SAid = dataset$VSid,
		SSid = dataset$VSid,
		SArecordType = "SA", #M
		SAsequenceNumber = dataset$VSid, #M
		SAparentSequenceNumber = "",
		SAstratification = "N", #M
		SAstratumName = "U", #M
		SAspeciesCode = 107254,  #M
		SAspeciesCodeFAO = "NEP",
		SAstateOfProcessing = "FRE", #M
		SApresentation = "WHL", #M
		SAspecimensState = "DeadOrZeroProbSurvival", #M
		SAcatchCategory = "Lan", #M
		SAlandingCategory = "",
		SAcommSizeCatScale = "",
		SAcommSizeCat = "",
		SAsex = "U", #M
		SAexclusiveEconomicZoneIndicator = "",
		SAarea = "",
		SArectangle = "",
		SAgsaSubarea = "NotApplicable", #M
		SAjurisdictionArea = "",
		SAnationalFishingActivity = "",
		SAmetier5 = "",
		SAmetier6 = "",
		SAgear = "",
		SAmeshSize = "",
		SAselectionDevice = "",
		SAselectionDeviceMeshSize = "",
		SAunitType = "Box", #M
		SAtotalWeightLive = "",
		SAsampleWeightLive = "",
		SAnumberTotal = 1,
		SAnumberSampled = 1,
		SAselectionProb = 1,
		SAinclusionProb = 1,
		SAselectionMethod = "CENSUS", #M
		SAunitName = dataset$VSid, #M
		SAlowerHierarchy = "D",
		SAsampler = "Observer",
		SAsampled = "N", #M
		SAreasonNotSampledFM = "",
		SAreasonNotSampledBV = "",
		SAtotalWeightMeasured = dataset[[target_var]],
		SAsampleWeightMeasured = dataset[[target_var]],
		SAconversionFactorMeasLive = 1,
		stringsAsFactors=FALSE
)



#====Builds final format===========


RDBESlist = list(DE = DE_df,SD = SD_df, VS = VS_df, FT = FT_df, FO = FO_df, SS = SS_df, SA = SA_df)

#id table
a<-merge(DE_df["DEid"],SD_df[c("DEid","SDid")])
a<-merge(a, VS_df[c("SDid","VSid")], all.x=T)
a<-merge(a, FT_df[c("VSid","FTid")], all.x=T)
a<-merge(a, FO_df[c("FTid","FOid")], all.x=T)
a<-merge(a, SS_df[c("FOid","SSid")], all.x=T)
a<-merge(a, SA_df[c("SSid","SAid")], all.x=T)

# reorder columns
a<-a[c("DEid","SDid","VSid","FTid","FOid","SSid","SAid")]
# reorder rows
a<-data.table(a)
a<-a[order(DEid,SDid,VSid,FTid,FOid,SSid,SAid),]

a$DEindex=apply(a[,1:which(colnames(a)=="DEid")],1,paste, collapse="_")
a$SDindex=apply(a[,1:which(colnames(a)=="SDid")],1,paste, collapse="_")
a$VSindex=apply(a[,1:which(colnames(a)=="VSid")],1,paste, collapse="_")
a$FTindex=apply(a[,1:which(colnames(a)=="FTid")],1,paste, collapse="_")
a$FOindex=apply(a[,1:which(colnames(a)=="FOid")],1,paste, collapse="_")
a$SSindex=apply(a[,1:which(colnames(a)=="SSid")],1,paste, collapse="_")
a$SAindex=apply(a[,1:which(colnames(a)=="SAid")],1,paste, collapse="_")

key<-c(a$DEindex[match(DE_df$DEid,a$DEid)],
a$SDindex[match(SD_df$SDid,a$SDid)],
a$VSindex[match(VS_df$VSid,a$VSid)],
a$FTindex[match(FT_df$FTid,a$FTid)],
a$FOindex[match(FO_df$FOid,a$FOid)],
a$SSindex[match(SS_df$SSid,a$SSid)],
a$SAindex[match(SA_df$SAid,a$SAid)]
)

# file production
Oldscipen<-.Options$scipen
options(scipen=500)

#remove all id
for (i in names(RDBESlist))
{
RDBESlist[[i]][which(grepl(colnames(RDBESlist[[i]]),pat="[A-Z]id"))]<-NULL
}

#===Save============





lapply(RDBESlist, function(x, filename1 = paste0(dir_outputs,filename_output_CS)){
if("DErecordType" %in% colnames(x)){
	write.table(x, file = filename1, append = FALSE, quote = FALSE, sep = ",",
				eol = "\n", na = "NA", dec = ".", row.names = FALSE,
				col.names = FALSE, qmethod = c("escape", "double"))
	} else {
write.table(x, file = filename1, append = TRUE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"))
			}
})

b<-read.table(file=paste0(dir_outputs,filename_output_CS), header=F, sep=";")
b<-cbind(key,b)
b<-b[order(as.character(b$key), decreasing=FALSE),]
b<-b[!is.na(key),]
b$key<-NULL
b$V1<-as.character(b$V1)

# saves CS output
write.table(b$V1, file=paste0(dir_outputs,filename_output_CS), col.names=FALSE, row.names = FALSE, quote=FALSE,sep=",")

# -----Builds and saves dummySL-----------------


	SL_base$SLspeciesListName<-project_name_outputs
	SL_base$SLyear<-DEyear
	SL_base$SLinstitute<-SDinstitution
# saves SL output
	write.table(SL_base,  file=paste0(dir_outputs,filename_output_SL), col.names=FALSE, row.names = FALSE, quote=FALSE,sep=",")


# -----Builds and saves dummyVD-----------------


# saves VD output

	write.table(VD_base, file=paste0(dir_outputs,filename_output_VD), col.names=FALSE, row.names = FALSE, quote=FALSE,sep=",")



# -----Clean SL after dowload-----------------

# cleans excess of SL rows frequently present in download
	# note: 
		# since it is an example - nicer to do here, fixing directly data input to scripts than patching it over multiple scripts later
		# since example data are very simple, a simple select is done on specieaListName - this may not be sufficient in some minor cases
		
filename_download <- "2022_10_14_054806.zip"
unzip(paste0(dir_outputs, filename_download), exdir = paste0(dir_outputs,"tmp"))
# reads file to be fixed, fixes it and saves it back overwriting
tmp<-fread(paste0(dir_outputs,"tmp/SpeciesList.csv"))
tmp<-tmp[SLspeciesListName==project_name_outputs,]
write.csv(tmp, file=paste0(dir_outputs,"tmp/SpeciesList.csv"), quote=F, row.names=F)
# re-zips the file, puts it in final dir and deletes tmp dir
library(zip)
zip::zip(zipfile = paste0(project_name_outputs,".zip"), files =paste0(dir_outputs,"tmp/",dir(paste0(dir_outputs,"tmp"))), mode="cherry-pick")
unlink(paste0(dir_outputs,"tmp"), force=T, recursive = T)


