# Analyses for:
# Cold and warm temperatures limit the maximum body length of
# teleost fishes across a latitudinal gradient in Norwegian waters

# Lavin CP, Gordo-Vilaseca C, Costello MJ, Shi Z, Stephenson F, Gruss A
# Environmental Biology of Fishes
# April 2022

# Corresponding author: Charles P. Lavin, email: charles.p.lavin@nord.no

# Code for source functions "AssessRelativeImportancePredictors.R",
# "CustomizedBarPlot.R" and "ModelEvaluation.R" are available upon request.

# Loading packages

library( tidyverse )
library( dplyr )
library( ggplot2 )
library( ggpubr )
library( ggpmisc )
library( ggh4x )
library(magrittr)
library( grid )
library( gridExtra )
library( fitdistrplus )
library( Hmisc )
library( corrplot )
library( mgcv )
library( gratia )
library( raster )
library( rgdal )
library( rgeos )
library( sdmpredictors )
library( ggmap )
library( sdm )
library( spbabel )
library( maptools )
#library( SoDA )
library(grid)
library(png)

# Set working directory to file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

final_Norway_max_filtered = read.csv("Lavin_et_al_2022_b_EBFI.csv")

# Beaked redfish, Sebastes mentella
red_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Sebastes mentella") )
summary(red_resid_gam)

# GAM Evaluation
#model_list = list( red_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_red_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_red_resid", file = "Evaluation_GAM_max_length_red_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( red_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature",  "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "red_max_length_GAM_IndicesOfRelativeImportance.RData" )

beaked_redfish <- final_Norway_max_filtered %>% filter(Species == "Sebastes mentella")

# Predicted maximum lengths in line with temperature
X_mean <- mean(beaked_redfish$X)
Y_mean <- mean(beaked_redfish$Y)
Temp_seq.red = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Sebastes mentella"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Sebastes mentella"] ), length = 1200 ) 
Oxygen_mean.red <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Sebastes mentella"] )
Temp.preddata.red = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.red, oxy_temp_resid = Oxygen_mean.red )
Temp.pred.red = predict( red_resid_gam, type = 'response', newdata = Temp.preddata.red, se.fit = T )  
Temp.pred.fit.red <- Temp.pred.red$fit
Temp.pred.minus.se.fit.red <- Temp.pred.red$fit - Temp.pred.red$se.fit
Temp.pred.plus.se.fit.red <- Temp.pred.red$fit + Temp.pred.red$se.fit


# Capelin, Mallotus villosus
cap_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Mallotus villosus") )
summary(cap_resid_gam)

# GAM evaluation
#model_list = list( cap_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_cap_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_cap_resid", file = "Evaluation_GAM_max_length_cap_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( cap_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "cap_max_length_GAM_IndicesOfRelativeImportance.RData" )

capelin <- final_Norway_max_filtered %>% filter(Species == "Mallotus villosus")

# Predicted maximum lengths in line with temperature
X_mean <- mean(capelin$X)
Y_mean <- mean(capelin$Y)
Temp_seq.cap = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Mallotus villosus"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Sebastes mentella"] ), length = 1200 ) 
Oxygen_mean.cap <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Mallotus villosus"] )
Temp.preddata.cap = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.cap, oxy_temp_resid = Oxygen_mean.cap )
Temp.pred.cap = predict( cap_resid_gam, type = 'response', newdata = Temp.preddata.cap, se.fit = T )  
Temp.pred.fit.cap <- Temp.pred.cap$fit
Temp.pred.minus.se.fit.cap <- Temp.pred.cap$fit - Temp.pred.cap$se.fit
Temp.pred.plus.se.fit.cap <- Temp.pred.cap$fit + Temp.pred.cap$se.fit


# Greenland halibut, Reinhardtius hippoglossoides
hal_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Reinhardtius hippoglossoides") )
summary(hal_resid_gam)

# GAM Validation
#model_list = list( hal_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_hal_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_hal_resid", file = "Evaluation_GAM_max_length_hal_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( hal_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "hal_max_length_GAM_IndicesOfRelativeImportance.RData" )

halibut <- final_Norway_max_filtered %>% filter(Species == "Reinhardtius hippoglossoides")

# Predicted maximum lengths in line with temperature
X_mean <- mean(halibut$X)
Y_mean <- mean(halibut$Y)
Temp_seq.hal = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Reinhardtius hippoglossoides"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Reinhardtius hippoglossoides"] ), length = 1200 ) 
Oxygen_mean.hal <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Reinhardtius hippoglossoides"] )
Temp.preddata.hal = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.hal, oxy_temp_resid = Oxygen_mean.hal )
Temp.pred.hal = predict( hal_resid_gam, type = 'response', newdata = Temp.preddata.hal, se.fit = T )  
Temp.pred.fit.hal <- Temp.pred.hal$fit
Temp.pred.minus.se.fit.hal <- Temp.pred.hal$fit - Temp.pred.hal$se.fit
Temp.pred.plus.se.fit.hal <- Temp.pred.hal$fit + Temp.pred.hal$se.fit


# Golden redfish, Sebastes norvegicus
gol_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Sebastes norvegicus") )
summary(gol_resid_gam)

#GAM Validation
#model_list = list( gol_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_gol_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_gol_resid", file = "Evaluation_GAM_max_length_gol_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( gol_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "gol_max_length_GAM_IndicesOfRelativeImportance.RData" )

golden <- final_Norway_max_filtered %>% filter(Species == "Sebastes norvegicus")

# Predicted maximum lengths in line with temperature
X_mean <- mean(golden$X)
Y_mean <- mean(golden$Y)
Temp_seq.gol = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Sebastes norvegicus"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Sebastes norvegicus"] ), length = 1200 ) 
Oxygen_mean.gol <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Sebastes norvegicus"] )
Temp.preddata.gol = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.gol, oxy_temp_resid = Oxygen_mean.gol )
Temp.pred.gol = predict( gol_resid_gam, type = 'response', newdata = Temp.preddata.gol, se.fit = T )  
Temp.pred.fit.gol <- Temp.pred.gol$fit
Temp.pred.minus.se.fit.gol <- Temp.pred.gol$fit - Temp.pred.gol$se.fit
Temp.pred.plus.se.fit.gol <- Temp.pred.gol$fit + Temp.pred.gol$se.fit


# Polar cod, Boreogadus saida
pol_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Boreogadus saida") )
summary(pol_resid_gam)

# GAM Validation
#model_list = list( pol_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_pol_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_pol_resid", file = "Evaluation_GAM_max_length_pol_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( pol_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "pol_max_length_GAM_IndicesOfRelativeImportance.RData" )

polar <- final_Norway_max_filtered %>% filter(Species == "Boreogadus saida")

# Predicted maximum lengths in line with temperature
X_mean <- mean(polar$X)
Y_mean <- mean(polar$Y)
Temp_seq.pol = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Boreogadus saida"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Boreogadus saida"] ), length = 1200 ) 
Oxygen_mean.pol <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Boreogadus saida"] )
Temp.preddata.pol = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.pol, oxy_temp_resid = Oxygen_mean.pol )
Temp.pred.pol = predict( pol_resid_gam, type = 'response', newdata = Temp.preddata.pol, se.fit = T )  
Temp.pred.fit.pol <- Temp.pred.pol$fit
Temp.pred.minus.se.fit.pol <- Temp.pred.pol$fit - Temp.pred.pol$se.fit
Temp.pred.plus.se.fit.pol <- Temp.pred.pol$fit + Temp.pred.pol$se.fit


# Norway redfish, Sebastes viviparus
nre_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Sebastes viviparus") )
summary(nre_resid_gam)

# GAM Validation
#model_list = list( nre_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_nre_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_nre_resid", file = "Evaluation_GAM_max_length_nre_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( nre_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "nre_max_length_GAM_IndicesOfRelativeImportance.RData" )

norway_red <- final_Norway_max_filtered %>% filter(Species == "Sebastes viviparus")

# Predicted maximum lengths in line with temperature
X_mean <- mean(norway_red$X)
Y_mean <- mean(norway_red$Y)
Temp_seq.nre = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Sebastes viviparus"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Sebastes viviparus"] ), length = 1200 ) 
Oxygen_mean.nre <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Sebastes viviparus"] )
Temp.preddata.nre = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.nre, oxy_temp_resid = Oxygen_mean.nre )
Temp.pred.nre = predict( nre_resid_gam, type = 'response', newdata = Temp.preddata.nre, se.fit = T )  
Temp.pred.fit.nre <- Temp.pred.nre$fit
Temp.pred.minus.se.fit.nre <- Temp.pred.nre$fit - Temp.pred.nre$se.fit
Temp.pred.plus.se.fit.nre <- Temp.pred.nre$fit + Temp.pred.nre$se.fit


# Atlantic wolffish, Anarhichas lupus
wol_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Anarhichas lupus") )
summary(wol_resid_gam)

#GAM Validation
#model_list = list( wol_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_wol_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_wol_resid", file = "Evaluation_GAM_max_length_wol_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( wol_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "wol_max_length_GAM_IndicesOfRelativeImportance.RData" )

wolf <- final_Norway_max_filtered %>% filter(Species == "Anarhichas lupus")

# Predicted maximum lengths in line with temperature
X_mean <- mean(wolf$X)
Y_mean <- mean(wolf$Y)
Temp_seq.wol = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Anarhichas lupus"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Anarhichas lupus"] ), length = 1200 ) 
Oxygen_mean.wol <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Anarhichas lupus"] )
Temp.preddata.wol = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.wol, oxy_temp_resid = Oxygen_mean.wol )
Temp.pred.wol = predict( wol_resid_gam, type = 'response', newdata = Temp.preddata.wol, se.fit = T )  
Temp.pred.fit.wol <- Temp.pred.wol$fit
Temp.pred.minus.se.fit.wol <- Temp.pred.wol$fit - Temp.pred.wol$se.fit
Temp.pred.plus.se.fit.wol <- Temp.pred.wol$fit + Temp.pred.wol$se.fit


# Spotted wolffish, Anarhichas minor
spo_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" )+ s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Anarhichas minor") )
summary(spo_resid_gam)

# GAM Validation
#model_list = list( spo_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_spo_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_spo_resid", file = "Evaluation_GAM_max_length_spo_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( spo_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "spo_max_length_GAM_IndicesOfRelativeImportance.RData" )

spotted <- final_Norway_max_filtered %>% filter(Species == "Anarhichas minor")

# Predicted maximum lengths in line with temperature
X_mean <- mean(spotted$X)
Y_mean <- mean(spotted$Y)
Temp_seq.spo = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Anarhichas minor"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Anarhichas minor"] ), length = 1200 ) 
Oxygen_mean.spo <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Anarhichas minor"] )
Temp.preddata.spo = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.spo, oxy_temp_resid = Oxygen_mean.spo )
Temp.pred.spo = predict( spo_resid_gam, type = 'response', newdata = Temp.preddata.spo, se.fit = T )  
Temp.pred.fit.spo <- Temp.pred.spo$fit
Temp.pred.minus.se.fit.spo <- Temp.pred.spo$fit - Temp.pred.spo$se.fit
Temp.pred.plus.se.fit.spo <- Temp.pred.spo$fit + Temp.pred.spo$se.fit


# Daubed shanny, Leptoclinus maculatus 
sha_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Leptoclinus maculatus") )
summary(sha_resid_gam)

# GAM Validation
#model_list = list( sha_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_sha_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_sha_resid", file = "Evaluation_GAM_max_length_sha_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( sha_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "sha_max_length_GAM_IndicesOfRelativeImportance.RData" )

shanny <- final_Norway_max_filtered %>% filter(Species == "Leptoclinus maculatus")

# Predicted maximum lengths in line with temperature
X_mean <- mean(shanny$X)
Y_mean <- mean(shanny$Y)
Temp_seq.sha = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Leptoclinus maculatus"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Leptoclinus maculatus"] ), length = 1200 ) 
Oxygen_mean.sha <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Leptoclinus maculatus"] )
Temp.preddata.sha = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.sha, oxy_temp_resid = Oxygen_mean.sha )
Temp.pred.sha = predict( sha_resid_gam, type = 'response', newdata = Temp.preddata.sha, se.fit = T )  
Temp.pred.fit.sha <- Temp.pred.sha$fit
Temp.pred.minus.se.fit.sha <- Temp.pred.sha$fit - Temp.pred.sha$se.fit
Temp.pred.plus.se.fit.sha <- Temp.pred.sha$fit + Temp.pred.sha$se.fit


# Cusk, Brosme brosme
cus_resid_gam = mgcv::gam( Max_length ~ s( Temp, k = 4, bs = "ts" ) + s( oxy_temp_resid, k = 4, bs = "ts" ) + te( X, Y ) , Gamma( link = "log" ), method = 'REML', data = final_Norway_max_filtered %>% filter(Species == "Brosme brosme") )
summary(cus_resid_gam)

# GAM Validation
#model_list = list( cus_resid_gam )
#source( "ModelEvaluation.R" )
#Evaluation_GAM_max_length_cus_resid = evaluateModel( model_list, nIter = 10, confInt = TRUE ) 
#save( "Evaluation_GAM_max_length_cus_resid", file = "Evaluation_GAM_max_length_cus_resid.RData" )

# Estimate the indices of relative importance of the predictors
#model_list = list( cus_resid_gam )
#source( "AssessRelativeImportancePredictors.R" )
#RIeval_mean_length = relImportantVar( model_list[[1]], final_Norway_max_filtered, nRep = 10 )
#RIeval_mean_length_2 = RIeval_mean_length[,c( 1 : 2, 4 )]
#source( "CustomizedBarPlot.R" )
#RIeval_mean_length_2 
#rownames( RIeval_mean_length_2 ) = c( "Temperature", "Eastings", "Northings", "Oxygen residuals" )
#RIeval_mean_length_2 <- as.data.frame( RIeval_mean_length_2 )
#RIeval_mean_length_2 <- RIeval_mean_length_2[order( - RIeval_mean_length_2$meanInd ),]
#save( "RIeval_mean_length", "RIeval_mean_length_2", file = "cus_max_length_GAM_IndicesOfRelativeImportance.RData" )

cusk <- final_Norway_max_filtered %>% filter(Species == "Brosme brosme")

# Predicted maximum lengths in line with temperature
X_mean <- mean(cusk$X)
Y_mean <- mean(cusk$Y)
Temp_seq.cus = seq( min( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Brosme brosme"] ), max( final_Norway_max_filtered$Temp[final_Norway_max_filtered$Species == "Brosme brosme"] ), length = 1200 ) 
Oxygen_mean.cus <- mean( final_Norway_max_filtered$oxy_temp_resid[final_Norway_max_filtered$Species == "Brosme brosme"] )
Temp.preddata.cus = data.frame( X = X_mean, Y = Y_mean, Temp = Temp_seq.cus, oxy_temp_resid = Oxygen_mean.cus )
Temp.pred.cus = predict( cus_resid_gam, type = 'response', newdata = Temp.preddata.cus, se.fit = T )  
Temp.pred.fit.cus <- Temp.pred.cus$fit
Temp.pred.minus.se.fit.cus <- Temp.pred.cus$fit - Temp.pred.cus$se.fit
Temp.pred.plus.se.fit.cus <- Temp.pred.cus$fit + Temp.pred.cus$se.fit

# Figures 3, predicted maximum lengths

#filename <- file.choose()
#spotted_img <- readPNG(filename)

spotted = ggplot() + geom_line(aes(x = Temp_seq.spo, y = Temp.pred.fit.spo )) +
  geom_ribbon(aes(x = Temp_seq.spo, ymin = Temp.pred.minus.se.fit.spo, ymax = Temp.pred.plus.se.fit.spo), fill="darkviolet", alpha = 0.5) +
  geom_segment(aes(x = 0.6, y = 22, xend = 7.6, yend = 22), linetype="dashed", color = "darkviolet", size =1) +
  geom_point(aes(x=3.5, y=22), shape=19, colour="darkviolet", size = 4) +
  scale_y_continuous(breaks=seq(20,100,20), limits = c(20, 100)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=1, y=99, label="Spotted wolffish", size = 6)
  #annotation_raster(spotted_img, xmin = 6, xmax = 12, ymin = 70, ymax = 100)


#filename <- file.choose()
#cusk_img <- readPNG(filename)

cusk = ggplot() + geom_line(aes(x = Temp_seq.cus, y = Temp.pred.fit.cus )) +
  geom_ribbon(aes(x = Temp_seq.cus, ymin = Temp.pred.minus.se.fit.cus, ymax = Temp.pred.plus.se.fit.cus), fill="firebrick", alpha = 0.5) +
  geom_segment(aes(x = 0.5, y = 50, xend = 7.7, yend = 50), linetype="dashed", color = "firebrick", size =1) +
  geom_point(aes(x=3.6, y=50), shape=19, colour="firebrick", size = 4) +
  scale_y_continuous(breaks=seq(50,80,10), limits = c(48, 80)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=1, y=80, label="Cusk", size = 6) 
  #annotation_raster(cusk_img, xmin = 6, xmax = 12, ymin = 68, ymax = 80)

#filename <- file.choose()
#halibut_img <- readPNG(filename)

halibut = ggplot() + geom_line(aes(x = Temp_seq.hal, y = Temp.pred.fit.hal )) +
  geom_ribbon(aes(x = Temp_seq.hal, ymin = Temp.pred.minus.se.fit.hal, ymax = Temp.pred.plus.se.fit.hal), fill="darkseagreen", alpha = 0.5) +
  geom_segment(aes(x = 0.5, y = 43, xend = 4.2, yend = 43), linetype="dashed", color = "darkseagreen", size =1) +
  geom_point(aes(x=3, y=43), shape=19, colour="darkseagreen", size = 4) +
  scale_y_continuous(breaks=seq(40,60,5), limits = c(40, 60)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=1.2, y=60, label="Greenland halibut", size = 6) 
  #annotation_raster(halibut_img, xmin = 7, xmax = 12, ymin = 52.49, ymax = 60)

#filename <- file.choose()
#golden_img <- readPNG(filename)

golden = ggplot() + geom_line(aes(x = Temp_seq.gol, y = Temp.pred.fit.gol )) +
  geom_ribbon(aes(x = Temp_seq.gol, ymin = Temp.pred.minus.se.fit.gol, ymax = Temp.pred.plus.se.fit.gol), fill="goldenrod", alpha = 0.5) +
  geom_segment(aes(x = 0.6, y = 27, xend = 9.3, yend = 27), linetype="dashed", color = "goldenrod", size =1) +
  geom_point(aes(x=3.7, y=27), shape=19, colour="goldenrod", size = 4) +
  scale_y_continuous(breaks=seq(25,55,10), limits = c(24, 55)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=0.8, y=54.9, label="Golden redfish", size = 6)
  #annotation_raster(golden_img, xmin = 7, xmax = 12, ymin = 33.35, ymax = 45)

#filename <- file.choose()
#wolf_img <- readPNG(filename)

wolf = ggplot() + geom_line(aes(x = Temp_seq.wol, y = Temp.pred.fit.wol )) +
  geom_ribbon(aes(x = Temp_seq.wol, ymin = Temp.pred.minus.se.fit.wol, ymax = Temp.pred.plus.se.fit.wol), fill="deepskyblue4", alpha = 0.5) +
  geom_segment(aes(x = 0.8, y = 21, xend = 12, yend = 21), linetype="dashed", color = "deepskyblue4", size =1) +
  geom_point(aes(x=7.8, y=21), shape=19, colour="deepskyblue4", size = 4) +
  scale_y_continuous(breaks=seq(20,45,5), limits = c(20, 45)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=1, y=44.8, label="Atlantic wolffish", size = 6)
  #annotation_raster(wolf_img, xmin = 6, xmax = 12, ymin = 35.61, ymax = 45)

#filename <- file.choose()
#beaked_img <- readPNG(filename)

beaked = ggplot() + geom_line(aes(x = Temp_seq.red, y = Temp.pred.fit.red )) +
  geom_ribbon(aes(x = Temp_seq.red, ymin = Temp.pred.minus.se.fit.red, ymax = Temp.pred.plus.se.fit.red), fill="coral4", alpha = 0.5) +
  geom_segment(aes(x = 0.4, y = 20.5, xend = 6.6, yend = 20.5), linetype="dashed", color = "coral4", size =1) +
  geom_point(aes(x=3.6, y=20.5), shape=19, colour="coral4", size = 4) +
  scale_y_continuous(breaks=seq(20,40,5), limits = c(20, 40)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=1, y=39.8, label="Beaked redfish", size = 6)
  #annotation_raster(beaked_img, xmin = 7, xmax = 12, ymin = 32.49, ymax = 40)


#filename <- file.choose()
#norway_img <- readPNG(filename)

norway = ggplot() + geom_line(aes(x = Temp_seq.nre, y = Temp.pred.fit.nre )) +
  geom_ribbon(aes(x = Temp_seq.nre, ymin = Temp.pred.minus.se.fit.nre, ymax = Temp.pred.plus.se.fit.nre), fill="black", alpha = 0.5) +
  geom_segment(aes(x = 6.8, y = 23.25, xend = 9.8, yend = 23.25), linetype="dashed", color = "black", size =1) +
  geom_point(aes(x=7.8, y=23.25), shape=19, colour="black", size = 4) +
  scale_y_continuous(breaks=seq(24,30,2), limits = c(23, 30)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=1, y=29.9, label="Norway redfish", size = 6)
  #annotation_raster(norway_img, xmin = 7, xmax = 12, ymin = 27.37, ymax = 30)

#filename <- file.choose()
#capelin_img <- readPNG(filename)

cap = ggplot() + geom_line(aes(x = Temp_seq.cap, y = Temp.pred.fit.cap )) +
  geom_ribbon(aes(x = Temp_seq.cap, ymin = Temp.pred.minus.se.fit.cap, ymax = Temp.pred.plus.se.fit.cap), fill="chartreuse4", alpha = 0.5) +
  geom_segment(aes(x = 0.3, y = 15.5, xend = 7.1, yend = 15.5), linetype="dashed", color = "chartreuse4", size =1) +
  geom_point(aes(x=2.1, y=15.5), shape=19, colour="chartreuse4", size = 4) +
  scale_y_continuous(breaks=seq(15,18,1), limits = c(15, 18.5)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19))  +
  annotate(geom="text", x=1, y=18.25, label="Capelin", size = 6)
  #annotation_raster(capelin_img, xmin = 6, xmax = 12, ymin = 17.19, ymax = 18.5)

#filename <- file.choose()
#shanny_img <- readPNG(filename)

shanny = ggplot() + geom_line(aes(x = Temp_seq.sha, y = Temp.pred.fit.sha )) +
  geom_ribbon(aes(x = Temp_seq.sha, ymin = Temp.pred.minus.se.fit.sha, ymax = Temp.pred.plus.se.fit.sha), fill="darkorange", alpha = 0.5) +
  geom_segment(aes(x = 0.2, y = 12.3, xend = 7, yend = 12.3), linetype="dashed", color = "darkorange", size =1) +
  geom_point(aes(x=2.3, y=12.3), shape=19, colour="darkorange", size = 4) +
  scale_y_continuous(breaks=seq(12,22,2), limits = c(12, 22)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=1, y=21.9, label="Daubed shanny", size = 6)
  #annotation_raster(shanny_img, xmin = 7, xmax = 12, ymin = 18.25, ymax = 22)

#filename <- file.choose()
#polar_img <- readPNG(filename)

polar = ggplot() + geom_line(aes(x = Temp_seq.pol, y = Temp.pred.fit.pol )) +
  geom_ribbon(aes(x = Temp_seq.pol, ymin = Temp.pred.minus.se.fit.pol, ymax = Temp.pred.plus.se.fit.pol), fill="tomato1", alpha = 0.5) +
  geom_segment(aes(x = -1.3, y = 14.1, xend = 4, yend = 14.1), linetype="dashed", color = "tomato1", size =1) +
  geom_point(aes(x=0.2, y=14.1), shape=19, colour="tomato1", size = 4) +
  scale_y_continuous(breaks=seq(14,20,2), limits = c(14, 20)) +
  scale_x_continuous(breaks=seq(-2,12,2), limits = c(-2, 12)) +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19)) +
  annotate(geom="text", x=1, y=19.8, label="Polar cod", size = 6)
  #annotation_raster(polar_img, xmin = 7, xmax = 12, ymin = 17.75, ymax = 20)

Figure = ggarrange(spotted, cusk, norway, cap, halibut, golden, shanny, polar, wolf, beaked, nrow=5, ncol = 2, align = c("hv")) +
  theme(plot.margin = margin(0.5,0.5,2,0.5, "cm"))

Figure_3 = annotate_figure(Figure, bottom=text_grob("Temperature (°C)", size = 22, vjust=-3), left=text_grob("Predicted maximum length (TL, cm)", rot=90, size=22, vjust=0.8))

Figure_3

#ggsave("Figure_3.tiff", Figure_3,
#       width = 25,
#       height = 33,
#       units = c("cm"),
#       device = c("tiff"))
 
