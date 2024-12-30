####Load packages
library(lme4)
library(psych)
library(lavaan)
library(haven)


#conduct longitudinal combat
# load packages for longitudinal combat
library(longCombat)
library(invgamma)
library(lme4)

featurenames <- c("smri_vol_cdk_banksstslh",
                  "smri_vol_cdk_cdacatelh",
                  "smri_vol_cdk_cdmdfrlh",
                  "smri_vol_cdk_cuneuslh",
                  "smri_vol_cdk_ehinallh",
                  "smri_vol_cdk_fusiformlh",
                  "smri_vol_cdk_ifpllh",
                  "smri_vol_cdk_iftmlh",
                  "smri_vol_cdk_ihcatelh",
                  "smri_vol_cdk_locclh",
                  "smri_vol_cdk_lobfrlh",
                  "smri_vol_cdk_linguallh",
                  "smri_vol_cdk_mobfrlh",
                  "smri_vol_cdk_mdtmlh",
                  "smri_vol_cdk_parahpallh",
                  "smri_vol_cdk_paracnlh",
                  "smri_vol_cdk_parsopclh",
                  "smri_vol_cdk_parsobislh",
                  "smri_vol_cdk_parstgrislh",
                  "smri_vol_cdk_pericclh",
                  "smri_vol_cdk_postcnlh",
                  "smri_vol_cdk_ptcatelh",
                  "smri_vol_cdk_precnlh",
                  "smri_vol_cdk_pclh",
                  "smri_vol_cdk_rracatelh",
                  "smri_vol_cdk_rrmdfrlh",
                  "smri_vol_cdk_sufrlh",
                  "smri_vol_cdk_supllh",
                  "smri_vol_cdk_sutmlh",
                  "smri_vol_cdk_smlh",
                  "smri_vol_cdk_frpolelh",
                  "smri_vol_cdk_tmpolelh",
                  "smri_vol_cdk_trvtmlh",
                  "smri_vol_cdk_insulalh",
                  "smri_vol_cdk_banksstsrh",
                  "smri_vol_cdk_cdacaterh",
                  "smri_vol_cdk_cdmdfrrh",
                  "smri_vol_cdk_cuneusrh",
                  "smri_vol_cdk_ehinalrh",
                  "smri_vol_cdk_fusiformrh",
                  "smri_vol_cdk_ifplrh",
                  "smri_vol_cdk_iftmrh",
                  "smri_vol_cdk_ihcaterh",
                  "smri_vol_cdk_loccrh",
                  "smri_vol_cdk_lobfrrh",
                  "smri_vol_cdk_lingualrh",
                  "smri_vol_cdk_mobfrrh",
                  "smri_vol_cdk_mdtmrh",
                  "smri_vol_cdk_parahpalrh",
                  "smri_vol_cdk_paracnrh",
                  "smri_vol_cdk_parsopcrh",
                  "smri_vol_cdk_parsobisrh",
                  "smri_vol_cdk_parstgrisrh",
                  "smri_vol_cdk_periccrh",
                  "smri_vol_cdk_postcnrh",
                  "smri_vol_cdk_ptcaterh",
                  "smri_vol_cdk_precnrh",
                  "smri_vol_cdk_pcrh",
                  "smri_vol_cdk_rracaterh",
                  "smri_vol_cdk_rrmdfrrh",
                  "smri_vol_cdk_sufrrh",
                  "smri_vol_cdk_suplrh",
                  "smri_vol_cdk_sutmrh",
                  "smri_vol_cdk_smrh",
                  "smri_vol_cdk_frpolerh",
                  "smri_vol_cdk_tmpolerh",
                  "smri_vol_cdk_trvtmrh",
                  "smri_vol_cdk_insularh",
                  "smri_vol_cdk_totallh",
                  "smri_vol_cdk_totalrh",
                  "smri_vol_cdk_total",
                  "smri_thick_cdk_banksstslh",
                  "smri_thick_cdk_cdacatelh",
                  "smri_thick_cdk_cdmdfrlh",
                  "smri_thick_cdk_cuneuslh",
                  "smri_thick_cdk_ehinallh",
                  "smri_thick_cdk_fusiformlh",
                  "smri_thick_cdk_ifpllh",
                  "smri_thick_cdk_iftmlh",
                  "smri_thick_cdk_ihcatelh",
                  "smri_thick_cdk_locclh",
                  "smri_thick_cdk_lobfrlh",
                  "smri_thick_cdk_linguallh",
                  "smri_thick_cdk_mobfrlh",
                  "smri_thick_cdk_mdtmlh",
                  "smri_thick_cdk_parahpallh",
                  "smri_thick_cdk_paracnlh",
                  "smri_thick_cdk_parsopclh",
                  "smri_thick_cdk_parsobislh",
                  "smri_thick_cdk_parstgrislh",
                  "smri_thick_cdk_pericclh",
                  "smri_thick_cdk_postcnlh",
                  "smri_thick_cdk_ptcatelh",
                  "smri_thick_cdk_precnlh",
                  "smri_thick_cdk_pclh",
                  "smri_thick_cdk_rracatelh",
                  "smri_thick_cdk_rrmdfrlh",
                  "smri_thick_cdk_sufrlh",
                  "smri_thick_cdk_supllh",
                  "smri_thick_cdk_sutmlh",
                  "smri_thick_cdk_smlh",
                  "smri_thick_cdk_frpolelh",
                  "smri_thick_cdk_tmpolelh",
                  "smri_thick_cdk_trvtmlh",
                  "smri_thick_cdk_insulalh",
                  "smri_thick_cdk_banksstsrh",
                  "smri_thick_cdk_cdacaterh",
                  "smri_thick_cdk_cdmdfrrh",
                  "smri_thick_cdk_cuneusrh",
                  "smri_thick_cdk_ehinalrh",
                  "smri_thick_cdk_fusiformrh",
                  "smri_thick_cdk_ifplrh",
                  "smri_thick_cdk_iftmrh",
                  "smri_thick_cdk_ihcaterh",
                  "smri_thick_cdk_loccrh",
                  "smri_thick_cdk_lobfrrh",
                  "smri_thick_cdk_lingualrh",
                  "smri_thick_cdk_mobfrrh",
                  "smri_thick_cdk_mdtmrh",
                  "smri_thick_cdk_parahpalrh",
                  "smri_thick_cdk_paracnrh",
                  "smri_thick_cdk_parsopcrh",
                  "smri_thick_cdk_parsobisrh",
                  "smri_thick_cdk_parstgrisrh",
                  "smri_thick_cdk_periccrh",
                  "smri_thick_cdk_postcnrh",
                  "smri_thick_cdk_ptcaterh",
                  "smri_thick_cdk_precnrh",
                  "smri_thick_cdk_pcrh",
                  "smri_thick_cdk_rracaterh",
                  "smri_thick_cdk_rrmdfrrh",
                  "smri_thick_cdk_sufrrh",
                  "smri_thick_cdk_suplrh",
                  "smri_thick_cdk_sutmrh",
                  "smri_thick_cdk_smrh",
                  "smri_thick_cdk_frpolerh",
                  "smri_thick_cdk_tmpolerh",
                  "smri_thick_cdk_trvtmrh",
                  "smri_thick_cdk_insularh",
                  "smri_thick_cdk_meanlh",
                  "smri_thick_cdk_meanrh",
                  "smri_thick_cdk_mean",
                  "smri_area_cdk_banksstslh",
                  "smri_area_cdk_cdacatelh",
                  "smri_area_cdk_cdmdfrlh",
                  "smri_area_cdk_cuneuslh",
                  "smri_area_cdk_ehinallh",
                  "smri_area_cdk_fusiformlh",
                  "smri_area_cdk_ifpllh",
                  "smri_area_cdk_iftmlh",
                  "smri_area_cdk_ihcatelh",
                  "smri_area_cdk_locclh",
                  "smri_area_cdk_lobfrlh",
                  "smri_area_cdk_linguallh",
                  "smri_area_cdk_mobfrlh",
                  "smri_area_cdk_mdtmlh",
                  "smri_area_cdk_parahpallh",
                  "smri_area_cdk_paracnlh",
                  "smri_area_cdk_parsopclh",
                  "smri_area_cdk_parsobislh",
                  "smri_area_cdk_parstgrislh",
                  "smri_area_cdk_pericclh",
                  "smri_area_cdk_postcnlh",
                  "smri_area_cdk_ptcatelh",
                  "smri_area_cdk_precnlh",
                  "smri_area_cdk_pclh",
                  "smri_area_cdk_rracatelh",
                  "smri_area_cdk_rrmdfrlh",
                  "smri_area_cdk_sufrlh",
                  "smri_area_cdk_supllh",
                  "smri_area_cdk_sutmlh",
                  "smri_area_cdk_smlh",
                  "smri_area_cdk_frpolelh",
                  "smri_area_cdk_tmpolelh",
                  "smri_area_cdk_trvtmlh",
                  "smri_area_cdk_insulalh",
                  "smri_area_cdk_banksstsrh",
                  "smri_area_cdk_cdacaterh",
                  "smri_area_cdk_cdmdfrrh",
                  "smri_area_cdk_cuneusrh",
                  "smri_area_cdk_ehinalrh",
                  "smri_area_cdk_fusiformrh",
                  "smri_area_cdk_ifplrh",
                  "smri_area_cdk_iftmrh",
                  "smri_area_cdk_ihcaterh",
                  "smri_area_cdk_loccrh",
                  "smri_area_cdk_lobfrrh",
                  "smri_area_cdk_lingualrh",
                  "smri_area_cdk_mobfrrh",
                  "smri_area_cdk_mdtmrh",
                  "smri_area_cdk_parahpalrh",
                  "smri_area_cdk_paracnrh",
                  "smri_area_cdk_parsopcrh",
                  "smri_area_cdk_parsobisrh",
                  "smri_area_cdk_parstgrisrh",
                  "smri_area_cdk_periccrh",
                  "smri_area_cdk_postcnrh",
                  "smri_area_cdk_ptcaterh",
                  "smri_area_cdk_precnrh",
                  "smri_area_cdk_pcrh",
                  "smri_area_cdk_rracaterh",
                  "smri_area_cdk_rrmdfrrh",
                  "smri_area_cdk_sufrrh",
                  "smri_area_cdk_suplrh",
                  "smri_area_cdk_sutmrh",
                  "smri_area_cdk_smrh",
                  "smri_area_cdk_frpolerh",
                  "smri_area_cdk_tmpolerh",
                  "smri_area_cdk_trvtmrh",
                  "smri_area_cdk_insularh",
                  "smri_area_cdk_totallh",
                  "smri_area_cdk_totalrh",
                  "smri_area_cdk_total",
                  "smri_vol_scs_cbwmatterlh",
                  "smri_vol_scs_ltventriclelh",
                  "smri_vol_scs_inflatventlh",
                  "smri_vol_scs_crbwmatterlh",
                  "smri_vol_scs_crbcortexlh",
                  "smri_vol_scs_tplh",
                  "smri_vol_scs_caudatelh",
                  "smri_vol_scs_putamenlh",
                  "smri_vol_scs_pallidumlh",
                  "smri_vol_scs_3rdventricle",
                  "smri_vol_scs_4thventricle",
                  "smri_vol_scs_bstem",
                  "smri_vol_scs_hpuslh",
                  "smri_vol_scs_amygdalalh",
                  "smri_vol_scs_csf",
                  "smri_vol_scs_aal",
                  "smri_vol_scs_vedclh",
                  "smri_vol_scs_cbwmatterrh",
                  "smri_vol_scs_ltventriclerh",
                  "smri_vol_scs_inflatventrh",
                  "smri_vol_scs_crbwmatterrh",
                  "smri_vol_scs_crbcortexrh",
                  "smri_vol_scs_tprh",
                  "smri_vol_scs_caudaterh",
                  "smri_vol_scs_putamenrh",
                  "smri_vol_scs_pallidumrh",
                  "smri_vol_scs_hpusrh",
                  "smri_vol_scs_amygdalarh",
                  "smri_vol_scs_aar",
                  "smri_vol_scs_vedcrh",
                  "smri_vol_scs_wmhint",
                  "smri_vol_scs_ccps",
                  "smri_vol_scs_ccmidps",
                  "smri_vol_scs_ccct",
                  "smri_vol_scs_ccmidat",
                  "smri_vol_scs_ccat",
                  "smri_vol_scs_wholeb",
                  "smri_vol_scs_latventricles",
                  "smri_vol_scs_allventricles",
                  "smri_vol_scs_intracranialv",
                  "smri_vol_scs_suprateialv",
                  "smri_vol_scs_subcorticalgv")

#################################
# longCombat() -- apply longitudinal ComBat
#################################
data_longcombat <- longCombat(idvar='src_subject_id',
                              timevar='eventname',
                              batchvar='mri_info_manufacturer',
                              features=featurenames,
                              formula='interview_age + eventname + demo_sex_v2',
                              ranef='(1|src_subject_id) + (1|site_id_l) + (1|rel_family_id)',
                              data=abcd_struct_all_noNA)


#################################
# get the harmonized data
data_longcombat_harmonized <- data_longcombat$data_combat
# save combat feature names
featurenames.combat <- names(data_longcombat_harmonized)[4:258]
# merge with original dataframe
abcd_struct_all_noNA2 <- merge(abcd_struct_all_noNA, data_longcombat_harmonized, by=c('src_subject_id', 'eventname'))


#conduct latent growth curve models
#import data: ABCD Data Release 5.1

data_forlongdata50 <- data_forlongdata50_v3_combat

####Set random seed so results are reproducible
set.seed(123)

#Functions that help
withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}

#Scale
FourOfHearts.Scale <- function(D, N){
  Scaling <- function(x, y) {
    Var <- x/sd(x, na.rm=TRUE)
    return(Var)}
  M <- N-1
  Initial <- as.data.frame(sapply(D[,N:ncol(D)], Scaling));
  Scaled <- as.data.frame(cbind(D[1:M], Initial, make.row.names=TRUE))
  return(Scaled)
}

data_forlongdata50$demo_comb_income_v2[data_forlongdata50$demo_comb_income_v2 %in% c(777, 999, 888)] <- NA
data_forlongdata50$group3_newscore <- factor(data_forlongdata50$group3_newscore)
data_forlongdata50[data_forlongdata50=='NA']<-NA


data_forlongdata50[,c(5:1249)] <- lapply(data_forlongdata50[,c(5:1249)],as.numeric)
data_forlongdata50 <- data_forlongdata50 %>%
  mutate_if(is.numeric, scale)
data_forlongdata50 <- as.data.frame(data_forlongdata50)

View(data_forlongdata50)


#for imaging
data_forlongdata50$qc_ok_samp <-0

data_forlongdata50$qc_ok_samp[
  ((iqc_t1_ok_ser.baseline_year_1_arm_1>=1 & (fsqc_qc.baseline_year_1_arm_1>=1| is.na(fsqc_qc.baseline_year_1_arm_1))) | (baseline_imaging_data == 0)) &
    ((iqc_t1_ok_ser.2_year_follow_up_y_arm_1>=1 & (fsqc_qc.2_year_follow_up_y_arm_1>=1 | is.na(fsqc_qc.2_year_follow_up_y_arm_1))) | (y2_imaging_data == 0)) &
    ((iqc_t1_ok_ser.4_year_follow_up_y_arm_1>=1 & (fsqc_qc.4_year_follow_up_y_arm_1>=1 | is.na(fsqc_qc.4_year_follow_up_y_arm_1))) | (y4_imaging_data == 0))]<-1

data_forlongdata50$qc_ok_samp[
  ((iqc_t1_ok_ser.baseline_year_1_arm_1==0 | fsqc_qc.baseline_year_1_arm_1==0)  & (baseline_imaging_data == 1)) |
    ((iqc_t1_ok_ser.2_year_follow_up_y_arm_1==0 | fsqc_qc.2_year_follow_up_y_arm_1==0)  & (y2_imaging_data == 1)) |
    ((iqc_t1_ok_ser.4_year_follow_up_y_arm_1==0 | fsqc_qc.4_year_follow_up_y_arm_1==0)  & (y4_imaging_data == 1))]<-0


groups <- svydesign(id=~rel_family_id.baseline_year_1_arm_1 + site_id_l.baseline_year_1_arm_1, data = data_forlongdata50)

data=subset(data_forlongdata50,qc_ok_samp==1)


#group difference slope models
##STD JUST pattern
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_pattern =~ 0*nihtbx_pattern_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_pattern_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_pattern_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

#regression
sgf_pattern~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_pattern_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_pattern_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_pattern_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

sgf_pattern ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate


##STD JUST picvocab
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_picvocab =~ 0*nihtbx_picvocab_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_picvocab_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_picvocab_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

#regression
sgf_picvocab~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_picvocab_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_picvocab_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_picvocab_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

sgf_picvocab ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST flanker
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_flanker =~ 0*nihtbx_flanker_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_flanker_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_flanker_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

#regression
sgf_flanker~ sex_STD + vist_type_Y4_remote_STD+demo_prnt_ed_v2_STD
nihtbx_flanker_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_flanker_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_flanker_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

sgf_flanker ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST picture
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_picture =~ 0*nihtbx_picture_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_picture_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_picture_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

#regression
sgf_picture~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_picture_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_picture_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_picture_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

sgf_picture ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST reading
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_reading =~ 0*nihtbx_reading_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_reading_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_reading_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

#regression
sgf_reading~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_reading_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_reading_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_reading_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

sgf_reading ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate





##STD JUST thickness
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_thick =~ 0*smri_thick_cdk_mean.baseline_year_1_arm_1_STD + 1*smri_thick_cdk_mean.2_year_follow_up_y_arm_1_STD + 2*smri_thick_cdk_mean.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
sgf_thick ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_thick_cdk_mean.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_thick_cdk_mean.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_thick_cdk_mean.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD

sgf_thick ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate


##STD JUST cortical volume
#MULTIGROUP MODEL
#LGC Model

gf_lgm_scons2 <-
  '
# LVs
sgf_vol =~ 0*smri_vol_cdk_total.baseline_year_1_arm_1_STD + 1*smri_vol_cdk_total.2_year_follow_up_y_arm_1_STD + 2*smri_vol_cdk_total.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

sgf_vol ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_vol_cdk_total.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_vol_cdk_total.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_vol_cdk_total.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD


sgf_vol ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate


##STD JUST subcortical volume
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_subcort =~ 0*smri_vol_scs_subcorticalgv.baseline_year_1_arm_1_STD + 1*smri_vol_scs_subcorticalgv.2_year_follow_up_y_arm_1_STD + 2*smri_vol_scs_subcorticalgv.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
sgf_subcort ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_vol_scs_subcorticalgv.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_vol_scs_subcorticalgv.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_vol_scs_subcorticalgv.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD

sgf_subcort ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.
surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate


##STD JUST cortical area
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_area =~ 0*smri_area_cdk_total.baseline_year_1_arm_1_STD + 1*smri_area_cdk_total.2_year_follow_up_y_arm_1_STD + 2*smri_area_cdk_total.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
sgf_area ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_area_cdk_total.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_area_cdk_total.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_area_cdk_total.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD

sgf_area ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST icv
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_icv =~ 0*smri_vol_scs_intracranialv.baseline_year_1_arm_1_STD + 1*smri_vol_scs_intracranialv.2_year_follow_up_y_arm_1_STD + 2*smri_vol_scs_intracranialv.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
sgf_icv ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_vol_scs_intracranialv.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_vol_scs_intracranialv.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_vol_scs_intracranialv.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD

sgf_icv ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.
surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate


##STD JUST list sort
#MULTIGROUP MODEL
#LCS Model
ULCS<-'

dLIST =~ 1*nihtbx_list_uncorrected.4_year_follow_up_y_arm_1_STD   # Fixed regression of dCOG1 on COG_T2
nihtbx_list_uncorrected.4_year_follow_up_y_arm_1_STD ~ 0*1          # This line constrains the intercept of COG_T2 to 0
nihtbx_list_uncorrected.4_year_follow_up_y_arm_1_STD ~~ 0*nihtbx_list_uncorrected.4_year_follow_up_y_arm_1_STD   # This fixes the variance of the COG_T2 to 0


nihtbx_list_uncorrected.baseline_year_1_arm_1_STD ~  1           # This estimates the intercept of COG_T1
dLIST ~~  dLIST       # This estimates the variance of the change scores
nihtbx_list_uncorrected.baseline_year_1_arm_1_STD ~~   nihtbx_list_uncorrected.baseline_year_1_arm_1_STD    # This estimates the variance of COG_T1
dLIST~nihtbx_list_uncorrected.baseline_year_1_arm_1_STD        # This estimates the self-feedback parameter

dLIST ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'

fitULCS <- lavaan(ULCS, data=data_forlongdata50, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitULCS, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate





#group difference intercept models


##STD JUST pattern
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_pattern =~ 1*nihtbx_pattern_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_pattern_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_pattern_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
igf_pattern~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_pattern_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_pattern_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_pattern_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

igf_pattern ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate



##STD JUST picvocab
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_picvocab =~ 1*nihtbx_picvocab_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_picvocab_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_picvocab_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
igf_picvocab~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_picvocab_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_picvocab_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_picvocab_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

igf_picvocab ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST flanker
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_flanker =~ 1*nihtbx_flanker_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_flanker_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_flanker_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
igf_flanker~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_flanker_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_flanker_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_flanker_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

igf_flanker ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST picture
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_picture =~ 1*nihtbx_picture_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_picture_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_picture_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
igf_picture~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_picture_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_picture_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_picture_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

igf_picture ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST reading
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_reading =~ 1*nihtbx_reading_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_reading_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_reading_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
igf_reading~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_reading_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_reading_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_reading_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

igf_reading ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate




##STD JUST thickness
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_thick =~ 1*smri_thick_cdk_mean.baseline_year_1_arm_1_STD + 1*smri_thick_cdk_mean.2_year_follow_up_y_arm_1_STD + 1*smri_thick_cdk_mean.4_year_follow_up_y_arm_1_STD  # intercept
igf_thick ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_thick_cdk_mean.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_thick_cdk_mean.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_thick_cdk_mean.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD


igf_thick ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate


##STD JUST cortical volume
#MULTIGROUP MODEL
#LGC Model

gf_lgm_scons2 <-
  '
# LVs
igf_vol =~ 1*smri_vol_cdk_total.baseline_year_1_arm_1_STD + 1*smri_vol_cdk_total.2_year_follow_up_y_arm_1_STD + 1*smri_vol_cdk_total.4_year_follow_up_y_arm_1_STD  # intercept

igf_vol ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_vol_cdk_total.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_vol_cdk_total.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_vol_cdk_total.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD


igf_vol~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate


##STD JUST subcortical volume
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_subcort =~ 1*smri_vol_scs_subcorticalgv.baseline_year_1_arm_1_STD + 1*smri_vol_scs_subcorticalgv.2_year_follow_up_y_arm_1_STD + 1*smri_vol_scs_subcorticalgv.4_year_follow_up_y_arm_1_STD  # intercept
igf_subcort ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_vol_scs_subcorticalgv.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_vol_scs_subcorticalgv.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_vol_scs_subcorticalgv.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD

igf_subcort ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.
surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate


##STD JUST cortical area
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_area =~ 1*smri_area_cdk_total.baseline_year_1_arm_1_STD + 1*smri_area_cdk_total.2_year_follow_up_y_arm_1_STD + 1*smri_area_cdk_total.4_year_follow_up_y_arm_1_STD  # intercept
igf_area ~demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_area_cdk_total.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_area_cdk_total.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_area_cdk_total.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD

igf_area ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST icv
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
igf_icv =~ 1*smri_vol_scs_intracranialv.baseline_year_1_arm_1_STD + 1*smri_vol_scs_intracranialv.2_year_follow_up_y_arm_1_STD + 1*smri_vol_scs_intracranialv.4_year_follow_up_y_arm_1_STD  # intercept
igf_icv ~ demo_comb_income_v2_STD + demo_prnt_ed_v2_STD
smri_vol_scs_intracranialv.baseline_year_1_arm_1_STD ~ pds_avg_BL_STD
smri_vol_scs_intracranialv.2_year_follow_up_y_arm_1_STD ~ pds_avg_y2_STD
smri_vol_scs_intracranialv.4_year_follow_up_y_arm_1_STD ~ pds_avg_y4_STD

igf_icv ~ c(m1, m2, m3) * 1
diff.g2.g1 := m2 - m1
diff.g3.g1 := m3 - m1
diff.g3.g2 := m3 - m2
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE, group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.
surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate




##STD JUST pattern
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_pattern =~ 0*nihtbx_pattern_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_pattern_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_pattern_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
igf_pattern =~ 1*nihtbx_pattern_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_pattern_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_pattern_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
sgf_pattern~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
igf_pattern~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_pattern_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_pattern_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_pattern_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate



##STD JUST picvocab
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_picvocab =~ 0*nihtbx_picvocab_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_picvocab_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_picvocab_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
igf_picvocab =~ 1*nihtbx_picvocab_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_picvocab_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_picvocab_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
sgf_picvocab~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
igf_picvocab~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_picvocab_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_picvocab_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_picvocab_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST flanker
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_flanker =~ 0*nihtbx_flanker_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_flanker_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_flanker_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
igf_flanker =~ 1*nihtbx_flanker_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_flanker_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_flanker_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
sgf_flanker~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
igf_flanker~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_flanker_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_flanker_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_flanker_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST picture
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_picture =~ 0*nihtbx_picture_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_picture_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_picture_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
igf_picture =~ 1*nihtbx_picture_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_picture_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_picture_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
sgf_picture~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
igf_picture~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_picture_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_picture_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_picture_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate

##STD JUST reading
#MULTIGROUP MODEL
#LGC Model
gf_lgm_scons2 <-
  '
# LVs
sgf_reading =~ 0*nihtbx_reading_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_reading_uncorrected.2_year_follow_up_y_arm_1_STD + 2*nihtbx_reading_uncorrected.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves
igf_reading =~ 1*nihtbx_reading_uncorrected.baseline_year_1_arm_1_STD + 1*nihtbx_reading_uncorrected.2_year_follow_up_y_arm_1_STD + 1*nihtbx_reading_uncorrected.4_year_follow_up_y_arm_1_STD       # intercept

#regression
sgf_reading~ sex_STD + vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
igf_reading~sex_STD+ vist_type_Y4_remote_STD+demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
nihtbx_reading_uncorrected.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
nihtbx_reading_uncorrected.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
nihtbx_reading_uncorrected.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=data, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.

surveyfitmod1 <- lavaan.survey(lavaan.fit = fitgf_lgm_scons2, survey.design = groups)
summary(surveyfitmod1)
parameterEstimates(surveyfitmod1, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate




#MULTIGROUP MODEL
#LGC Model

gf_lgm_scons2 <-
  '
# LVs
igf_thick =~ 1*smri_thick_cdk_mean.baseline_year_1_arm_1_STD + 1*smri_thick_cdk_mean.2_year_follow_up_y_arm_1_STD + 1*smri_thick_cdk_mean.4_year_follow_up_y_arm_1_STD       # intercept
sgf_thick =~ 0*smri_thick_cdk_mean.baseline_year_1_arm_1_STD + 1*smri_thick_cdk_mean.2_year_follow_up_y_arm_1_STD + 2*smri_thick_cdk_mean.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

# Covariances
sgf_thick~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
igf_thick~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
smri_thick_cdk_mean.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
smri_thick_cdk_mean.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
smri_thick_cdk_mean.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=datamatch, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.


gf_lgm_scons2 <-
  '
# LVs
igf_cort =~ 1*smri_vol_cdk_total.baseline_year_1_arm_1_STD + 1*smri_vol_cdk_total.2_year_follow_up_y_arm_1_STD + 1*smri_vol_cdk_total.4_year_follow_up_y_arm_1_STD       # intercept
sgf_cort =~ 0*smri_vol_cdk_total.baseline_year_1_arm_1_STD + 1*smri_vol_cdk_total.2_year_follow_up_y_arm_1_STD + 2*smri_vol_cdk_total.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves


igf_cort~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
sgf_cort~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
smri_vol_cdk_mean.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
smri_vol_cdk_mean.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
smri_vol_cdk_mean.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD


# Covariances
sgf_cort ~~ igf_cort
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=datamatch, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.



gf_lgm_scons2 <-
  '
# LVs
igf_subcort =~ 1*smri_vol_scs_subcorticalgv.baseline_year_1_arm_1_STD + 1*smri_vol_scs_subcorticalgv.2_year_follow_up_y_arm_1_STD + 1*smri_vol_scs_subcorticalgv.4_year_follow_up_y_arm_1_STD       # intercept
sgf_subcort =~ 0*smri_vol_scs_subcorticalgv.baseline_year_1_arm_1_STD + 1*smri_vol_scs_subcorticalgv.2_year_follow_up_y_arm_1_STD + 2*smri_vol_scs_subcorticalgv.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

igf_subcort~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
sgf_subcort~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
smri_vol_scs_subcorticalgv.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
smri_vol_scs_subcorticalgv.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
smri_vol_scs_subcorticalgv.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

# Covariances
sgf_subcort ~~ igf_subcort
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=datamatch, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.



gf_lgm_scons2 <-
  '
# LVs
igf_area =~ 1*smri_area_cdk_total.baseline_year_1_arm_1_STD + 1*smri_area_cdk_total.2_year_follow_up_y_arm_1_STD + 1*smri_area_cdk_total.4_year_follow_up_y_arm_1_STD       # intercept
sgf_area =~ 0*smri_area_cdk_total.baseline_year_1_arm_1_STD + 1*smri_area_cdk_total.2_year_follow_up_y_arm_1_STD + 2*smri_area_cdk_total.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

igf_area~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
sgf_area~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
smri_area_cdk_total.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
smri_area_cdk_total.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
smri_area_cdk_total.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

# Covariances
sgf_area ~~ igf_area
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=datamatch, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.


gf_lgm_scons2 <-
  '
# LVs
igf_icv =~ 1*smri_vol_scs_intracranialv.baseline_year_1_arm_1_STD + 1*smri_vol_scs_intracranialv.2_year_follow_up_y_arm_1_STD + 1*smri_vol_scs_intracranialv.4_year_follow_up_y_arm_1_STD       # intercept
sgf_icv =~ 0*smri_vol_scs_intracranialv.baseline_year_1_arm_1_STD + 1*smri_vol_scs_intracranialv.2_year_follow_up_y_arm_1_STD + 2*smri_vol_scs_intracranialv.4_year_follow_up_y_arm_1_STD  # slope, based on mean interval between waves

igf_icv~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
sgf_icv~demo_comb_income_v2_STD+demo_prnt_ed_v2_STD
smri_vol_scs_intracranialv.baseline_year_1_arm_1_STD ~age0_STD + pds_avg_BL_STD
smri_vol_scs_intracranialv.2_year_follow_up_y_arm_1_STD ~ age2_STD + pds_avg_y2_STD
smri_vol_scs_intracranialv.4_year_follow_up_y_arm_1_STD ~ age4_STD + pds_avg_y4_STD

# Covariances
sgf_icv ~~ igf_icv
'
fitgf_lgm_scons2 <- growth(gf_lgm_scons2, data=datamatch, estimator='mlr', fixed.x=F, missing='fiml', meanstructure = TRUE,  group="group3_newscore")
summary(fitgf_lgm_scons2, fit.measures=T, standardized=T, rsquare=T)
parameterEstimates(fitgf_lgm_scons2, boot.ci.type="bca.simple",level=0.95, ci=TRUE,standardized = FALSE,  output = "text") # Estimate CIs.



#latent profile analyses
#LPA packages
library(tidyLPA)
library(dplyr)
library(mclust)

#load data without pguids
pps3 <- fread("~/Documents/ABCD/Datasets/pps.csv", select = c(2:6))
#load data with pguids
pps5 <- fread("~/Documents/ABCD/Datasets/pps.csv", select = c(1:6))

#scale pps variables
ppsZ <- pps_50_scaled[ppsZitems]
ppsZ <- as.data.frame(ppsZ)
#do complete casese
pps4 <- ppsZ[complete.cases(ppsZ), ]
pps5 <- pps_50_scaled[complete.cases(pps_50_scaled), ]

#LPA profiles 1 through 5
m2 <- pps4 %>%
  single_imputation() %>%
  estimate_profiles(1:5)
comp <- suppressWarnings(compare_solutions(m2))

#get data for best fitting model
m2 <- ppsZ %>%
  single_imputation() %>%
  estimate_profiles(c(2))
get_data(m2)

#examine frequencies of classes
res <- saveM2 %>% group_by(Class) %>% summarise(Freq=n())

#save the data for best fitting model
saveM2 <- get_data(m2)



