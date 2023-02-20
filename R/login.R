################################################################################
## Project: Exposome and Mental Health
## Script purpose: Log in to servers  
## Date: 23th April 2022
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

#install_github("lifecycle-project/ds-helper")

################################################################################
# 1. Get tokens  
################################################################################
token_abcd <- armadillo.get_token("https://armadillo.abcdstudie.nl")
token_alspac <- armadillo.get_token("https://alspac-armadillo.molgenis.org")
token_bib <- armadillo.get_token("https://bib-armadillo.molgenis.org")
token_eden <- armadillo.get_token("https://armadillo.sicopre.elfe-france.fr")

################################################################################
# 2. Define cohort details
################################################################################
builder <- DSI::newDSLoginBuilder()

## ---- ABCD -------------------------------------------------------------------
builder$append(
  server = "abcd",
  url = "https://armadillo.abcdstudie.nl",
  table = "urbandevelopment/2_3_core_2_2/core_non_rep",
  token = token_abcd,
  driver = "ArmadilloDriver")

## ---- ALSPAC -----------------------------------------------------------------
builder$append(
  server = "alspac",
  url = "https://alspac-armadillo.molgenis.org",
  table = "lc18/2_1_core_1_4/non_rep",
  token = token_alspac,
  driver = "ArmadilloDriver")

## ---- BiB --------------------------------------------------------------------
builder$append(
  server = "bib",
  url = "https://bib-armadillo.molgenis.org",
  table = "sp482/2_2_core_1_5/non_rep",
  token = token_bib,
  driver = "ArmadilloDriver")

## ---- DNBC -------------------------------------------------------------------
builder$append(
  server = "dnbc",
  url = "https://opal.sund.ku.dk",
  user = "acbinter",
  password = "H5sR2auOPE",
  table = "lc_dnbc_core_2_2.2_2_core_non_rep_acbinter_2021-lc01",
  driver = "OpalDriver")

## ---- EDEN -------------------------------------------------------------------
builder$append(
  server = "eden_nan",
  url = "https://armadillo.sicopre.elfe-france.fr",
  token = token_eden,
  table = "project25-eden/2_1_core_1_0/non_rep",
  driver = "ArmadilloDriver")

## ---- EDEN -------------------------------------------------------------------
builder$append(
  server = "eden_poit",
  url = "https://armadillo.sicopre.elfe-france.fr",
  token = token_eden,
  table = "project25-eden/2_1_core_1_0/non_rep",
  driver = "ArmadilloDriver")

## ---- GEN-R ------------------------------------------------------------------
builder$append(
  server = "genr",
  url = "https://opal.erasmusmc.nl/",
  user = "A.C.Binter",
  password = "kc:Qu(nMv)4M",
  table = "lc_genr_core_2_2.2_2_core_non_rep_MG_ACB _ECCNLC202048",
  driver = "OpalDriver")

## ---- INMA -------------------------------------------------------------------
builder$append(
  server = "inma_gip",
  url = "https://opal.isglobal.org/repo",
  user = "lifecycle_UrbanBehav",
  password = "cfKax0puSP0rpXMA8UYa",
  table = "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_UrbanBehav", 
  profile = "rock-inma",
  driver = "OpalDriver")

## ---- INMA -------------------------------------------------------------------
builder$append(
  server = "inma_sab",
  url = "https://opal.isglobal.org/repo",
  user = "lifecycle_UrbanBehav",
  password = "cfKax0puSP0rpXMA8UYa",
  table = "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_UrbanBehav",
  driver = "OpalDriver", 
  profile = "rock-inma")

## ---- INMA -------------------------------------------------------------------
builder$append(
  server = "inma_val",
  url = "https://opal.isglobal.org/repo",
  user = "lifecycle_UrbanBehav",
  password = "cfKax0puSP0rpXMA8UYa",
  table = "lc_isglobal_core_2_1.2_1_core_1_1_non_rep_UrbanBehav",
  driver = "OpalDriver", 
  profile = "rock-inma")

## ---- MoBa -------------------------------------------------------------------
builder$append(
  server = "moba",
  url = "https://datashield.prod.nhn.no",
  user = "tim_cadman",
  password = "x46Th85%yPLw9",
  table = "lc_moba_core_2_1.2_1_core_2022_3_non_rep_urban_exposome_cognitive_motor_new",
  driver = "OpalDriver")

## ---- NINFEA -----------------------------------------------------------------
builder$append(
  server = "ninfea",
  url = "https://www.lifecycle.unito.it",
  user = "ac.binter",
  password = "mh$2412ar!",
  table = "lc_ninfea_core_2_1.p14_binter_nonrep",
  driver = "OpalDriver")

## ---- RHEA -------------------------------------------------------------------
builder$append(
  server = "rhea",
  url = "https://rheacohort.med.uoc.gr/",
  user = "acbinter",
  password = "zDLvYdwm",
  table = "lc_rhea_core_2_1.acbinter_nonrep",
  driver = "OpalDriver")

logindata <- builder$build()

################################################################################
# 3. Login to servers
################################################################################
conns <- DSI::datashield.login(logins = logindata, assign = FALSE)
