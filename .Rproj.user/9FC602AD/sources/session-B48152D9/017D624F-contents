### These functions were modified from `rFIA` (Stanke et al.) by Daniel Perret to support range-wide status and trends assessments for subalpine fir in the western US. See Perret et al. 2023 ("Range-wide status and trends assessments for subalpine fir indicate widespread disturbance-driven decline", Forest Ecology and Management) for more details. 


## DEFINING GRM COMPONENTS

typeDomain_grow_dlp.metric <- function(db, treeType, landType, sizeThresh, evals) {
  # evals is a list of EVALIDs that you want to use
  
  eval.plots <- db$POP_PLOT_STRATUM_ASSGN %>% 
    filter(EVALID %in% evals) %>% 
    distinct(PLT_CN) %>%
    pull()
  
  ## Build domain indicator variable which is 1 if observation meets criteria, and 0 otherwise
  # Land type domain indicator
  if (tolower(landType) == 'forest'){
    db$COND$landD <- ifelse(db$COND$COND_STATUS_CD==1, 1, 0)
  }
  
  if (tolower(treeType) == 'all'){
    db$TREE$typeD <- 1
  }
  
  # here we recode GRM components -- these are blank or wrong for much of the western US
  
  db$newCOMPONENT <- db$TREE %>% 
    filter(PLT_CN%in%eval.plots) %>% #limit trees to those on plots included in the EVALS we want
    select(TRE_CN,PLT_CN,INVYR,SPCD,AGENTCD,DIA,PREVDIA,PREV_TRE_CN,
           STATUSCD,PREV_STATUS_CD,RECONCILECD,state_key,
           CONDID,PREVCOND,
           contains("UNADJ")) %>% 
    left_join(., db$TREE %>% 
                select(TRE_CN,TPA_UNADJ),
              by=c("PREV_TRE_CN"="TRE_CN"),
              suffix=c("",".prev")) %>% 
    rename(old.tpa = TPA_UNADJ.prev,
           old.CN = PREV_TRE_CN) %>% 
    left_join(., db$TREE_GRM_COMPONENT %>% 
                select(TRE_CN, SUBP_TPAMORT_UNADJ_AL_FOREST,
                       SUBP_TPAREMV_UNADJ_AL_FOREST, 
                       SUBP_TPAGROW_UNADJ_AL_FOREST,
                       SUBP_COMPONENT_AL_FOREST,
                       SUBP_SUBPTYP_GRM_AL_FOREST),
              by="TRE_CN") %>% 
    left_join(., db$PLOT %>% select(PLT_CN,REMPER),
              by="PLT_CN") %>% 
    mutate(PREV_STATUS_CD = ifelse(PREV_STATUS_CD%in%c(2,3,0) & STATUSCD==1,
                                   1, PREV_STATUS_CD),
           AGENTCD = ifelse(is.na(AGENTCD), 0, AGENTCD),
           STATUSCD = ifelse(STATUSCD==3, 2, STATUSCD),
           RECONCILECD = ifelse(is.na(RECONCILECD), 0, RECONCILECD),
           
           ## explanation of reCOMPONENT classification: 
           ## survivor/mortality1 = above thresh at both times
           ## survivor/mortality2 = crossed thresh
           ## survivor/mortality0 = stayed below thresh
           ## ingrowth1 = new tree, above thresh but still on microplot
           ## ingrowth2 = this is  "ongrowth" -- trees that would have been counted but was too small at t1. we *dont* want to include this! Presumed miscounted if grew directly onto subplot. Following FIA convention via AG.
           ## ingrowth0 = new tree, below thresh
           ## note: mortality includes harvest; agentcd==80
           
           reCOMPONENT = case_when(
             #previous dead
             (STATUSCD==2 & PREV_STATUS_CD==2) | RECONCILECD==4 ~ "PREVDEAD",
             
             #full information
             STATUSCD==1 & PREV_STATUS_CD==1 & PREVDIA>=sizeThresh ~ "SURVIVOR1",
             STATUSCD==1 & PREV_STATUS_CD==1 & PREVDIA<sizeThresh & DIA>=sizeThresh ~ "SURVIVOR2",
             STATUSCD==1 & PREV_STATUS_CD==1 & DIA<sizeThresh ~ "SURVIVOR0",
             
             STATUSCD==2 & PREV_STATUS_CD==1 & PREVDIA>=sizeThresh ~ "MORTALITY1",
             STATUSCD==2 & PREV_STATUS_CD==1 & PREVDIA<sizeThresh & DIA>=sizeThresh ~ "MORTALITY2",
             STATUSCD==2 & PREV_STATUS_CD==1 & DIA<sizeThresh ~ "MORTALITY0",
             
             STATUSCD==1 & is.na(PREV_STATUS_CD) & RECONCILECD==0 & DIA>=sizeThresh & DIA<12.7 ~ "INGROWTH1",
             STATUSCD==1 & is.na(PREV_STATUS_CD) & RECONCILECD==0 & DIA>=sizeThresh & DIA>=12.7 ~ "INGROWTH2",
             STATUSCD==1 & is.na(PREV_STATUS_CD) & RECONCILECD==0 & DIA<sizeThresh ~ "INGROWTH0",
             STATUSCD==2 & is.na(PREV_STATUS_CD) & RECONCILECD==0 & DIA>=sizeThresh ~ "MORTALITY2",
             STATUSCD==2 & is.na(PREV_STATUS_CD) & RECONCILECD==0 & DIA<sizeThresh ~ "MORTALITY0",
             
             # reconciled trees
             RECONCILECD==1 & STATUSCD==1 & DIA>=sizeThresh & DIA<12.7 ~ "INGROWTH1",
             RECONCILECD==1 & STATUSCD==1 & DIA>=sizeThresh & DIA>=12.7 ~ "INGROWTH2",
             RECONCILECD==1 & STATUSCD==1 & DIA<sizeThresh ~ "INGROWTH0",
             RECONCILECD==1 & STATUSCD==2 & DIA>=sizeThresh ~ "MORTALITY2",
             RECONCILECD==1 & STATUSCD==2 & DIA<sizeThresh ~ "MORTALITY0",
             
             RECONCILECD==2 & STATUSCD==1 & DIA>=sizeThresh ~ "INGROWTH1",
             RECONCILECD==2 & STATUSCD==1 & DIA<sizeThresh ~ "INGROWTH0",
             RECONCILECD==2 & STATUSCD==2 & DIA>=sizeThresh ~ "MORTALITY2",
             RECONCILECD==2 & STATUSCD==2 & DIA<sizeThresh ~ "MORTALITY0",
             
             RECONCILECD==3 & STATUSCD==1 & DIA>=sizeThresh ~ "SURVIVOR1",
             RECONCILECD==3 & STATUSCD==1 & DIA<sizeThresh ~ "SURVIVOR0",
             RECONCILECD==3 & STATUSCD==2 & DIA>=sizeThresh ~ "MORTALITY1",
             RECONCILECD==3 & STATUSCD==2 & DIA<sizeThresh ~ "MORTALITY0",
             
             #unclassified
             TRUE ~ "UNCLASSIFIED"
           ),
           
           TPARECR_UNADJ = ifelse(reCOMPONENT %in% c("INGROWTH1",
                                                     "SURVIVOR2"
                                                     #"MORTALITY2"
           ), 
           TPA_UNADJ, 0)) %>% 
    
    mutate(TPA_UNADJ = ifelse(is.na(TPA_UNADJ),0,TPA_UNADJ),
           TPAMORT_UNADJ = ifelse(is.na(TPAMORT_UNADJ),0,TPAMORT_UNADJ),
           TPAGROW_UNADJ = ifelse(is.na(TPAGROW_UNADJ),0,TPAGROW_UNADJ),
           TPAREMV_UNADJ = ifelse(is.na(TPAREMV_UNADJ),0,TPAREMV_UNADJ),
           TPARECR_UNADJ = ifelse(is.na(TPARECR_UNADJ),0,TPARECR_UNADJ))
  
  
  return(db)
}



### GROW MORT base function

growMortStarter_dlp.metric <- function(x,
                                       db,
                                       grpBy_quo = NULL,
                                       polys = NULL,
                                       returnSpatial = FALSE,
                                       bySpecies = FALSE,
                                       bySizeClass = FALSE,
                                       landType = 'forest',
                                       treeType = 'all',
                                       method = 'TI',
                                       lambda = .5,
                                       stateVar = 'TPA',
                                       treeDomain = NULL,
                                       areaDomain = NULL,
                                       totals = FALSE,
                                       byPlot = FALSE,
                                       treeList = FALSE,
                                       nCores = 1,
                                       remote,
                                       mr,
                                       sizeThresh,
                                       evals){
  
  ## Prep other variables ------------------------------------------------------
  ## Need a plotCN, and a new ID
  db$PLOT <- db$PLOT %>%
    dplyr::mutate(PLT_CN = CN,
                  pltID = stringr::str_c(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_'))
  db$TREE <- db$TREE %>%
    dplyr::mutate(TRE_CN = CN)
  
  ## Convert grpBy to character
  grpBy <- rFIA:::grpByToChar(db, grpBy_quo)
  
  # I like a unique ID for a plot through time
  if (byPlot | byPlot) {grpBy <- c('pltID', grpBy)}
  
  ## Intersect plots with polygons if polygons are given
  if (!is.null(polys)){
    
    ## Add shapefile names to grpBy
    grpBy = c(grpBy, names(polys)[names(polys) != 'geometry'])
    ## Do the intersection
    db <- rFIA:::arealSumPrep2(db, grpBy, polys, nCores, remote)
    
    ## If there's nothing there, skip the state
    if (is.null(db)) return('no plots in polys')
  }
  
  ## If we want to return spatial plots
  if (byPlot & returnSpatial){
    grpBy <- c(grpBy, 'LON', 'LAT')
  }
  
  ### HANDLE THE STATE VARIABLE, only applying to the midpoint table for consistency
  if (stringr::str_to_upper(stateVar) == 'TPA'){
    db$TREE_GRM_MIDPT$state <- 1
    db$TREE$state_recr <- 1
  } else if (stringr::str_to_upper(stateVar) == 'BAA'){
    #db$TREE_GRM_MIDPT$state <- rFIA:::basalArea(db$TREE_GRM_MIDPT$DIA*2.54)
    
    db$TREE_GRM_MIDPT$state <- (((db$TREE_GRM_MIDPT$DIA*2.54)^2)*(pi/4))/1e4
    
    #db$TREE$state_recr <- rFIA:::basalArea(db$TREE$DIA)
    
    db$TREE$state_recr <- ((db$TREE$DIA^2)*(pi/4))/1e4
    
  } else if (stringr::str_to_upper(stateVar) == 'NETVOL'){
    db$TREE_GRM_MIDPT$state <- db$TREE_GRM_MIDPT$VOLCFNET
    db$TREE$state_recr <- db$TREE$VOLCFNET
  } else if (stringr::str_to_upper(stateVar) == 'SNDVOL'){
    db$TREE_GRM_MIDPT$state <- db$TREE_GRM_MIDPT$VOLCFSND
    db$TREE$state_recr <- db$TREE$VOLCFSND
  } else if (stringr::str_to_upper(stateVar) == 'BIO_AG'){
    db$TREE_GRM_MIDPT$state <- db$TREE_GRM_MIDPT$DRYBIO_AG
    db$TREE$state_recr <- db$TREE$DRYBIO_AG
  } else if (stringr::str_to_upper(stateVar) == 'BIO_BG'){
    db$TREE_GRM_MIDPT$state <- db$TREE_GRM_MIDPT$DRYBIO_BG
    db$TREE$state_recr <- db$TREE$DRYBIO_BG
  } else if (stringr::str_to_upper(stateVar) == 'BIO'){
    db$TREE_GRM_MIDPT$state <- db$TREE_GRM_MIDPT$DRYBIO_BG + db$TREE_GRM_MIDPT$DRYBIO_AG
    db$TREE$state_recr <- db$TREE$DRYBIO_BG + db$TREE$DRYBIO_AG
  } else if (stringr::str_to_upper(stateVar) == 'CARB_AG'){
    db$TREE_GRM_MIDPT$state <- db$TREE_GRM_MIDPT$DRYBIO_AG * .5
    db$TREE$state_recr <- db$TREE$DRYBIO_AG * .5
  } else if (stringr::str_to_upper(stateVar) == 'CARB_BG'){
    db$TREE_GRM_MIDPT$state <- db$TREE_GRM_MIDPT$DRYBIO_BG * .5
    db$TREE$state_recr <- db$TREE$DRYBIO_BG * .5
  } else if (stringr::str_to_upper(stateVar) == 'CARB'){
    db$TREE_GRM_MIDPT$state <- (db$TREE_GRM_MIDPT$DRYBIO_AG + db$TREE_GRM_MIDPT$DRYBIO_BG) * .5
    db$TREE$state_recr <- (db$TREE$DRYBIO_AG + db$TREE$DRYBIO_BG) * .5
  } else {
    stop(paste0('Method not known for stateVar: ', stateVar, '. Please choose one of: TPA, BAA, SAWVOL, SAWVOL_BF, NETVOL, BIO_AG, BIO_BG, BIO, CARB_AG, CARB_BG, or CARB.' ))
  }
  
  ## Build a domain indicator for each observation (1 or 0) --------------------
  ## Land type and tree type combined
  db <- typeDomain_grow_dlp.metric(db = db, 
                                   treeType=treeType,
                                   landType=landType,
                                   sizeThresh=sizeThresh,
                                   evals=evals)
  
  ## Spatial boundary
  if(!is.null(polys)){
    db$PLOT$sp <- ifelse(!is.na(db$PLOT$polyID), 1, 0)
  } else {
    db$PLOT$sp <- 1
  }
  
  # User defined domain indicator for area (ex. specific forest type)
  db <- rFIA:::udAreaDomain(db, areaDomain)
  
  # Here's some code to fix problems where trees were identified as different species. If that happened, we'll change the SPCD to the *most recent* identification
  # There are other options for this, but this seems reasonable; as the tree is larger and hence easier to identify the second time
  # 
  # db$TREE <- db$TREE %>% 
  #   left_join(.,
  #             db$TREE %>% 
  #               select(PREV_TRE_CN, SPCD) %>% 
  #               rename(LATER_SPCD=SPCD),
  #             by=c("TRE_CN"="PREV_TRE_CN")) %>% 
  #   mutate(SPCD = case_when(SPCD!=LATER_SPCD & !is.na(LATER_SPCD) ~ LATER_SPCD,
  #                           is.na(LATER_SPCD) ~ SPCD,
  #                           TRUE ~ SPCD))
  # 
  # User defined domain indicator for tree (ex. trees > 20 ft tall)
  db <- rFIA:::udTreeDomain(db, treeDomain)
  
  
  
  ## Handle population tables --------------------------------------------------
  ## Filtering out all inventories that are not relevant to the current estimation
  ## type. If using estimator other than TI, handle the differences in P2POINTCNT
  ## and in assigning YEAR column (YEAR = END_INVYR if method = 'TI')
  pops <- rFIA:::handlePops(db, evalType = c('GROW', 'MORT', 'REMV'), method, mr, ga = TRUE)
  
  ## Generate a list that tells us if a plot is ever associated with a growth
  ## accounting inventory. If so, we will be more strict with domain definitions
  plt.ga <- pops %>%
    dtplyr::lazy_dt() %>%
    dplyr::mutate(ga = case_when(GROWTH_ACCT == 'Y' ~ 1,
                                 TRUE ~ 0)) %>%
    dplyr::group_by(PLT_CN) %>%
    dplyr::summarise(ga = sum(ga, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ga = case_when(ga > 0 ~ 1, TRUE ~ 0)) %>%
    as.data.frame()
  
  ## A lot of states do their stratification in such a way that makes it impossible
  ## to estimate variance of annual panels w/ post-stratified estimator. That is,
  ## the number of plots within a panel within an stratum is less than 2. When
  ## this happens, merge strata so that all have at least two obs
  if (stringr::str_to_upper(method) != 'TI') {
    pops <- rFIA:::mergeSmallStrata(db, pops)
  }
  
  ## Break into size classes
  if (bySizeClass){
    grpBy <- c(grpBy, 'sizeClass')
    db$TREE$sizeClass <- makeClasses(db$TREE$DIA, interval = 5, numLabs = TRUE)
    db$TREE <- db$TREE[!is.na(db$TREE$sizeClass),]
  }
  
  
  
  
  ## Prep the tree list --------------------------------------------------------
  ## Narrow up the tables to the necessary variables
  ## Which grpByNames are in which table? Helps us subset below
  grpP <- names(db$PLOT)[names(db$PLOT) %in% grpBy]
  grpC <- names(db$COND)[names(db$COND) %in% grpBy &
                           !c(names(db$COND) %in% grpP)]
  grpT <- names(db$TREE)[names(db$TREE) %in% grpBy &
                           !c(names(db$TREE) %in% c(grpP, grpC))]
  
  eval.plots <- db$POP_PLOT_STRATUM_ASSGN %>% 
    filter(EVALID %in% evals) %>% 
    distinct(PLT_CN) %>%
    pull()
  
  ## Dropping irrelevant rows and columns
  db$PLOT <- db$PLOT %>%
    dplyr::select(c(PLT_CN, STATECD, MACRO_BREAKPOINT_DIA,
                    INVYR, MEASYEAR, PLOT_STATUS_CD,
                    dplyr::all_of(grpP), sp, COUNTYCD,
                    PREV_PLT_CN, REMPER)) %>%
    left_join(db$PLOT %>%
                select(PLT_CN,PLOT_STATUS_CD),
              by=c("PREV_PLT_CN"="PLT_CN"),
              suffix = c("",".prev")) %>%
    ## Drop non-forested plots, outside domain of interest, and those that weren't actually measured the first time!
    dplyr::filter(PLOT_STATUS_CD == 1,
                  PLOT_STATUS_CD.prev==1,
                  sp==1,
                  PLT_CN %in% eval.plots) %>%      ### testing this!
    ## Drop visits not used in our eval of interest
    dplyr::filter(PLT_CN %in% pops$PLT_CN)
  
  db$COND <- db$COND %>%
    dplyr::select(c(PLT_CN, CONDPROP_UNADJ, PROP_BASIS,
                    COND_STATUS_CD, CONDID,
                    dplyr::all_of(grpC), aD, landD)) %>%
    ## Drop non-forested plots, and those otherwise outside our domain of interest
    dplyr::filter(aD == 1 & landD == 1) %>%
    ## Drop visits not used in our eval of interest
    dplyr::filter(PLT_CN %in% c(db$PLOT$PLT_CN, db$PLOT$PREV_PLT_CN))
  
  # db$TREE_GRM_COMPONENT <- db$TREE_GRM_COMPONENT %>%
  #   dplyr::select(c(PLT_CN, TRE_CN, SUBPTYP_GRM, TPAGROW_UNADJ, TPARECR_UNADJ,
  #                   TPAREMV_UNADJ, TPAMORT_UNADJ, reCOMPONENT)) %>%
  #   dplyr::filter(TPAGROW_UNADJ > 0 | TPARECR_UNADJ > 0 | TPAREMV_UNADJ > 0 | TPAMORT_UNADJ > 0) %>%
  #   ## Drop visits not used in our eval of interest
  #   dplyr::filter(PLT_CN %in% db$PLOT$PLT_CN) %>%
  #   dplyr::select(-c(PLT_CN))
  
  # db$newCOMPONENT <- db$newCOMPONENT %>% 
  #   filter(TPAGROW_UNADJ>0 | TPARECR_UNADJ>0 | TPAREMV_UNADJ>0 | TPAMORT_UNADJ>0 | TPA_UNADJ>0,
  #          PLT_CN %in% c(db$PLOT$PLT_CN, db$PLOT$PREV_PLT_CN))
  
  db$TREE <- db$TREE %>%
    dplyr::select(c(PLT_CN, SPCD, CONDID, PREVCOND, TRE_CN,
                    PREV_TRE_CN, SUBP, TREE, dplyr::all_of(grpT), tD,
                    typeD, state_recr, TPA_UNADJ,
                    STATUSCD, DIA)) %>%
    ## Drop plots outside our domain of interest
    dplyr::filter(PLT_CN %in% c(db$PLOT$PLT_CN, db$PLOT$PREV_PLT_CN))
  
  db$TREE_GRM_MIDPT <- db$TREE_GRM_MIDPT %>%
    select(c(TRE_CN, DIA, state)) %>%
    mutate(DIA=DIA*2.54) %>% 
    filter(TRE_CN %in% db$newCOMPONENT$TRE_CN)
  
  if ('SUBP_COND_CHNG_MTRX' %in% names(db)) {
    db$SUBP_COND_CHNG_MTRX <- dplyr::select(db$SUBP_COND_CHNG_MTRX, PLT_CN, PREV_PLT_CN,
                                            SUBPTYP, SUBPTYP_PROP_CHNG, PREVCOND, CONDID) %>%
      dplyr::filter(PLT_CN %in% c(db$PLOT$PLT_CN, db$PLOT$PREV_PLT_CN))
  }
  
  # Separate area grouping names from tree grouping names
  if (!is.null(polys)){
    aGrpBy <- grpBy[grpBy %in% c(names(db$PLOT), names(db$COND), names(polys))]
  } else {
    aGrpBy <- grpBy[grpBy %in% c(names(db$PLOT), names(db$COND))]
  }
  
  
  ## Full tree list -----
  
  data <- db$newCOMPONENT %>% 
    select(-which(names(.)%in%grpT)) %>% 
    dtplyr::lazy_dt() %>% 
    filter(PLT_CN %in% db$PLOT$PLT_CN) %>% ## added this to make sure we don't have NAs from unmatched plots
    left_join(db$PLOT %>% 
                select(PLT_CN, STATECD, MACRO_BREAKPOINT_DIA, INVYR,
                       MEASYEAR, PLOT_STATUS_CD, PREV_PLT_CN,
                       dplyr::all_of(grpP), sp),
              by= "PLT_CN") %>%
    # left_join(all.fia$PLOT %>%
    #             select(PLT_CN,PLOT_STATUS_CD) %>% 
    #           rename(PREV_PLT_CN=PLT_CN),
    #           by="PREV_PLT_CN",
    #           #by=c("PREV_PLT_CN"="PLT_CN"),
    #           suffix=c("",".prev")) %>%
    # filter(PLOT_STATUS_CD==1,
    #        PLOT_STATUS_CD.prev==1) %>% #already implemented above!
    left_join(db$TREE %>%
                select(TRE_CN,
                       PREV_TRE_CN, SUBP, TREE, dplyr::all_of(grpT),
                       tD, typeD, state = state_recr),
              by="TRE_CN") %>% 
    left_join(db$COND %>% 
                select(PLT_CN,CONDID,
                       CONDPROP_UNADJ, PROP_BASIS,
                       COND_STATUS_CD, dplyr::all_of(grpC),
                       aD, landD),
              by=c("PLT_CN",
                   "CONDID")) %>% 
    left_join(plt.ga, by = "PLT_CN") %>% 
    left_join(db$PLOT %>% 
                select(PLT_CN, dplyr::all_of(grpP), sp),
              by=c("PREV_PLT_CN"="PLT_CN"),
              suffix = c("",".prev")) %>% 
    left_join(db$COND %>% 
                select(PLT_CN, CONDID, landD, aD, dplyr::all_of(grpC),
                       COND_STATUS_CD),
              by=c("PREV_PLT_CN" = "PLT_CN", "PREVCOND" = "CONDID"),
              suffix = c("",".prev")) %>% 
    left_join(db$TREE %>% 
                select(TRE_CN, dplyr::all_of(grpT),
                       typeD, tD, DIA, STATUSCD, TPA_UNADJ, state = state_recr),
              by = c("PREV_TRE_CN" = "TRE_CN"), 
              suffix = c("",".prev")) %>%   
    
    ## dealing with tree basis (plot type) for area estimation
    
    mutate(SUBPTYP = dplyr::case_when((is.na(DIA) & is.na(PREVDIA)) ~ 0, #modified from rFIA:::tpa_starter()
                                      (is.na(DIA) & PREVDIA<12.7) ~ 2,
                                      (is.na(DIA) & PREVDIA>=12.7 & is.na(MACRO_BREAKPOINT_DIA)) ~ 1, #... some of these have macroplot TPA_UNADJ though....
                                      (is.na(DIA) & PREVDIA>=12.7 & PREVDIA < (MACRO_BREAKPOINT_DIA*2.54)) ~ 1,
                                      (is.na(DIA) & PREVDIA>=(MACRO_BREAKPOINT_DIA*2.54)) ~ 3,
                                      DIA < 12.7 ~ 2, 
                                      DIA >= 12.7 & is.na((MACRO_BREAKPOINT_DIA*2.54)) ~ 1, 
                                      DIA >= 12.7 & DIA < (MACRO_BREAKPOINT_DIA*2.54) ~ 1, 
                                      DIA >= (MACRO_BREAKPOINT_DIA*2.54) ~ 3)) %>% 
    
    ##### MODIFIED TPA_UNADJ FACTORS -----
  
  mutate(state = ifelse(RECONCILECD %in% 3:4 & stateVar!="TPA", 0, state),
         state.prev = case_when(!is.na(state.prev) ~ state.prev,
                                RECONCILECD %in% 3:4 & stateVar=="TPA" ~ 1,
                                RECONCILECD %in% 3:4 & stateVar!="TPA" ~ 0,
                                TRUE ~ 0),
         TPAREMV_UNADJ = TPAREMV_UNADJ * state,
         TPA_UNADJ.orig = TPA_UNADJ, #unmodified from db; old.tpa is T1 TPA

         #TPAMORT_UNADJ = TPAMORT_UNADJ * state,
         #TPARECR_UNADJ = TPARECR_UNADJ * state, # TPARECR hasn't been corrected; can be done using same scheme as below
         
         TPARECR_UNADJ = case_when(reCOMPONENT %in% c("SURVIVOR2","INGROWTH1") & RECONCILECD==3 ~ TPA_UNADJ.orig*state,
                               reCOMPONENT %in% c("SURVIVOR2") & (DIA<12.7 | PREVDIA<12.7) ~ old.tpa*state, #freezing at T1
                               reCOMPONENT %in% c("SURVIVOR2") ~ TPAGROW_UNADJ*state,
                               reCOMPONENT %in% c("INGROWTH1") ~ TPA_UNADJ.orig*state,
                               TRUE ~ 0),
         TPA_UNADJ = case_when(reCOMPONENT %in% c("SURVIVOR1","SURVIVOR2","INGROWTH1") & RECONCILECD==3 ~ TPA_UNADJ.orig*state,
                               reCOMPONENT %in% c("SURVIVOR1","SURVIVOR2") & (DIA<12.7 | PREVDIA<12.7) ~ old.tpa*state, #freezing at T1
                               reCOMPONENT %in% c("SURVIVOR1","SURVIVOR2") ~ TPAGROW_UNADJ*state,
                               reCOMPONENT %in% c("INGROWTH1") ~ TPA_UNADJ.orig*state,
                               TRUE ~ 0),
         
         TPA_UNADJ = ifelse(TPA_UNADJ>90, 74.965282, TPA_UNADJ), # this is a correction for old adjustments from the WY periodic inventory
         
         TPA_UNADJ.prev = case_when(reCOMPONENT %in% c("SURVIVOR1") & DIA>=12.7 & RECONCILECD!=3 ~ TPAGROW_UNADJ*state.prev,
                                    reCOMPONENT %in% c("SURVIVOR1") & (DIA<12.7 | RECONCILECD==3) ~ TPA_UNADJ.orig*state.prev,
                                    
                                    reCOMPONENT %in% c("MORTALITY1") & DIA>=12.7 & RECONCILECD!=3 ~ TPAGROW_UNADJ*state.prev,
                                    reCOMPONENT %in% c("MORTALITY1") & (DIA<12.7 | RECONCILECD==3) ~ TPA_UNADJ.orig*state.prev,
                                    reCOMPONENT %in% c("MORTALITY1") & is.na(DIA) ~ old.tpa*state.prev,
                                    TRUE ~ 0),
         
         TPAMORT_UNADJ = ifelse(reCOMPONENT=="MORTALITY1",TPA_UNADJ.prev,0) * state) %>% 
    
    ## DOMAIN INDICATORS?
    mutate(aChng = case_when(ga == 1 & COND_STATUS_CD == 1 & COND_STATUS_CD.prev == 1 & 
                               !is.na(CONDPROP_UNADJ) ~ 1,
                             ga == 0 ~ 1,
                             TRUE ~ 0),
           tChng = case_when(ga == 1 & COND_STATUS_CD == 1 & COND_STATUS_CD.prev == 1 ~ 1,
                             ga == 0 ~ 1,
                             TRUE ~ 0),
           landD.prev = case_when(ga == 1 & landD == 1 & landD.prev == 1 ~ 1,
                                  ga == 0 & is.na(landD) & is.na(landD.prev) ~ 0,
                                  ga == 0 & is.na(landD.prev) ~ landD,
                                  ga == 0 ~ landD.prev,
                                  TRUE ~ 0)) %>% 
    ## If previous attributes are unavailable for trees, default to current
    mutate(tD.prev = ifelse(is.na(tD.prev), tD, tD.prev),
           typeD.prev = ifelse(is.na(typeD.prev), typeD, typeD.prev),
           aD.prev = ifelse(is.na(aD.prev), aD, aD.prev),
           sp.prev = ifelse(is.na(sp.prev), sp, sp.prev)) %>%
    
    ## Comprehensive domain indicators
    mutate(tDI = landD.prev * aD.prev * tD.prev * typeD.prev * sp.prev * tChng,
           tDI_r = landD * aD * tD * typeD * sp * tChng, # All previous attributes NA for recruitment
           aDI = landD * aD * sp * aChng) %>%
    as.data.frame()
  
  if ('SUBP_COND_CHNG_MTRX' %in% names(db)) {
    ### DOING AREA SEPARATELY NOW FOR GROWTH ACCOUNTING PLOTS
    aData <- dplyr::select(db$PLOT, c(PLT_CN, STATECD, MACRO_BREAKPOINT_DIA,
                                      INVYR, MEASYEAR, PLOT_STATUS_CD, PREV_PLT_CN,
                                      REMPER, dplyr::all_of(grpP), sp)) %>%
      dplyr::left_join(dplyr::select(db$SUBP_COND_CHNG_MTRX, PLT_CN, PREV_PLT_CN,
                                     SUBPTYP, SUBPTYP_PROP_CHNG, PREVCOND, CONDID),
                       by = c('PLT_CN', 'PREV_PLT_CN')) %>%
      dplyr::left_join(dplyr::select(db$COND, c(PLT_CN, CONDPROP_UNADJ, PROP_BASIS,
                                                COND_STATUS_CD, CONDID, dplyr::all_of(grpC),
                                                aD, landD)),
                       by = c('PLT_CN', 'CONDID')) %>%
      dplyr::left_join(dplyr::select(db$COND, c(PLT_CN, PROP_BASIS, COND_STATUS_CD,
                                                CONDID, dplyr::all_of(grpC), aD, landD)),
                       by = c('PREV_PLT_CN' = 'PLT_CN', 'PREVCOND' = 'CONDID'), suffix = c('', '.prev')) %>%
      dplyr::mutate(aChng = dplyr::if_else(COND_STATUS_CD == 1 &
                                             COND_STATUS_CD.prev == 1 &
                                             !is.null(CONDPROP_UNADJ) &
                                             SUBPTYP == 1,
                                           1, 0),
                    SUBPTYP_PROP_CHNG = SUBPTYP_PROP_CHNG * .25)
    
    aData$landD <- ifelse(aData$landD == 1 & aData$landD.prev == 1, 1, 0)
    aData$aDI_ga <- aData$landD * aData$aD * aData$sp * aData$aChng
    
  }
  
  
  
  ## Plot-level summaries ------------------------------------------------------
  if (byPlot & !treeList){
    
    grpBy <- c('YEAR', grpBy)
    grpSyms <- syms(grpBy)
    aGrpSyms <- syms(aGrpBy)
    
    
    if ('SUBP_COND_CHNG_MTRX' %in% names(db)) {
      ## Forested land area w growth accounting
      a.ga <- aData %>%
        ## Will be lots of trees here, so CONDPROP listed multiple times
        ## Adding PROP_BASIS so we can handle adjustment factors at strata level
        #distinct(PLT_CN, SUBP, CONDID, .keep_all = TRUE) %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(PLT_CN, !!!aGrpSyms) %>%
        dplyr::summarize(fa_ga = sum(SUBPTYP_PROP_CHNG * aDI_ga, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      #SO FAGA IS 0 FOR MOST PLOTS BECAUSE OF aDI_ga
      a <- data %>%
        ## Will be lots of trees here, so CONDPROP listed multiple times
        ## Adding PROP_BASIS so we can handle adjustment factors at strata level
        dplyr::distinct(PLT_CN, CONDID, .keep_all = TRUE) %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(PLT_CN, !!!aGrpSyms) %>%
        dplyr::summarize(fa = sum(CONDPROP_UNADJ * aDI, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(dplyr::select(a.ga, PLT_CN, !!!aGrpSyms, fa_ga),
                         by = c('PLT_CN', aGrpBy)) %>%
        dplyr::left_join(plt.ga, by = 'PLT_CN') %>%
        dplyr::mutate(PROP_FOREST = case_when(ga == 1 ~ fa_ga,
                                              TRUE ~ fa)) %>%
        dplyr::select(PLT_CN, !!!aGrpSyms, PROP_FOREST) %>%
        as.data.frame()
      
    } else {
      ## Forested land area w/out growth accounting
      a <- data %>%
        ## Will be lots of trees here, so CONDPROP listed multiple times
        ## Adding PROP_BASIS so we can handle adjustment factors at strata level
        dplyr::distinct(PLT_CN, CONDID, .keep_all = TRUE) %>%
        dtplyr::lazy_dt() %>%
        dplyr::group_by(PLT_CN, !!!aGrpSyms) %>%
        dplyr::summarize(PROP_FOREST = sum(CONDPROP_UNADJ * aDI, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        as.data.frame()
    }
    
    t <- data %>%
      dplyr::mutate(YEAR = MEASYEAR) %>%
      dplyr::distinct(PLT_CN, TRE_CN, reCOMPONENT, .keep_all = TRUE) %>%
      #dtplyr::lazy_dt() %>%
      # Compute estimates at plot level
      dplyr::group_by(!!!grpSyms, PLT_CN, REMPER) %>%
      dplyr::summarise(RECR_TPA = sum(TPARECR_UNADJ * tDI_r, na.rm = TRUE),
                       MORT_TPA = sum(TPAMORT_UNADJ * tDI, na.rm = TRUE),
                       REMV_TPA = sum(TPAREMV_UNADJ * tDI, na.rm = TRUE),
                       CURR_TPA = sum(TPA_UNADJ * tDI, na.rm = TRUE),
                       PREV_TPA = sum(TPA_UNADJ.prev * tDI, na.rm = TRUE)) %>%
      #dplyr::mutate(PREV_TPA = PREV_TPA + (MORT_TPA + REMV_TPA)*REMPER) %>% #this out because I already incorporated that into TPA_UNADJ.prev
      dplyr::ungroup() %>%
      dplyr::mutate(CHNG_TPA = (CURR_TPA - PREV_TPA),# / REMPER,
                    GROW_TPA = CHNG_TPA - (RECR_TPA + MORT_TPA + REMV_TPA),
                    RECR_PERC = (RECR_TPA / PREV_TPA) * 100,
                    MORT_PERC = (MORT_TPA / PREV_TPA) * 100,
                    REMV_PERC = (REMV_TPA / PREV_TPA) * 100,
                    GROW_PERC = (GROW_TPA / PREV_TPA) * 100,
                    CHNG_PERC = (CHNG_TPA / PREV_TPA) * 100) %>%
      dplyr::select(PLT_CN, !!!grpSyms, REMPER, 
                    RECR_TPA:REMV_TPA, GROW_TPA, CHNG_TPA, 
                    RECR_PERC:CHNG_PERC, PREV_TPA, CURR_TPA) %>%
      as.data.frame() %>%
      ## Rounding errors will generate tiny values, make them zero
      dplyr::mutate(GROW_TPA = dplyr::case_when(abs(GROW_TPA) < 1e-5 ~ 0,
                                                TRUE ~ GROW_TPA)) %>%
      dplyr::left_join(a, by = c('PLT_CN', aGrpBy)) %>%
      dplyr::distinct()
    
    
    ## Make it spatial
    if (returnSpatial){
      t <- t %>%
        dplyr::filter(!is.na(LAT) & !is.na(LON)) %>%
        sf::st_as_sf(coords = c('LON', 'LAT'),
                     crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
      grpBy <- grpBy[grpBy %in% c('LAT', 'LON') == FALSE]
      
    }
    
    out <- list(tEst = t, grpBy = grpBy, aGrpBy = aGrpBy)
    
    
    
  } else {
    ## Population estimation -----------------------------------------------------
    aGrpSyms <- syms(aGrpBy)
    ## Forested area
    a <- data %>%
      ## Will be lots of trees here, so CONDPROP listed multiple times
      ## Adding PROP_BASIS so we can handle adjustment factors at strata level
      dplyr::distinct(PLT_CN, CONDID, .keep_all = TRUE) %>%
      dplyr::mutate(fa = CONDPROP_UNADJ * aDI) %>%
      dplyr::select(PLT_CN, AREA_BASIS = PROP_BASIS, CONDID, !!!aGrpSyms, fa)
    
    
    if ('SUBP_COND_CHNG_MTRX' %in% names(db)) {
      ### Plot-level estimates -- growth accounting
      a_ga <- aData %>%
        ## Will be lots of trees here, so CONDPROP listed multiple times
        ## Adding PROP_BASIS so we can handle adjustment factors at strata level
        #distinct(PLT_CN, SUBP, CONDID, .keep_all = TRUE) %>%
        dtplyr::lazy_dt() %>%
        dplyr::filter(!is.na(PROP_BASIS)) %>%
        dplyr::group_by(PLT_CN, PROP_BASIS, CONDID, !!!aGrpSyms) %>%
        dplyr::summarize(fa_ga = sum(SUBPTYP_PROP_CHNG * aDI_ga, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        as.data.frame()
      
      a <- a %>%
        dplyr::left_join(dplyr::select(a_ga, PLT_CN, AREA_BASIS = PROP_BASIS, CONDID, !!!aGrpSyms, fa_ga),
                         by = c('PLT_CN', 'AREA_BASIS', 'CONDID', aGrpBy)) %>%
        dplyr::left_join(plt.ga, by = 'PLT_CN') %>%
        dplyr::mutate(fa = case_when(ga == 1 ~ fa_ga,
                                     TRUE ~ fa)) %>%
        dplyr::select(PLT_CN, AREA_BASIS, CONDID, !!!aGrpSyms, fa) %>%
        dplyr::filter(fa > 0)
    }
    
    ## Tree list
    grpSyms <- syms(grpBy)
    ## Tree list                  #### I'M GOING TO TRY REWRITING THIS!
    # t <- data %>%
    #   dplyr::distinct(PLT_CN, TRE_CN, .keep_all = TRUE) %>%
    #   dtplyr::lazy_dt() %>%
    #   dplyr::filter(!is.na(SUBPTYP_GRM)) %>%
    #   dplyr::filter(tDI > 0 | tDI_r > 0) %>%
    #   # Compute estimates at plot level
    #   dplyr::mutate(rPlot = TPARECR_UNADJ * tDI_r,
    #                 mPlot = TPAMORT_UNADJ * tDI,
    #                 hPlot = TPAREMV_UNADJ * tDI,
    #                 tPlot = TPA_UNADJ * tDI,
    #               #  pPlot = (TPA_UNADJ.prev * tDI),# + ((mPlot + hPlot)*REMPER),
    #                 cPlot = (tPlot - pPlot) / REMPER,
    #                 gPlot = cPlot - rPlot + mPlot + hPlot) %>%
    #   dplyr::mutate(TREE_BASIS = case_when(SUBPTYP_GRM == 0 ~ NA_character_,
    #                                        SUBPTYP_GRM == 1 ~ 'SUBP',
    #                                        SUBPTYP_GRM == 2 ~ 'MICR',
    #                                        SUBPTYP_GRM == 3 ~ 'MACR')) %>%
    #   as.data.frame() %>%
    #   dplyr::select(PLT_CN, TREE_BASIS, SUBP, TREE, !!!grpSyms, rPlot:gPlot, contains("UNADJ"),tDI,REMPER)
    
    ### DLP'S VERSION
    
    t <- data %>% 
      dplyr::distinct(PLT_CN, TRE_CN, .keep_all=T) %>% 
      dplyr::filter(tDI==1 | tDI_r==1) %>%  # need this; area estimation in `a`
      dplyr::mutate(rPlot = TPARECR_UNADJ * tDI_r,
                    mPlot = TPAMORT_UNADJ * tDI,
                    hPlot = TPAREMV_UNADJ * tDI,
                    tPlot = TPA_UNADJ * tDI,
                    pPlot = TPA_UNADJ.prev * tDI,
                    cPlot = (tPlot - pPlot),
                    gPlot = TPAGROW_UNADJ * tDI) %>% 
      dplyr::mutate(TREE_BASIS = dplyr::case_when(SUBPTYP == 0 ~ NA_character_,
                                                  SUBPTYP == 1 ~ 'SUBP',
                                                  SUBPTYP == 2 ~ 'MICR',
                                                  SUBPTYP == 3 ~ 'MACR')) %>%
      as.data.frame() %>%
      dplyr::select(PLT_CN, #TRE_CN, REMPER, 
                    TREE_BASIS, SUBP, TREE, !!!grpSyms, rPlot:gPlot)#, contains("UNADJ"),tDI,REMPER)
    
    
    
    
    if (treeList) {
      
      # tEst <- a %>%
      #   dplyr::left_join(t, by = c('PLT_CN', aGrpBy)) %>%
      #   dplyr::mutate(EVAL_TYP = list(c('GROW', 'MORT', 'REMV'))) %>%
      #   dplyr::select(TRE_CN, PLT_CN, EVAL_TYP, TREE_BASIS, AREA_BASIS,
      #                 !!!grpSyms, CONDID, SUBP, TREE,
      #                 reCOMPONENT,
      #                # RATE_TPA = ratePlot,
      #                 RECR_TPA = rPlot,
      #                 MORT_TPA = mPlot,
      #                 REMV_TPA = hPlot,
      #                 GROW_TPA = gPlot,
      #                 CHNG_TPA = cPlot,
      #                 CURR_TPA = tPlot,
      #                 PREV_TPA = pPlot,
      #                 PROP_FOREST = fa)
      #out <- list(tEst = tEst, aEst = NULL, grpBy = grpBy, aGrpBy = aGrpBy)

      tEst <- t %>%
        dplyr::left_join(a, by = c('PLT_CN', aGrpBy)) %>%
        dplyr::mutate(EVAL_TYP = list(c('GROW', 'MORT', 'REMV'))) %>%
        dplyr::select(PLT_CN, EVAL_TYP, TREE_BASIS, AREA_BASIS,
                      !!!grpSyms, CONDID, SUBP, TREE,
                      REMPER,
                      RECR_TPA = rPlot,
                      MORT_TPA = mPlot,
                      REMV_TPA = hPlot,
                      GROW_TPA = gPlot,
                      CHNG_TPA = cPlot,
                      CURR_TPA = tPlot,
                      PREV_TPA = pPlot,
                      PROP_FOREST = fa)
      out <- list(tEst = tEst, aEst = NULL, grpBy = grpBy, aGrpBy = aGrpBy)
      
      
      
    } else {
      
      ## Sum variable(s) up to plot-level and adjust for non-response
      tPlt <- rFIA:::sumToPlot(t, pops, grpBy) %>% 
        left_join(db$newCOMPONENT %>% select(PLT_CN,REMPER) %>% distinct(),by="PLT_CN") %>% 
        mutate(cPlot = (tPlot-pPlot)) %>% 
        select(!REMPER)
      aPlt <- rFIA:::sumToPlot(a, pops, aGrpBy)
      
      ## Adding YEAR to groups
      grpBy <- c('YEAR', grpBy)
      aGrpBy <- c('YEAR', aGrpBy)
      
      
      ## Sum variable(s) up to strata then estimation unit level
      eu.sums <- rFIA:::sumToEU(db, tPlt, aPlt, pops, grpBy, aGrpBy, method)
      tEst <- eu.sums$x
      aEst <- eu.sums$y
      
      ## Have to repeat this with tree totals as the denominator
      eu.sums <- rFIA:::sumToEU(db, 
                                dplyr::select(tPlt, -c(tPlot, pPlot)), 
                                dplyr::select(tPlt, -c(rPlot, mPlot, hPlot, gPlot, cPlot, tPlot)), 
                                pops, grpBy, grpBy, method)
      
      ttEst <- eu.sums$x %>%
        ungroup() %>% 
        dplyr::select(ESTN_UNIT_CN, 
                      all_of(grpBy),
                      rPlot_cv_t = rPlot_cv, 
                      mPlot_cv_t = mPlot_cv, 
                      hPlot_cv_t = hPlot_cv,
                      gPlot_cv_t = gPlot_cv, 
                      cPlot_cv_t = cPlot_cv)
      
      tEst <- dplyr::left_join(tEst, ttEst, by = c('ESTN_UNIT_CN', grpBy))
      
      out <- list(tEst = tEst, aEst = aEst, grpBy = grpBy, aGrpBy = aGrpBy)
    }
  }
  
  
  return(out)
  
}


## GROWMORT final function

growMort_dlp.metric <- function (db, grpBy = NULL, polys = NULL, returnSpatial = FALSE, 
                                 bySpecies = FALSE, bySizeClass = FALSE, landType = "forest", 
                                 treeType = "all", method = "TI", lambda = 0.5, 
                                 stateVar = "TPA", treeDomain = NULL, areaDomain = NULL, 
                                 totals = FALSE, byPlot = FALSE, treeList = FALSE, 
                                 nCores = 1, sizeThresh, evals) 
{
  grpBy_quo <- rlang::enquo(grpBy)
  areaDomain <- rlang::enquo(areaDomain)
  treeDomain <- rlang::enquo(treeDomain)
  remote <- ifelse(class(db) == "Remote.FIA.Database", 
                   1, 0)
  #iter <- rFIA:::remoteIter(db, remote)
  iter <- 1
  mr <- rFIA:::checkMR(db, remote)
  polys <- rFIA:::arealSumPrep1(polys)
  out <- lapply(X = iter, FUN = growMortStarter_dlp.metric, db, grpBy_quo, 
                polys, returnSpatial, bySpecies, bySizeClass, landType, 
                treeType, method, lambda, stateVar, treeDomain, areaDomain, 
                totals, byPlot, treeList, nCores, remote, mr, sizeThresh, evals)
  out <- unlist(out, recursive = FALSE)
  
  if (remote) { out <- dropStatesOutsidePolys(out) }
  
  aEst <- dplyr::bind_rows(out[names(out) == "aEst"])
  tEst <- dplyr::bind_rows(out[names(out) == "tEst"])
  grpBy <- out[names(out) == "grpBy"][[1]]
  aGrpBy <- out[names(out) == "aGrpBy"][[1]]
  grpSyms <- dplyr::syms(grpBy)
  aGrpSyms <- dplyr::syms(aGrpBy)
  if (!byPlot & !treeList) {
    if (mr) {
      tEst <- rFIA:::combineMR(tEst, grpBy)
      aEst <- rFIA:::combineMR(aEst, aGrpBy)
    }
    aEst <- aEst %>% 
      dplyr::group_by(!!!aGrpSyms) %>% 
      dplyr::summarize(dplyr::across(dplyr::everything(),  sum, na.rm = TRUE)) %>% 
      dplyr::select(!!!aGrpSyms, fa_mean, fa_var, nPlots.y)
    
    tEst <- tEst %>% dplyr::group_by(!!!grpSyms) %>% 
      dplyr::summarize(dplyr::across(dplyr::everything(), sum, na.rm = TRUE)) %>% 
      dplyr::left_join(aEst, by = aGrpBy) %>% 
      dplyr::mutate(CURR_TOTAL = tPlot_mean, 
                    PREV_TOTAL = pPlot_mean, 
                    RECR_TOTAL = rPlot_mean, 
                    MORT_TOTAL = mPlot_mean, 
                    REMV_TOTAL = hPlot_mean, 
                    GROW_TOTAL = gPlot_mean, 
                    CHNG_TOTAL = cPlot_mean, 
                    
                    AREA_TOTAL = fa_mean, 
                    AREA_TOTAL_ha = fa_mean*0.4047, # to hectare
                    
                    RECR_TPH = rPlot_mean/(AREA_TOTAL_ha),#fa_mean), 
                    MORT_TPH = mPlot_mean/(AREA_TOTAL_ha),#fa_mean), 
                    REMV_TPH = hPlot_mean/(AREA_TOTAL_ha),#fa_mean), 
                    GROW_TPH = gPlot_mean/(AREA_TOTAL_ha),#fa_mean), 
                    CHNG_TPH = cPlot_mean/(AREA_TOTAL_ha),#fa_mean),
                    CURR_TPH = tPlot_mean/(AREA_TOTAL_ha),#fa_mean), #here
                    PREV_TPH = pPlot_mean/(AREA_TOTAL_ha),#fa_mean), #here
                    
                    RECR_PERC = rPlot_mean/pPlot_mean, 
                    MORT_PERC = mPlot_mean/pPlot_mean, 
                    REMV_PERC = hPlot_mean/pPlot_mean, 
                    GROW_PERC = gPlot_mean/pPlot_mean, 
                    CHNG_PERC = cPlot_mean/pPlot_mean, 
                    
                    CURR_TOTAL_VAR = tPlot_var, 
                    PREV_TOTAL_VAR = pPlot_var, 
                    RECR_TOTAL_VAR = rPlot_var, 
                    MORT_TOTAL_VAR = mPlot_var, 
                    REMV_TOTAL_VAR = hPlot_var, 
                    GROW_TOTAL_VAR = gPlot_var, 
                    CHNG_TOTAL_VAR = cPlot_var, 
                    AREA_TOTAL_VAR = fa_var, 
                    AREA_TOTAL_ha_VAR = fa_var * 0.4047^2, # var to hectare
                    
                    #not messing with the ratio variances... will just leave that all in acres for TPA estimates
                    # RECR_TPA_VAR = rFIA:::ratioVar(rPlot_mean, fa_mean, rPlot_var, fa_var, rPlot_cv), 
                    # MORT_TPA_VAR = rFIA:::ratioVar(mPlot_mean, fa_mean, mPlot_var, fa_var, mPlot_cv), 
                    # REMV_TPA_VAR = rFIA:::ratioVar(hPlot_mean, fa_mean, hPlot_var, fa_var, hPlot_cv), 
                    # GROW_TPA_VAR = rFIA:::ratioVar(gPlot_mean, fa_mean, gPlot_var, fa_var, gPlot_cv), 
                    # CHNG_TPA_VAR = rFIA:::ratioVar(cPlot_mean,fa_mean, cPlot_var, fa_var, cPlot_cv), 
                    # CURR_TPA_VAR = rFIA:::ratioVar(tPlot_mean, fa_mean, tPlot_var, fa_var, cPlot_cv), #here
                    
                    #not messing with the ratio variances... will just leave that all in acres for TPA estimates
                    RECR_TPH_VAR = rFIA:::ratioVar(rPlot_mean, AREA_TOTAL_ha, 
                                                   rPlot_var, AREA_TOTAL_ha_VAR, rPlot_cv), 
                    MORT_TPH_VAR = rFIA:::ratioVar(mPlot_mean, AREA_TOTAL_ha, 
                                                   mPlot_var, AREA_TOTAL_ha_VAR, mPlot_cv), 
                    REMV_TPH_VAR = rFIA:::ratioVar(hPlot_mean, AREA_TOTAL_ha, 
                                                   hPlot_var, AREA_TOTAL_ha_VAR, hPlot_cv), 
                    GROW_TPH_VAR = rFIA:::ratioVar(gPlot_mean, AREA_TOTAL_ha, 
                                                   gPlot_var, AREA_TOTAL_ha_VAR, gPlot_cv), 
                    CHNG_TPH_VAR = rFIA:::ratioVar(cPlot_mean,AREA_TOTAL_ha, 
                                                   cPlot_var, AREA_TOTAL_ha_VAR, cPlot_cv), 
                    CURR_TPH_VAR = rFIA:::ratioVar(tPlot_mean, AREA_TOTAL_ha, 
                                                   tPlot_var, AREA_TOTAL_ha_VAR, cPlot_cv), #here
                    
                    RECR_PERC_VAR = rFIA:::ratioVar(rPlot_mean, pPlot_mean, rPlot_var, pPlot_var, rPlot_cv_t), 
                    MORT_PERC_VAR = rFIA:::ratioVar(mPlot_mean, pPlot_mean, mPlot_var, pPlot_var, mPlot_cv_t), 
                    REMV_PERC_VAR = rFIA:::ratioVar(hPlot_mean, pPlot_mean, hPlot_var, pPlot_var, hPlot_cv_t), 
                    GROW_PERC_VAR = rFIA:::ratioVar(gPlot_mean, pPlot_mean, gPlot_var, pPlot_var, gPlot_cv_t),
                    CHNG_PERC_VAR = rFIA:::ratioVar(cPlot_mean, pPlot_mean, cPlot_var, pPlot_var, cPlot_cv_t), 
                    
                    RECR_PERC = RECR_PERC * 100,
                    MORT_PERC = MORT_PERC * 100,
                    REMV_PERC = REMV_PERC * 100,
                    GROW_PERC = GROW_PERC * 100,
                    CHNG_PERC = CHNG_PERC * 100,
                    RECR_PERC_VAR = RECR_PERC_VAR * 100^2,
                    MORT_PERC_VAR = MORT_PERC_VAR * 100^2,
                    REMV_PERC_VAR = REMV_PERC_VAR * 100^2,
                    GROW_PERC_VAR = GROW_PERC_VAR * 100^2,
                    CHNG_PERC_VAR = CHNG_PERC_VAR * 100^2,
                    
                    CURR_TOTAL_SE = sqrt(tPlot_var),
                    PREV_TOTAL_SE = sqrt(pPlot_var),
                    RECR_TOTAL_SE = sqrt(rPlot_var),
                    MORT_TOTAL_SE = sqrt(mPlot_var),
                    REMV_TOTAL_SE = sqrt(hPlot_var),
                    GROW_TOTAL_SE = sqrt(gPlot_var),
                    CHNG_TOTAL_SE = sqrt(cPlot_var),
                    AREA_TOTAL_SE = sqrt(fa_var),
                    AREA_TOTAL_ha_SE = sqrt(fa_var * 0.4047^2),
                    
                    RECR_TPH_SE = sqrt(RECR_TPH_VAR),
                    MORT_TPH_SE = sqrt(MORT_TPH_VAR),
                    REMV_TPH_SE = sqrt(REMV_TPH_VAR),
                    CHNG_TPH_SE = sqrt(CHNG_TPH_VAR),
                    CURR_TPH_SE = sqrt(CURR_TPH_VAR), 
                    RECR_PERC_SE = sqrt(RECR_PERC_VAR),
                    MORT_PERC_SE = sqrt(MORT_PERC_VAR),
                    REMV_PERC_SE = sqrt(REMV_PERC_VAR),
                    GROW_PERC_SE = sqrt(GROW_PERC_VAR),
                    CHNG_PERC_SE = sqrt(CHNG_PERC_VAR),
                    
                    CURR_TOTAL_CV = sqrt(tPlot_var)/tPlot_mean * 100,
                    PREV_TOTAL_CV = sqrt(pPlot_var)/pPlot_mean * 100,
                    RECR_TOTAL_CV = sqrt(rPlot_var)/rPlot_mean * 100,
                    MORT_TOTAL_CV = sqrt(mPlot_var)/mPlot_mean * 100,
                    REMV_TOTAL_CV = sqrt(hPlot_var)/rPlot_mean * 100,
                    GROW_TOTAL_CV = sqrt(gPlot_var)/gPlot_mean * 100,
                    CHNG_TOTAL_CV = sqrt(cPlot_var)/cPlot_mean * 100,
                    AREA_TOTAL_CV = sqrt(fa_var)/fa_mean * 100,
                    AREA_TOTAL_ha_CV = sqrt(fa_var * 0.4047^2)/(AREA_TOTAL_ha) * 100,
                    RECR_TPH_CV = sqrt(RECR_TPH_VAR)/RECR_TPH * 100,
                    MORT_TPH_CV = sqrt(MORT_TPH_VAR)/MORT_TPH * 100,
                    REMV_TPH_CV = sqrt(REMV_TPH_VAR)/REMV_TPH * 100,
                    CHNG_TPH_CV = sqrt(CHNG_TPH_VAR)/CHNG_TPH * 100,
                    CURR_TPH_CV = sqrt(CURR_TPH_VAR)/CURR_TPH * 100, # here
                    RECR_PERC_CV = sqrt(RECR_PERC_VAR)/RECR_PERC * 100,
                    MORT_PERC_CV = sqrt(MORT_PERC_VAR)/MORT_PERC * 100,
                    REMV_PERC_CV = sqrt(REMV_PERC_VAR)/REMV_PERC * 100,
                    GROW_PERC_CV = sqrt(GROW_PERC_VAR)/GROW_PERC * 100,
                    CHNG_PERC_CV = sqrt(CHNG_PERC_VAR)/CHNG_PERC * 100,
                    
                    nPlots_TREE = nPlots.x, 
                    nPlots_AREA = nPlots.y, 
                    N = P2PNTCNT_EU) %>% 
      dplyr::select(!!!grpSyms,
                    RECR_TPH:CHNG_PERC, 
                    RECR_TOTAL:CHNG_TOTAL, 
                    PREV_TOTAL,
                    CURR_TOTAL,
                    CURR_TPH,
                    CURR_TPH_VAR, CURR_TPH_CV, CURR_TPH_SE,
                    AREA_TOTAL_ac = AREA_TOTAL, 
                    AREA_TOTAL_ha,
                    RECR_TPH_VAR:CHNG_PERC_VAR, RECR_TOTAL_VAR:CHNG_TOTAL_VAR, PREV_TOTAL_VAR, CURR_TOTAL_VAR, AREA_TOTAL_VAR, AREA_TOTAL_ha_VAR,
                    RECR_TPH_CV:CHNG_PERC_CV, RECR_TOTAL_CV:CHNG_TOTAL_CV, PREV_TOTAL_CV, CURR_TOTAL_CV, AREA_TOTAL_CV, AREA_TOTAL_ha_CV,
                    RECR_TPH_SE:CHNG_PERC_SE, RECR_TOTAL_SE:CHNG_TOTAL_SE, PREV_TOTAL_SE, CURR_TOTAL_SE, AREA_TOTAL_SE, AREA_TOTAL_ha_SE,
                    nPlots_TREE,nPlots_AREA, N) %>%
      
      dplyr::mutate(dplyr::across(c(GROW_TPH, GROW_PERC, GROW_TOTAL, GROW_TPH_VAR, GROW_PERC_VAR,GROW_TOTAL_VAR),
                                  .fns = ~case_when(abs(.x) < 0.00001 ~ 0, TRUE ~ .x)))
    if (!totals) {
      tEst <- tEst[, !stringr::str_detect(names(tEst), 
                                          "_TOTAL")]
    }
    
    # commented out because I want to return both VAR and SE
    # default behavior is to return variance (VAR), standard error (SE), and coefficient of variation (CV)
    
    # if (variance) {
    #   tEst <- tEst[, !stringr::str_detect(names(tEst), 
    #                                       "_SE")]
    # } else {
    #   tEst <- tEst[, !stringr::str_detect(names(tEst), 
    #                                       "_VAR")]
    # }
  }
  
  if (stateVar != "TPA") {
    names(tEst) <- str_replace(names(tEst), "TPH", 
                               paste(stateVar, "ACRE", sep = "_"))
    names(tEst) <- str_replace(names(tEst), "TPA", 
                               paste(stateVar, "ACRE", sep = "_"))
  }
  
  names(tEst) <- str_replace(names(tEst), "BAA_ACRE", 
                             "BAH")
  
  tEst <- tEst %>% dplyr::ungroup() %>% dplyr::mutate_if(is.factor, 
                                                         as.character) %>% as_tibble()
  if (!treeList) {
    tEst <- tEst %>% tidyr::drop_na(grpBy[!c(grpBy %in% names(polys))]) %>% 
      dplyr::arrange(YEAR)
  }
  if (returnSpatial & byPlot) 
    grpBy <- grpBy[grpBy %in% c("LAT", "LON") == 
                     FALSE]
  if (returnSpatial & !byPlot) {
    tEst <- dplyr::left_join(tEst, as.data.frame(dplyr::select(polys, 
                                                               polyID, geometry)), by = "polyID")
  }
  if (returnSpatial) 
    tEst <- sf::st_sf(tEst)
  return(tEst)
}

