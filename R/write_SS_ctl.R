#'@description Function to write SS control file changes in line with Atlantis operating model. The function modifies the key biological
#'and selectivity parameter initial values and bounds. These parameters include: LN(R0), h, M, L1, L2, k, CV_young, CV_old
#'TO DO: add in logic to modify which parameters are fixed and which are estimated

write_SS_ctl <- function(wt_len_obj, biolprm_obj, ...){



  meanwt_spp <- wt_len_obj$muweight %>%
    filter(time>burnin) %>%
    group_by(agecl) %>%
    summarize(meanwt = mean(atoutput))

  meanln_spp <- wt_len_obj$mulen %>%
    filter(time>burnin) %>%
    group_by(agecl) %>%
    summarize(meanln = mean(atoutput), cvln=sd(atoutput)/mean(atoutput))


  sardine.ctl <-r4ss::SS_readctl_3.30(paste0("./inst/extdata/",
                                             model_dir,
                                             ctlfile_name))
  sardine.ctl <- SS_write_biol("SAR", wtsage=meanwt_spp, lensage = meanln_spp)

  li_a_use <- biolprm$wl[match(fgs$Code[match(species,fgs$Name)],biolprm$wl[, 1]), 2]/1000
  li_b_use <- biolprm$wl[match(fgs$Code[match(species,fgs$Name)],biolprm$wl[, 1]), 3]


}
