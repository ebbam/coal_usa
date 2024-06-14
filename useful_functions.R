##############################################################
## Function to identify FIPS code

fips_format <- function(fipscode){
  if (is.na(fipscode)) return("00000")
  fips_c = as.character(fipscode)
  test_chars = nchar(fips_c)
  if (is.null(fipscode) | is.na(fipscode)) {
    return (nchar(trunc(fipscode)))
  }
  else if (test_chars < 4) {
    return (fips_c)
  }
  else if (test_chars < 5) {
    return (paste0(0, fips_c, sep = ""))
  }
  else {
    return (fips_c)
  }
}


create_cols <- function(model){
  pval <- model %>% pvalue() %>% 
    lapply(., function(z) ifelse(z <= 0.001, 4,
                                 ifelse(z > 0.001 & z <= 0.01, 3, 
                                        ifelse(z > 0.01 & z <= 0.05, 2, 
                                               ifelse(z > 0.05 & z <= 0.1, 1,0)))))
  pval <- as.matrix(as.numeric(pval[1:3]))
  iden <- matrix(nrow = 3, ncol = 3, data = 0)
  diag(iden) <- unlist(lapply(coef(model)[1:3], sign))
  return(-1*(t(pval) %*% iden))
}


# AIC and BIC function for splm object #
AICsplm = function(object, k=2, criterion=c("AIC", "BIC")){
  sp = summary(object)
  l = sp$logLik
  np = length(coef(sp))
  N = nrow(sp$model)
  if (sp$effects=="sptpfe") {
    n = length(sp$res.eff[[1]]$res.sfe)
    T = length(sp$res.eff[[1]]$res.tfe)
    np = np+n+T
  }
  if (sp$effects=="spfe") {
    n = length(sp$res.eff[[1]]$res.sfe)
    np = np+n+1
  }
  if (sp$effects=="tpfe") {
    T = length(sp$res.eff[[1]]$res.tfe)
    np = np+T+1
  }
  if(criterion=="AIC"){
    aic = -2*l+k*np
    names(aic) = "AIC"
    return(aic)
  }
  if(criterion=="BIC"){
    bic = -2*l+log(N)*np
    names(bic) = "BIC"
    return(bic)
  }
}


dw_compat <- function(sp){
  spdf <- summary(sp)
  ti_closure <- data.frame(
    term = spdf$CoefTable[1:5,0],
    estimate = spdf$CoefTable[1:5,1],
    std.error = spdf$CoefTable[1:5,2])
  ti_closure$model = "Spatial Error"
  ti_closure$term <- row.names(ti_closure)
  # df = nrow(spdf$model)
  # ti_closure$conf.low = ti_closure$estimate -1*ti_closure$std.error*qt(0.975, df-1)
  # ti_closure$conf.high = ti_closure$estimate +1*ti_closure$std.error*qt(0.975, df-1)
  return(ti_closure)
}

dw_compat_durbin <- function(sp){
  spdf <- summary(sp)
  ti_closure <- data.frame(
    term = spdf$CoefTable[6:8,0],
    estimate = spdf$CoefTable[6:8,1],
    std.error = spdf$CoefTable[6:8,2])
  ti_closure$model = "Durbin Spatial Error"
  ti_closure$term <- row.names(ti_closure)
  # df = nrow(spdf$model)
  # ti_closure$conf.low = ti_closure$estimate -1*ti_closure$std.error*qt(0.975, df-1)
  # ti_closure$conf.high = ti_closure$estimate +1*ti_closure$std.error*qt(0.975, df-1)
  return(ti_closure)
}

dw_compat_imp <- function(imp){
  impdf <- summary(imp, zstats=TRUE, short=T)
  ti_closure <- data.frame(
    term = c("mines_diff", "lag_diff","lag_diff2","diff_log_realgdp_pc"),
    estimate = impdf$res$direct[1:4],
    std.error = impdf$semat[,1])
  #ti_closure$term <- row.names(ti_closure)
  #ti_closure$conf.low = ti_closure$estimate -1*ti_closure$std.error*qt(0.975, 200)
  #ti_closure$conf.high = ti_closure$estimate +1*ti_closure$std.error*qt(0.975, 200)
  return(ti_closure)
}

dw_compat_direct <- function(imp){
  impdf <- summary(imp, zstats=TRUE, short=T)
  ti_closure <- data.frame(
    term = c("mines_diff", "lag_diff","lag_diff2","diff_log_realgdp_pc"),
    estimate = impdf$res$direct,
    std.error = impdf$semat[,1])
  #ti_closure$term <- row.names(ti_closure)
  #ti_closure$conf.low = ti_closure$estimate -1*ti_closure$std.error*qt(0.975, 200)
  #ti_closure$conf.high = ti_closure$estimate +1*ti_closure$std.error*qt(0.975, 200)
  return(ti_closure)
}

dw_compat_indirect <- function(imp){
  impdf <- summary(imp, zstats=TRUE, short=T)
  ti_closure <- data.frame(
    term = c("mines_diff", "lag_diff","lag_diff2","diff_log_realgdp_pc"),
    estimate = impdf$res$indirect,
    std.error = impdf$semat[,2])
  #ti_closure$term <- row.names(ti_closure)
  #ti_closure$conf.low = ti_closure$estimate -1*ti_closure$std.error*qt(0.975, 200)
  #ti_closure$conf.high = ti_closure$estimate +1*ti_closure$std.error*qt(0.975, 200)
  return(ti_closure)
}

dw_compat_total <- function(imp){
  impdf <- summary(imp, zstats=TRUE, short=T)
  ti_closure <- data.frame(
    term = c("mines_diff", "lag_diff","lag_diff2","diff_log_realgdp_pc"),
    estimate = impdf$res$total,
    std.error = impdf$semat[,3])
  #ti_closure$term <- row.names(ti_closure)
  #ti_closure$conf.low = ti_closure$estimate -1*ti_closure$std.error*qt(0.975, 200)
  #ti_closure$conf.high = ti_closure$estimate +1*ti_closure$std.error*qt(0.975, 200)
  return(ti_closure)
}



# Spatial model summary results
spat_output <- function(spat_model) {
  sums <- summary(spat_model, zstats=TRUE, short=TRUE)
  sums$pzmat <- ifelse(sums$pzmat <= 0.001, "***",
                       ifelse(sums$pzmat > 0.001 & sums$pzmat <= 0.01, "**", 
                              ifelse(sums$pzmat > 0.01 & sums$pzmat <= 0.05, "*", 
                                     ifelse(sums$pzmat > 0.05 & sums$pzmat <= 0.1, ".",""))))
  sums$res <- sums$res %>% as.data.frame %>% mutate_if(is.numeric, round, 3)
  sums$semat <- sums$semat %>% as.data.frame %>% mutate_if(is.numeric, round, 3)
  sumstable <- data.frame(matrix(ncol = 3, nrow = 5))
  for(l in 1:ncol(sums$pzmat)){
    c = paste0(as.data.frame(sums$res)[,l],sums$pzmat[,l])
    if(length(c) == 5){
      sumstable[,l] <- as.data.frame(c)
    }else{
      c = c(c,NA)
      sumstable[,l] <- as.data.frame(c)
    }
  }

  row.names(sumstable) <- c("mines_diff", "lag_diff", "lag_diff2", "diff_log_realgdp","diff_log_pop")
  names(sumstable) <- c("Direct", "Indirect", "Total")
  sumstable$Value_type <- "Estimate"
  sumstable <- rbind(sumstable, cbind(sums$semat, Value_type = "SE"))
  print(sumstable)
  orderst <- sumstable[match(c("mines_diff", "mines_diff1", "lag_diff", "lag_diff1", "lag_diff2", "lag_diff21", "diff_log_realgdp", "diff_log_realgdp1","diff_log_pop","diff_log_pop1"), rownames(sumstable)),]
  
  return(orderst)
}

spat_output_rob <- function(spat_model) {
  sums <- summary(spat_model, zstats=TRUE, short=TRUE)
  sums$pzmat <- ifelse(sums$pzmat <= 0.001, "***",
                       ifelse(sums$pzmat > 0.001 & sums$pzmat <= 0.01, "**", 
                              ifelse(sums$pzmat > 0.01 & sums$pzmat <= 0.05, "*", 
                                     ifelse(sums$pzmat > 0.05 & sums$pzmat <= 0.1, ".",""))))
  sums$res <- sums$res %>% as.data.frame %>% mutate_if(is.numeric, round, 3)
  sums$semat <- sums$semat %>% as.data.frame %>% mutate_if(is.numeric, round, 3)
  sumstable <- data.frame(matrix(ncol = 3, nrow = 4))
  for(l in 1:ncol(sums$pzmat)){
    c = paste0(as.data.frame(sums$res)[,l],sums$pzmat[,l])
    if(length(c) == 4){
      sumstable[,l] <- as.data.frame(c)
    }else{
      c = c(c,NA)
      sumstable[,l] <- as.data.frame(c)
    }
  }
  names(sumstable) <- c("Direct", "Indirect", "Total")
  sumstable$Value_type <- "Estimate"
  sumstable <- rbind(sumstable, cbind(sums$semat, Value_type = "se"))
  varnames <- sums$pzmat %>% rownames(.)
  sumstable$var <- c(varnames, varnames)
  orderst <- sumstable %>% arrange(factor(var, levels = varnames), Value_type) %>% relocate(var) %>% rename(measure = Value_type)
  
  return(orderst)
}



df_va <- function(df){
  fips_pre <- as.list(unique(df$fips))
  for(a in 1:length(fips_pre)){
    f = fips_pre[a]
    if(f %in% getfips){
      newfips = names(getfips[which(getfips == f)])
      if(length(newfips) != 0){
        for(b in 1:length(newfips)){
          copy <- subset(df, fips == f)
          copy$fips <- newfips[b]
          df <- rbind(df, copy)
        }
      }else{
      }
      df <- subset(df, fips != f)
    }
  }
  return(df)
}

dw_compat_kss <- function(kss){
  kssdf <- summary(kss)
  ksscompat <- data.frame(
    term = dimnames(kssdf$coefficients)[[1]][1:3],
    estimate = kssdf$coefficients[1:3,1],
    std.error = kssdf$coefficients[1:3,2])
  ksscompat$model = paste("HTT Model w/",kssdf$KSS.obj$used.dim,"factors")
  return(ksscompat)
}

tablefun <- function(obj) {
  briefdf <- as.data.frame(cbind(obj$coefficients))
  names(briefdf)[4] <- "p_values"
  newdf <- briefdf %>%  mutate_at("p_values", function(y) ifelse(y <= 0.001, "***",
                                                                 ifelse(y > 0.001 & y <= 0.01, "**",
                                                                        ifelse(y > 0.01 & y <= 0.05, "*",
                                                                               ifelse(y > 0.05 & y <= 0.1, ".",""))))) %>%
    mutate_if(is.numeric, round, 3) %>%
    mutate(Estimate = paste0(Estimate, p_values), StdErr = paste0("(",StdErr,")"), var = rownames(.)) %>%
    select("Estimate","StdErr", "var") %>%
    pivot_longer(!var, names_to = "measure", values_to = "estimate")
  return(newdf)
}

# Initialise function for specific size of matrix
c_mat <- function(var) {matrix(var, 18, 3072)}