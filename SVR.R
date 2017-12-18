#GETRATIOS <- function(s,YEAR){

n <- length(s)
pb <- txtProgressBar(min = 0, max = n, style=3)
EPS <- matrix(data = NA,nrow = n, ncol = 1)
BVPS <- matrix(data = NA,nrow = n, ncol = 1)
OPM <- matrix(data = NA,nrow = n, ncol = 1)
DE <- matrix(data = NA,nrow = n, ncol = 1)
CR <- matrix(data = NA,nrow = n, ncol = 1)
QE <- matrix(data = NA,nrow = n, ncol = 1)
IT <- matrix(data = NA,nrow = n, ncol = 1)
RT <- matrix(data = NA,nrow = n, ncol = 1)
OIG <- matrix(data = NA,nrow = n, ncol = 1)
NIG <- matrix(data = NA,nrow = n, ncol = 1)
EPSG <- matrix(data = NA,nrow = n, ncol = 1)
EPSGtw <- matrix(data = NA,nrow = n, ncol = 1)
FCF <- matrix(data = NA,nrow = n, ncol = 1)
POR <- matrix(data = NA,nrow = n, ncol = 1)
BETA <- matrix(data = NA,nrow = n, ncol = 1)
MOM <- matrix(data = NA,nrow = n, ncol = 1)

goback <-c(as.numeric(YEAR)-3)

PRICEHISTIND <- tq_get("^GSPC", get = "stock.prices", from = as.Date(paste0(goback,"01","01"),format = "%Y%m%d") ,to = as.Date(paste0(YEAR,"12","31"),format = "%Y%m%d"))

for(i in 480:n){
  
  symbol <- s[i]
  
  INTRATIOS <- (tq_get(symbol, get="key.ratios"))
  
  FR <- INTRATIOS$data[[1]]
  PR <- INTRATIOS$data[[2]]
  GR <- INTRATIOS$data[[3]]
  CFR <- INTRATIOS$data[[4]]
  FHR <- INTRATIOS$data[[5]]
  EFR <- INTRATIOS$data[[6]]
  VR <- INTRATIOS$data[[7]]
  
  
  
  D <- FR$date[1]
  d <- day(as.POSIXlt(D,format = "%Y/%m/%d"))
  m <- month(as.POSIXlt(D,format = "%Y/%m/%d"))
  

  if(d <= 9 & m <= 9){
    
    STARTFROM <- YEAR
    fix <- "0"
    lookingfor <- as.Date(paste0(STARTFROM,fix,m,fix,d), format = "%Y%m%d")
    
  }
  else{
    
    STARTFROM <- YEAR
    lookingfor <- as.Date(paste0(STARTFROM,m,d), format = "%Y%m%d")
    
  }
  
  
  FRthatyear <- FR[which(FR$date == lookingfor, arr.ind = TRUE),]
  PRthatyear <- PR[which(PR$date == lookingfor, arr.ind = TRUE),]
  GRthatyear <- GR[which(GR$date == lookingfor, arr.ind = TRUE),]
  CFRthatyear <- CFR[which(CFR$date == lookingfor, arr.ind = TRUE),]
  FHRthatyear <- FHR[which(FHR$date == lookingfor, arr.ind = TRUE),]
  EFRthatyear <- EFR[which(EFR$date == lookingfor, arr.ind = TRUE),]
  VRthatyear <- VR[which(VR$date == lookingfor, arr.ind = TRUE),]
  
  #Profitability ratios 
  
  if(length(FRthatyear$value[which(FRthatyear$category == "Earnings Per Share USD", arr.ind = TRUE)]) == 0){
    
    EPS[i,1] <- NA
  }
  else{
    EPS[i,1] <- FRthatyear$value[which(FRthatyear$category == "Earnings Per Share USD", arr.ind = TRUE)]
    
  }
  
  if(length(FRthatyear$value[which(FRthatyear$category == "Book Value Per Share * USD", arr.ind = TRUE)]) == 0){
    
    BVPS[i,1] <- NA
  }
  else{
    
    BVPS[i,1] <- FRthatyear$value[which(FRthatyear$category == "Book Value Per Share * USD", arr.ind = TRUE)]
    
  }
  
  
  #OPM[[i]] <- FRthatyear$value[which(FRthatyear$category == "Operating margin %", arr.ind = TRUE)]
  
  #Leverage ratio
  if(length(FHRthatyear$value[which(FHRthatyear$category == "Debt/Equity", arr.ind = TRUE)]) == 0){
    
    
  }
  else{
    
    DE[i,1] <- FHRthatyear$value[which(FHRthatyear$category == "Debt/Equity", arr.ind = TRUE)]
    
  }
  
    
  #Liquidity ratios
  if(length(FHRthatyear$value[which(FHRthatyear$category == "Current Ratio", arr.ind = TRUE)]) == 0){
    
    CR[i,1] <- NA
    
  }
    else{
      
      CR[i,1] <- FHRthatyear$value[which(FHRthatyear$category == "Current Ratio", arr.ind = TRUE)]
      
    }
  
  if(length(FHRthatyear$value[which(FHRthatyear$category == "Free Cash Flow USD Mil ", arr.ind = TRUE)]) == 0){
    
    FCF[i,1] <- NA
    
  }
  else{
    
    FCF[i,1] <- FHRthatyear$value[which(FHRthatyear$category == "Free Cash Flow USD Mil", arr.ind = TRUE)]
    
  }
  
  if(length(FHRthatyear$value[which(FHRthatyear$category == "Payout Ratio % *", arr.ind = TRUE)]) == 0){
    
    POR[i,1] <- NA
    
  }
  else{
    
    POR[i,1] <- FHRthatyear$value[which(FHRthatyear$category == "Payout Ratio % *", arr.ind = TRUE)]
    
  }
  
  
  if(length(FHRthatyear$value[which(FHRthatyear$category == "Quick Ratio", arr.ind = TRUE)]) == 0){
    
    QE[i,1] <- NA
    
  }
  else{
    
    QE[i,1] <- FHRthatyear$value[which(FHRthatyear$category == "Quick Ratio", arr.ind = TRUE)] 
    
  }
  
   
  
  #Efficiency ratios 
  if(length(EFRthatyear$value[which(EFRthatyear$category == "Inventory Turnover", arr.ind = TRUE)]) == 0){
    
    IT[i,1] <- NA
    
  }
    else{
      
      IT[i,1] <-   EFRthatyear$value[which(EFRthatyear$category == "Inventory Turnover", arr.ind = TRUE)]
      
    }
  if(length(EFRthatyear$value[which(EFRthatyear$category == "Receivables Turnover", arr.ind = TRUE)]) == 0){
    
    RT[i,1] <- NA
    
  }
  else{
    
    RT[i,1] <- EFRthatyear$value[which(EFRthatyear$category == "Receivables Turnover", arr.ind = TRUE)]
    
  }
  
 
    
  # Growth ratios
  
  # Operating income
  if(length((GRthatyear$value[which(GRthatyear$sub.section == "Operating Income %", arr.ind = TRUE)])[1]) == 0){
    
    OIG[i,1] <- NA
    
  }
  else{
    
    OIG[i,1] <- (GRthatyear$value[which(GRthatyear$sub.section == "Operating Income %", arr.ind = TRUE)])[1]
    
  }
  # Net income
  if(length((GRthatyear$value[which(GRthatyear$sub.section == "Operating Income %", arr.ind = TRUE)])[1]) == 0){
    
    NIG[i,1] <- NA
    
  }
  else{
    
    NIG[i,1] <- (GRthatyear$value[which(GRthatyear$sub.section == "Operating Income %", arr.ind = TRUE)])[1] 
    
  }
  
  
  
  if(length((GRthatyear$value[which(GRthatyear$sub.section == "EPS %", arr.ind = TRUE)])[1]) == 0){
    
    EPSG[i,1] <- NA
    
  }
  else{
    
    EPSG[i,1] <- (GRthatyear$value[which(GRthatyear$sub.section == "EPS %", arr.ind = TRUE)])[1] 
    
  } 
  
  if(length((GRthatyear$value[which(GRthatyear$sub.section == "EPS %", arr.ind = TRUE)])[2]) == 0){
    
    EPSGtw[i,1] <- NA
    
  }
  else{
    
    EPSGtw[i,1] <- (GRthatyear$value[which(GRthatyear$sub.section == "EPS %", arr.ind = TRUE)])[2] 
    
  } 
  
  if(length((GRthatyear$value[which(GRthatyear$sub.section == "EPS %", arr.ind = TRUE)])[1]) == 0){
    
    EPSG[i,1] <- NA
    
  }
  else{
    
    EPSG[i,1] <- (GRthatyear$value[which(GRthatyear$sub.section == "EPS %", arr.ind = TRUE)])[1] 
    
  } 
  
  if(length((GRthatyear$value[which(GRthatyear$sub.section == "EPS %", arr.ind = TRUE)])[2]) == 0){
    
    EPSGtw[i,1] <- NA
    
  }
  else{
    
    EPSGtw[i,1] <- (GRthatyear$value[which(GRthatyear$sub.section == "EPS %", arr.ind = TRUE)])[2] 
    
  } 
  
  goback <-c(as.numeric(YEAR)-3)
  
  PRICEHIST <- tq_get(s[i], get = "stock.prices",from = as.Date(paste0(goback,"01","01"),format = "%Y%m%d"), to = as.Date(paste0(YEAR,"12","31"),format = "%Y%m%d"))
  
  if(length(PRICEHIST) == 1 || nrow(PRICEHIST) < nrow(PRICEHISTIND)){
    
    BETA[i,1] <- NA
    MOM[i,1] <- NA
    
  }
  else{
    
    mrtest <- tq_transmute(PRICEHIST, adjusted, periodReturn, period = "monthly")
    
    
    
    mrind <- tq_transmute(PRICEHISTIND, adjusted, periodReturn, period = "monthly")
    
    fit <- (lm(mrtest$monthly.returns ~ mrind$monthly.returns))
    
    BETA[i,1] <- (fit$coefficients[2])
    
    nst <- nrow(PRICEHIST)
    prev <- nst - 200
    
    #200day price momentum
    
    MOM[i,1] <- (PRICEHIST[nst,7]/PRICEHIST[prev,7])$adjusted
    
    setTxtProgressBar(pb, i) 
  }
    
  
  }

  Ratios <- cbind(EPS,BVPS,DE,CR,QE,IT,RT,OIG,NIG,EPSG,EPSGtw,FCF,POR,BETA,MOM)
  colnames(Ratios) <- c("EPS","BVPS","DE","CR","QE","IT","RT", "OIG", "NIG","EPSG","EPSGtw","FCF","POR","BETA","MOM")
  rownames(Ratios) <- t(s)
  
#  return(Ratios)
#}





