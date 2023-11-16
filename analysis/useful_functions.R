################################################################################
# Useful functions for working with motus data
# 7-Mar-2022
# adapted from Ariel Lenske
#
################################################################################

# function for mapping tower detections - simplifies so not too much data to plot####
fun.getpath <- function(df) 
{
  df %>%
    filter(!is.na(recvDeployLat) | !(recvDeployLat == 0)) %>% # drops data without lon/lat
    group_by(motusTagID, runID, recvName, ambigID, 
             tagDepLon, tagDepLat, recvDeployLat, recvDeployLon, tagDeployStart, motusFilter) %>%
    #summarizing by runID to get max run length and mean time stamp:
    summarize(max.runLen = max(runLen), ts.h = mean(ts)) %>% 
    mutate(year = as.numeric(format(ts.h,'%Y')),
           month = as.numeric(format(ts.h,'%m'))) %>%
    arrange(motusTagID, ts.h) %>%
    ungroup()
} # end of function call

# function for plotting signal strength against time by recv
plotTagSig_mod <- function (df, motusTagID) 
{
  tag.id <- motusTagID
  df <- filter(df, motusTagID == tag.id)
  df <- select(df, motusTagID, sig, noise, ts, port, recvDeployLat, 
               recvDeployLon, recvName, runLen, motusFilter)
  df <- mutate(df, 
               recvName = paste(recvName, 
                                round(recvDeployLat, digits = 1), sep = "\n"), 
               recvName = paste(recvName, 
                                round(recvDeployLon, digits = 1), sep = ", "), 
               ts = as_datetime(ts, tz = "UTC")
               # antBearing.ch = if_else(is.na(antBearing),"none", as.character(antBearing))
  )
  df <- within(df, recvName <- reorder(recvName, (recvDeployLat)))
  
  ggplot(df, aes(ts, sig, col = as.factor(port))) +
    geom_point() +
    geom_point(aes(ts, noise), col = "lightgrey") +
    theme_bw() + 
    labs(title = paste("Detection Time vs Signal Strength, coloured by antenna \n motusTagID:", 
                       motusTagID),
         x = "Date", 
         y = "Signal Strength", 
         colour = "antenna") + 
    facet_grid(recvName ~ .) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} # end of function call

# calculate distance between two points
# used to be in motus package?
latLonDist = function(lat1, lon1, lat2, lon2) {
  a = 6378137
  b = 6356752.314245
  f = 1/298.257223563  ## WGS-84 ellipsoid params
  
  llmat = cbind(lat1, lon1, lat2, lon2) ## recycles coordinates to match
  
  s = rep(-1, nrow(llmat)) ## return values; -1 means not yet computed
  for (i in 1:nrow(llmat)) {  ## calculate distance between i'th pair of points
    if (!all(is.finite(llmat[i,]))) {
      s[i] = NA
      next
    }
    
    L = rad(llmat[i, 4]-llmat[i, 2])
    U1 = atan((1-f) * tan(rad(llmat[i, 1])))
    U2 = atan((1-f) * tan(rad(llmat[i, 3])))
    sinU1 = sin(U1)
    cosU1 = cos(U1)
    sinU2 = sin(U2)
    cosU2 = cos(U2)
    lambda = L
    iterLimit = 100
    repeat {
      sinLambda = sin(lambda)
      cosLambda = cos(lambda)
      sinSigma = sqrt((cosU2*sinLambda) * (cosU2*sinLambda) + 
                        (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda))
      if (abs(sinSigma) < 1e-10) {
        s[i] = 0 ## co-incident points
        break
      }
      cosSigma = sinU1*sinU2 + cosU1*cosU2*cosLambda
      sigma = atan2(sinSigma, cosSigma)
      sinAlpha = cosU1 * cosU2 * sinLambda / sinSigma
      cosSqAlpha = 1 - sinAlpha*sinAlpha
      cos2SigmaM = cosSigma - 2*sinU1*sinU2 / cosSqAlpha
      if (is.nan(cos2SigmaM))
        cos2SigmaM = 0  ## equatorial line: cosSqAlpha=0 (§6)
      C = f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
      lambdaP = lambda
      lambda = L + (1-C) * f * sinAlpha *
        (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
      iterLimit = iterLimit - 1
      if (abs(lambda-lambdaP) <= 1e-12 || iterLimit == 0)
        break
    } 
    
    if (iterLimit==0) {
      s[i] = NaN  ## formula failed to converge
    } else if (s[i] < 0) {
      uSq = cosSqAlpha * (a*a - b*b) / (b*b)
      A = 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
      B = uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
      deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)-
                                                 B/6*cos2SigmaM*(-3+4*sinSigma*sinSigma)*(-3+4*cos2SigmaM*cos2SigmaM)))
      s[i] = b*A*(sigma-deltaSigma)
    }
  }
  s = round(s, 3)
  return (s)
}

# radians
rad = function(x) {
  return (x * (pi/180))
}

################################################################################
#'functions used to calculate site transition statistics

################################################################################
#'main function:
################################################################################

#'modifided siteTrans function (from the Motus package) - Summarize transitions between sites for each tag

site_transit_min <- function(data, latCoord = "recvDeployLat", lonCoord = "recvDeployLon"){
  tmp <- data
  data <- rename(tmp, lat = latCoord, lon = lonCoord)
  
  data <- data %>% select(ts, motusTagID, lat, lon, recvName, recvProjID, month, year) %>% #pull out relevant columns
    dplyr::group_by(motusTagID) %>% do(consec.fun(.)) %>% #re-format data step#1
    dplyr::group_by(motusTagID) %>% do(site.fun(.)) %>% #re-format data step#2
    mutate(tot_ts = round(difftime(ts.y, ts.x, units = "secs"))) %>% #time diff between detections in seconds
    mutate(tot_ts = ifelse(tot_ts == 0, 1, tot_ts), #deal with simultaneous detections by making them one sec apart
           dist = round(latLonDist(lat.x, lon.x, lat.y, lon.y)), #meters
           dist.min = dist - 50000) %>% #account for detection ranges of towers
    mutate(dist.min = ifelse(dist.min < 0, 0, dist.min),
           rate = round(dist.min/(as.numeric(tot_ts)), digits = 4)) %>% #rate of travel in m/s
    mutate(suspect.transit = ifelse(rate > 72, "suspect", "fine"))
  
  return(data)
}

site_transit_stopover <- function(data, latCoord = "recvDeployLat", lonCoord = "recvDeployLon"){
  tmp <- data
  data <- rename(tmp, lat = latCoord, lon = lonCoord)
  
  data <- data %>% 
    do(add_deploy_loc_rgroup(.)) %>%
    select(ts, motusTagID, lat, lon, recvName, recvProjID, recvGroup, month, year) %>% #pull out relevent columns
    dplyr::group_by(motusTagID) %>% do(consec.fun(.)) %>% #re-format data step#1
    dplyr::group_by(motusTagID) %>% do(site.fun(.)) %>% #re-format data step#2
    mutate(tot_ts = round(difftime(ts.y, ts.x, units = "secs"))) %>% #time diff between detections in seconds
    mutate(tot_ts = ifelse(tot_ts == 0, 1, tot_ts), #deal with simultaneous detections by making them one sec apart
           dist = round(latLonDist(lat.x, lon.x, lat.y, lon.y))) %>% #meters
    mutate(rate = round(dist/(as.numeric(tot_ts)), digits = 4)) %>% #rate of travel in m/s
    mutate(stopover = ifelse(rate < 5, "yes", "no"))
  
  return(data)
}




###############################################################################
#'functions that are used in main function:
###############################################################################

#'function to add row for tag deployment location/time to use for calculating transitions

add_deploy_loc <- function(df){
  
  df.r1 <- df %>% 
    group_by(motusTagID) %>%
    arrange(ts) %>%
    slice(1) %>%
    mutate(ts = tagDeployStart,
           recvName = "deploy location",
           recvDeployLat = tagDepLat, 
           recvDeployLon = tagDepLon,
           month = as.numeric(format(ts,'%m')),
           year = as.numeric(format(ts,'%Y'))) 
  
  df <- bind_rows(df.r1, df) %>% group_by(motusTagID) %>% arrange(ts) %>% as.data.frame()
  
}

add_deploy_loc_rgroup <- function(df){
  
  df.r1 <- df %>% 
    group_by(motusTagID) %>%
    arrange(ts) %>%
    slice(1) %>%
    mutate(ts = tagDeployStart,
           recvName = "deploy location",
           recvDeployLat = tagDepLat, 
           recvDeployLon = tagDepLon,
           recvGroup = "deploy location",
           month = as.numeric(format(ts,'%m')),
           year = as.numeric(format(ts,'%Y'))) 
  
  df <- bind_rows(df.r1, df) %>% group_by(motusTagID) %>% arrange(ts) %>% as.data.frame()
  
}



################################################################################
#' Create dataframe for siteTrans function
#'
#' @param data dataframe of Motus detection data containing at a minimum fullID, ts, lat, lon
#'
#' @author Zoe Crysler \email{zcrysler@@gmail.com}
#'

## site.fun and consec.fun adapted from "between.locs.R" script written by Phil

consec.fun <- function(df) {
  
  df <- df[order(df$ts),]
  a <- df$recvName[-length(df$recvName)]
  b <- df$recvName[-1]
  tmp <- c(0, 1 - (as.numeric(a==b)))
  run <- cumsum(tmp)
  transitions <- which(diff(run) != 0)
  transitions <- c(transitions, transitions+1, length(df$recvName))
  out.df <- df[transitions,]
  out.df <- out.df[order(out.df$ts),]
  return(out.df)
  
}

##################################################################################
#' Create dataframe for siteTrans function
#'
#' @param data dataframe of Motus detection data containing at a minimum fullID, ts, lat, lon
#'
#' @author Zoe Crysler \email{zcrysler@@gmail.com}
#'

## site.fun and consec.fun adapted from "between.locs.R" script written by Phil
site.fun <- function(df) {
  
  df <- subset(df, select = -c(motusTagID))
  df <- df[order(df$ts),] ## should already be in order, but just in case
  out.df.x <- df[1:(length(df$recvName)-1), ]
  names(out.df.x) <- paste(names(df), "x", sep=".")
  out.df.y <- df[2:length(df$recvName), ]
  names(out.df.y) <- paste(names(df), "y", sep=".")
  out.df <- cbind(out.df.x, out.df.y)
  out.df <- subset(out.df, ((recvName.x != recvName.y)))
  
  
  return(out.df)
}

####################################################################
