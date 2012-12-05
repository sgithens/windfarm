convertDate <- function(winddate) {
  year <- substr(winddate,1,4)
  month <- substr(winddate,5,6)
  day <- substr(winddate,7,8)
}

rundata <- function() {
  siteClasses <- c()
  siteClasses$DATE <- "character"

  site1.dat <- read.csv("/home/sgithens/code/windfarm/actual/SITE_00001.CSV",
                        header=TRUE, skip=2,
                        colClasses=siteClasses)

  site1.dat$DATE <- as.Date(site1.dat$DATE, format="%Y%m%d")
  
  return(site1.dat)
}

plotDay <- function(oneday) {
  attach(oneday)
  png(filename="images/2004-01-01.time.vs.speed.png")
  plot(TIME.UTC.,SPEED80M.M.S.,type="l")
  dev.off()

  png(filename="images/2004-01-01.time.vs.power.png")
  plot(TIME.UTC.,NETPOWER.MW.,type="l")
  dev.off()

  png(filename="images/2004-01-01.speed.vs.power.png")
  plot(SPEED80M.M.S.,NETPOWER.MW.)
  dev.off()
  
  detach(oneday)
}

getOneDay <- function() {
  d <- rundata()
  oneday <- subset(d,DATE=='2004-01-01')
  return(oneday)
}

fitLinearSpeedPowerModel <- function(data) {
  attach(data)
  #m <- lm(NETPOWER.MW. ~ SPEED80M.M.S.)
  m <- lm(NETPOWER.MW. ~ SPEED80M.M.S. + I(SPEED80M.M.S.^2) + I(SPEED80M.M.S.^3) + I(SPEED80M.M.S.^4) + I(SPEED80M.M.S.^5) )
  #m <- lm(NETPOWER.MW. ~ I(sqrt(SPEED80M.M.S.)))
  print(summary(m))
  detach(data)
  return(m)
}

fitCurvedSpeedPowerModel <- function(data) {
  attach(data)
  m <- nls(NETPOWER.MW. ~ SSweibull(SPEED80M.M.S.,Asym,Drop,lrc,pwr))
  print(summary(m))
  detach(data)
  return(m)
}

main <- function() {
  oneday <- getOneDay()
  plotDay(oneday)
  m <- fitCurvedSpeedPowerModel(oneday)
  attach(oneday)
  png(filename="images/SSweibullFit.png")
  plot(SPEED80M.M.S.,NETPOWER.MW.)
  points(SPEED80M.M.S.,predict(m), col = "blue")
  dev.off()
  detach(oneday)
}





