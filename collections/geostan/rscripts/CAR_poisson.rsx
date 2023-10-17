##geostan=group
##Layer=vector polygon
##Count=Field Layer
##Denominator=Field Layer
##Covaritates=Field Layer
##Covariates=optional string
##Output=output vector

library(geostan)
library(cli)
library(sf)

df <- data.frame(
    Count = Layer[[Count]],
    E = log(Layer[[Denominator]])
)

if (nchar(Covariates[1]) > 0) {
   cli_alert_info("Using {x} covariate{?s}:")
}


Covariates = c('pct_ag_1910', 'wpop', 'bpop')

if (inherits(Covariates, "character")) {
   dtmp <- NULL
   for (i in seq_along(1:length(Covariates))) {
       dtmp <- as.data.frame(cbind(dtmp, Layer[[Covariates[i]]]))
       names(dtmp)[i] <- Covariates[i]
   }
  df <- cbind(df, dtmp)
}

f <- "Count ~ offset(E)"
if () {
   for (i in seq_along(Covariates)) f <- paste0(f, " + ", Covariates[i])
   }   

C <- shape2mat(Layer, style = "B")
car_dl <- prep_car_data(C)

model <- stan_car(Count ~ offset(E) + ., 
    data = df,
    car_parts = car_dl,
    family = poisson(),
    iter = 1e3)

Layer$Fitted_car <- fitted(model)$mean

Output <- Layer
