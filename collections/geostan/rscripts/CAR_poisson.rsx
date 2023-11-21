##geostanTest=group
##Layer=vector polygon
##Outcome=Field Layer
##Denominator=optional Field Layer
##Covariates=optional Field multiple Layer
##family=selection Normal;Poisson;Binomial

##Chains=number 4
##Iterations=number 2000
##Cores=number 1

##Save_model=selection No;Yes
##Results_folder=optional folder
##Output=output vector

library(geostan)
library(readr)
library(cli)
library(sf)

# set base formula 
if (family == 0) form <- paste0(Outcome, " ~ 1")
if (family == 1) form <- paste0(Outcome, " ~ 1 + ", "offset(log(", Denominator, "))")
if (family == 2) form <- paste0("cbind(", Outcome, ", ", Denominator, " - ", Outcome, ") ~ 1")

# add covariates if provided
if (is.null(Covariates) == FALSE) {
    for (i in seq_along(Covariates)) {
   form <- paste0(form, " + ", Covariates[i])
    }    
}

# print model formula
cli_alert_info("Setting model formula to:\n {.code {form}}\n\n")

# string to formula
form <- formula(form)
   
# create weights matrix 
C <- shape2mat(Layer, style = "B")

# create list of data for CAR model
car_dl <- prep_car_data(C)

# sample from the Stan model
if (family == 0) {
    model <- stan_car(form, 
                      data = Layer,
                      C = C,
                      car_parts = car_dl,
                      family = gaussian(),
                      iter = Iterations,
                      chains = Chains,
                      cores = Cores)
}

if (family == 1) {
    model <- stan_car(form, 
                      data = Layer,
                      C = C,
                      car_parts = car_dl,
                      family = poisson(),
                      iter = Iterations,
                      chains = Chains,
                      cores = Cores)
}

if (family == 2) {
    model <- stan_car(form, 
                      data = Layer,
                      C = C,
                      car_parts = car_dl,
                      family = binomial(),
                      iter = Iterations,
                      chains = Chains,
                      cores = Cores)
}

# add fitted values to output layer
fdf <- fitted(model)[, c("mean", "sd", "2.5%", "97.5%")]
names(fdf) <- c("fitted", "fitted_sd", "fitted_lwr_2.5%", "fitted_upr_97.5%")
Output <- sf::st_sf(data.frame(Layer, fdf))

# print model summary
cat("\n")
print(model)

# save with date and time
file_ID <- format(Sys.time(), "%b %d %X %Y")

# save model summary to disk
if (nchar(Results_folder) == 0) Results_folder <- getwd()
if (!dir.exists(Results_folder)) {
  cat("\n")
  cli_alert_info("Creating directory for results: {.file {Results_folder}}")
  dir.create(Results_folder)  
 }
path <- file.path(Results_folder, paste0("CAR-summary-", file_ID, ".txt"))
sink(path)
print(model)
sink()

cat("\n")
cli_alert_info("Summary results saved to {.file {path}}\n\n")

# save geostan model to disk if selected
if (Save_model == "Yes") {
    path <- file.path(Results_folder, paste0("CAR-model-", file_ID, ".txt"))
    readr::write_rds(model, path2)
    cli_alert_info("Model saved to {.file {path2}}\n\n")   
}
