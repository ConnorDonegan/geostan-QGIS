##geostan=group
##Layer=vector polygon
##Count=Field Layer
##Denominator=Field Layer
##Single_covariate=optional Field Layer
##Covariates_formula=optional string
##Results_folder=optional folder
##Results_file_ID=optional string CAR-poisson-
##Output=output vector

library(geostan)
library(cli)
library(sf)

cat("\n------------------\n\n")

if (nchar(Single_covariate) > 0 & nchar(Covariates_formula) > 0) {
  stop("Covariate formula given and single covariate selection made. \nEither use the dropdown menu for selecting a single covariate, OR provide a Covariate formula.")
}

LHS <- ".+~"
if(grepl(LHS, Covariates_formula)) {
  cli_alert_warning("Only include right-hand side of the formula to specify covariates, as in 'x1 + log10(x2)', and NOT 'y ~ x1 + log10(x2)'.")
  Covariates_formula <- sub(LHS, "", Covariates_formula)
  cli_alert_info("User-provided formula changed to: {.code {Covariates_formula}}")
}

if (grepl("offset", Covariates_formula, fixed = TRUE)) {
  cli_abort("The phrase 'offset' was found in the Covariates formula. The offset must be provided through the Denominator selection menu; your Covariates formula was {.code {Covariates_formula}}.")
}

# set base formula 
form <- paste0(Count, " ~ ", "offset(log(", Denominator, "))")

# add single covariate if provided
if (nchar(Single_covariate) > 0) {
   form <- paste0(form, " + ", Single_covariate)
   cli_alert_info("Covariate selection of {.emph Single_covariate} identified")
   cli_alert_info("Setting model formula to {.code {form}}\n\n")
}

# use Covariates formula if provided
if (nchar(Covariates_formula) > 0) {
   form <- paste0(form, " + ", Covariates_formula)
   cli_alert_info("Using user-provided Covariates formula; model formula is\n {.code {form}}\n\n")
}

# string to formula
form <- formula(form)
   
# create weights matrix 
C <- shape2mat(Layer, style = "B")

# create list of data for CAR model
car_dl <- prep_car_data(C)

# sample from the Stan model
model <- stan_car(form, 
    data = Layer,
    C = C,
    car_parts = car_dl,
    family = poisson(),
    iter = 1e3)

# add fitted values to output layer
fdf <- fitted(model)[, c("mean", "sd", "2.5%", "97.5%")]
names(fdf) <- c("fitted", "sd", "fitted_lwr_2.5%", "fitted_upr_97.5%")
Output <- sf::st_sf(data.frame(Layer, fdf))

# print model summary
cat("\n")
print(model)

# save model summary to disk, if files given
if (nchar(Results_folder) == 0) Results_folder <- getwd()
if (!dir.exists(Results_folder)) {
  cat("\n")
  cli_alert_info("Creating directory for results: {.file {Results_folder}}")
  dir.create(Results_folder)  
 }
path <- file.path(Results_folder, paste0(Results_file_ID, "summary.txt"))
sink(path)
print(model)
sink()
cat("\n")
cli_alert_info("Summary results saved to {.file {path}}\n\n")

