## ----setup---------------------------------------------------------------
set.seed(10)

## ----load_library--------------------------------------------------------
library(omxr)

## ----create_omx, echo=c(1,3)---------------------------------------------
zones <- 1:10
rhdf5::H5close()
create_omx( file = "simple.omx", numrows = length(zones), numcols = length(zones))

## ----make_matrix---------------------------------------------------------
trips <- matrix(rnorm(n = length(zones)^2, 200, 50),  
                nrow = length(zones), ncol = length(zones))
cost <- matrix(rlnorm(n = length(zones)^2, 1, 1),
               nrow = length(zones), ncol = length(zones))

## ----write_omx-----------------------------------------------------------
write_omx(file = "simple.omx", matrix = trips, "trips", 
          description = "Total Trips")

write_omx(file = "simple.omx", matrix = cost, "cost", 
          description = "Generalized Cost")

## ----read_matrix---------------------------------------------------------
read_omx("simple.omx", "trips")
read_omx("simple.omx", "cost")

## ----long_matrix(), message=FALSE, warning=FALSE-------------------------
library(tidyverse)
read_omx("simple.omx", "trips") %>%
  long_matrix(value = "trips")

## ----read_subset---------------------------------------------------------
read_omx("simple.omx", "trips", row_index = 2:4, col_index = 2:5)

## ----attributes----------------------------------------------------------
get_omx_attr("simple.omx")
list_omx("simple.omx")

## ----write_lookup--------------------------------------------------------
lookup <- zones %in% c(1, 2:5, 9)
lookup
write_lookup("simple.omx", lookup_v = lookup, 
             name = "trial", description = "test lookup", replace = TRUE)

## ----read_lookup---------------------------------------------------------
read_selected_omx("simple.omx", "trips", 
                  row_selection = "trial", col_selection = "trial")

