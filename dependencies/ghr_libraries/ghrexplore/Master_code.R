# Dependencies 
usethis::use_package("dplyr",type = "Imports")
usethis::use_package("rlang",type = "Imports")
usethis::use_package("tidyr",type = "Imports")
usethis::use_package("ggplot2",type = "Imports")
usethis::use_package("grDevices",type = "Imports")

usethis::use_package("sf",type = "Suggests")
#usethis::use_package("lintr",type = "Suggests")
#usethis::use_package("styler",type = "Suggests")

#Cread Manual folder
devtools::document()

#Create Test Folder 
usethis::use_test()

#Test Helper function
usethis::use_test("helper_functions")
