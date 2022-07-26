install.packages('dplyr')
install.packages('gtfstools')
install.packages('mapview')
install.packages('remotes')

remotes::install_github('ipeaGIT/aopdata', subdir = 'r-package')

zip::unzip("data-raw.zip")
