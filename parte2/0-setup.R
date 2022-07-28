# instalar pacotes
install.packages('dplyr')
install.packages('mapview')
install.packages('remotes')
install.packages('zip')
install.packages('sf')


remotes::install_github('ipeaGIT/aopdata', subdir = 'r-package')
remotes::install_github('ipeaGIT/gtfstools')


# deszipar dados brutos
zip::unzip("data-raw.zip")


remove.packages('gtfstools')
install.packages('gtfstools')