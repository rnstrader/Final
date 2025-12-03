#official R plumber image
FROM rstudio/plumber:latest

#Install libraries
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libpng-dev pandoc

#Install R packages required
RUN R -e "install.packages(c('GGally', 'leaflet', 'plumber', 'tidyverse', 'tidymodels', 'ranger', 'ggplot2'))"

#Copy my API and related files
COPY API.R API.R
COPY diabetes.rds diabetes.rds
COPY final_rf_wf.rds final_rf_wf.rds

#port plumber will run on
EXPOSE 8000

#Starting API when the container launches
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(\"API.R\"); pr$run(host='0.0.0.0', port=8000)"]