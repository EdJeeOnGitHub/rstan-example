FROM rocker/verse:3.5.3
MAINTAINER "Ed Jee" edjee96@gmail.com

RUN apt-get update \
	&& apt-get install -y --no-install-recommends apt-utils ed libnlopt-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/
    
RUN add-apt-repository -y "ppa:marutter/rrutter" \
	&& add-apt-repository -y "ppa:marutter/c2d4u3.5" \
	&& apt-get update \
	&& apt-get install r-cran-rstan  \
	&& apt-get install r-cran-rstanarm
	
## Run an install.R script, if it exists.
RUN if [ -f install.R ]; then R --quiet -f install.R; fi


# Global site-wide config -- neeeded for building packages
RUN mkdir -p $HOME/.R/ \
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs \n" >> $HOME/.R/Makevars

# Config for rstudio user
RUN mkdir -p $HOME/.R/ \
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations\n" >> $HOME/.R/Makevars \
    && echo "rstan::rstan_options(auto_write = TRUE)\n" >> /home/rstudio/.Rprofile \
    && echo "options(mc.cores = parallel::detectCores())\n" >> /home/rstudio/.Rprofile
