FROM rocker/geospatial:4.0.4

LABEL org.label-schema.license="GPL-2.0" \
    org.label-schema.vcs-url="https://github.com/rocker-org/rocker-versioned" \
    org.label-schema.vendor="Rocker Project" \
    maintainer="Carl Boettiger <cboettig@ropensci.org>"

ENV RSTUDIO_VERSION 1.3.959
RUN /rocker_scripts/install_rstudio.sh

ENV NB_USER=jovyan

RUN /rocker_scripts/install_python.sh
RUN /rocker_scripts/install_binder.sh

CMD jupyter notebook --ip 0.0.0.0

USER ${NB_USER}

WORKDIR /home/${NB_USER}
