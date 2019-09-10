
FROM ubuntu:18.04
LABEL maintainer="Dominique Besagni <dominique.besagni@inist.fr>"

# Install application and set rights

COPY neurodoc.pl /usr/bin/neurodoc
RUN chmod 0755 /usr/bin/neurodoc


# Install K-means programmes

WORKDIR /usr/local/kmeans
COPY Kmeans-programmes /usr/local/kmeans


# Install MATLAB scripts 

WORKDIR /usr/share/matlab
COPY Octave-scripts/*.m /usr/share/matlab/
ENV MATLABPATH /usr/share/matlab


# Install ACC programme

WORKDIR /usr/local/acc
COPY ACC /usr/local/acc


# Install necessary compilers, make and clean up

RUN buildDeps='gcc gfortran libc6-dev make' \
    && apt-get update \
    && apt-get install -y $buildDeps cpanminus octave --no-install-recommends \
    && rm -rf /var/lib/apt/lists/* \
    && (cd /usr/local/kmeans \
        && make \
        && chmod 0755 IndocInitMat IndocKmeansAx \
        && mv IndocInitMat IndocKmeansAx /usr/bin/) \
    && rm -r /usr/local/kmeans \
    && (cd /usr/local/acc \
        && make \
        && chmod 0755 acc \
        && mv acc /usr/bin/) \
    && rm -r /usr/local/acc \
    && apt-get purge -y --auto-remove $buildDeps \
    && apt-get clean


# Run Neurodoc

WORKDIR /tmp
CMD ["neurodoc", "-h"]
