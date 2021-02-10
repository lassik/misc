FROM debian:buster
WORKDIR /
RUN apt-get update
RUN apt-get install -y ca-certificates git git-svn subversion
RUN mkdir /git /svn
ADD https://files.scheme.org/vx-scheme-20080617-repo.svndump.gz /svn/
COPY convert-inside-docker.sh /
RUN sh /convert-inside-docker.sh | tee /git/git-svn.log
RUN mv /git/git-svn.log /git/vx-scheme-20080617-repo.git/
WORKDIR /git
RUN tar -czf vx-scheme-20080617-repo.git.tar.gz \
  vx-scheme-20080617-repo.git
