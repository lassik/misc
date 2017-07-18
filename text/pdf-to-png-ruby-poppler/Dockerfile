FROM ruby:alpine
RUN apk update && apk add poppler-utils ghostscript-fonts
VOLUME /app
COPY . /app
WORKDIR /app
CMD sh
