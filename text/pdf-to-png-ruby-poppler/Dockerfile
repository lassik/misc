FROM ruby:alpine
RUN apk update && apk add poppler-utils
VOLUME /app
COPY . /app
WORKDIR /app
CMD sh
