FROM alpine:edge
RUN \
    apk update && \
    apk add ruby ruby-io-console
RUN \
    echo 'http://nl.alpinelinux.org/alpine/edge/testing' >> /etc/apk/repositories && \
    apk update && \
    apk add wkhtmltopdf
RUN gem install --no-document bundler
VOLUME /app
COPY . /app
WORKDIR /app
RUN bundle
CMD ruby app.rb
