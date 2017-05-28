FROM ruby:alpine
VOLUME /app
COPY . /app
WORKDIR /app
RUN bundle
CMD ruby app.rb
