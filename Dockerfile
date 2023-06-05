# Build stage 0
FROM erlang:26-alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang application
COPY . .

# And build the release
RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Install the released application
COPY --from=0 /buildroot/_build/prod/rel/msgsvc /msgsvc

# Expose relevant ports
EXPOSE 8080

CMD ["/msgsvc/bin/msgsvc", "foreground"]
