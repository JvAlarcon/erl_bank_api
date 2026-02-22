FROM erlang:28-alpine
# Install build dependencies
RUN apk add --no-cache git build-base

WORKDIR /app
COPY rebar.config rebar.lock ./
COPY src/ ./src/

# Fetch dependencies and compile
RUN rebar3 do deps, compile
# Create a release
RUN rebar3 release

# Minimal runtine image to keep things lean
FROM alpine:3.19 AS runner
RUN apk add --no-cache libstdc++ libgcc ncurses-libs openssl
WORKDIR /app
COPY --from=builder /app/_build/default/rel/erl_bank_api/ ./
EXPOSE 8080
CMD ["bin/erl_bank_api", "foreground"]