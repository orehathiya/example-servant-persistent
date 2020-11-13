FROM fpco/stack-build:lts-16.21 as build
WORKDIR /opt/build
COPY stack.yaml stack.yaml.lock package.yaml example-servant-persistent.cabal ./
RUN stack build --system-ghc --only-dependencies
COPY . /opt/build
RUN stack build --system-ghc
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

FROM ubuntu:latest
WORKDIR /opt/myapp/
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp-dev \
    libncurses5
COPY --from=build /opt/build/bin ./
EXPOSE 3000
CMD ["/opt/myapp/example-servant-persistent"]
