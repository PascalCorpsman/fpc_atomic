# Dockerfile for FPC Atomic TCP Server on Fly.io
# Multi-stage build for smaller final image

FROM ubuntu:22.04 AS builder

WORKDIR /build

# Install Lazarus and dependencies
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    fpc \
    fp-compiler \
    lazarus \
    lazarus-ide-gtk2 \
    libgtk2.0-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Copy source code
COPY server /build/server
COPY units /build/units
COPY client /build/client
COPY macos/third_party/lnet_src /build/lnet_src
COPY macos/data /build/data

# Modify .lpi to use correct search paths for Docker build and compile
WORKDIR /build/server
RUN sed -i 's|../../Sample/TCP_IP;../../Sample/DatenSteuerung;../units;../../Sample/Graphik;../macos/third_party/lnet/lib;../macos/third_party/lnet_src/lazaruspackage|../units;../lnet_src/lib|g' atomic_server.lpi && \
    echo "=== Building TCP server with Lazarus ===" && \
    lazbuild --lazarusdir=/usr/lib/lazarus/default --build-mode="Default" atomic_server.lpi && \
    mkdir -p /app && \
    mv /build/atomic_server /app/atomic_server && \
    ls -la /app/atomic_server && \
    file /app/atomic_server

# Runtime stage
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libc6 \
    && rm -rf /var/lib/apt/lists/*

# Copy compiled server and data
COPY --from=builder /app/atomic_server /app/atomic_server
COPY --from=builder /build/data /app/data

# Make executable
RUN chmod +x /app/atomic_server

# Expose TCP port
EXPOSE 5521/tcp

# Run server
# -p 5521 = port
# -t 0 = no time limit
# -l 2 = log level Info
CMD ["/app/atomic_server", "-p", "5521", "-t", "0", "-l", "2"]

