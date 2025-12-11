# Dockerfile for FPC Atomic TCP Server on Fly.io
# Multi-stage build for smaller final image

FROM ubuntu:22.04 AS builder

WORKDIR /build

# Install Lazarus and dependencies (including gcc for AI library)
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    fpc \
    fp-compiler \
    lazarus \
    lazarus-ide-gtk2 \
    libgtk2.0-dev \
    build-essential \
    gcc \
    make \
    && rm -rf /var/lib/apt/lists/*

# Copy source code
# Build context should be project root, not flyio_server/
COPY server /build/server
COPY units /build/units
COPY client /build/client
COPY macos/third_party/lnet_src /build/lnet_src
COPY ai /build/ai

# Copy data directory
# Note: Workflow ensures flyio_server/data exists (copies from data/ if needed)
# If flyio_server/data doesn't exist, this will fail - but workflow should handle it
COPY flyio_server/data /build/data

# Build AI library (Pascal version - fully functional!)
WORKDIR /build/ai
RUN echo "=== Building Pascal AI library ===" && \
    cp /build/server/uai_types.pas . && \
    cp /build/units/ufifo.pas . && \
    fpc -Tlinux -Px86_64 -O3 -CX -XX -Xs -vw ai.lpr && \
    mv libai.so ../libai.so && \
    echo "=== AI library built, checking..." && \
    ls -la ../libai.so && \
    file ../libai.so && \
    nm -D ../libai.so | grep -E "(AiInit|AiHandlePlayer|AiVersion)" && \
    echo "=== Pascal AI library OK ==="

# Modify .lpi to use correct search paths for Docker build and compile
WORKDIR /build/server
RUN sed -i 's|../../Sample/TCP_IP;../../Sample/DatenSteuerung;../units;../../Sample/Graphik;../macos/third_party/lnet/lib;../macos/third_party/lnet_src/lazaruspackage|../units;../lnet_src/lib|g' atomic_server.lpi && \
    echo "=== Building TCP server with Lazarus ===" && \
    lazbuild --lazarusdir=/usr/lib/lazarus/default --build-mode="Default" atomic_server.lpi && \
    mkdir -p /app && \
    mv /build/atomic_server /app/atomic_server && \
    mv /build/libai.so /app/libai.so && \
    ls -la /app/atomic_server /app/libai.so && \
    file /app/atomic_server /app/libai.so

# Runtime stage
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libc6 \
    && rm -rf /var/lib/apt/lists/*

# Copy compiled server, AI library, and data directory
COPY --from=builder /app/atomic_server /app/atomic_server
COPY --from=builder /app/libai.so /app/libai.so
COPY --from=builder /build/data /app/data

# Make executable
RUN chmod +x /app/atomic_server

# Expose TCP port
EXPOSE 5521/tcp

# Run server
# -p 5521 = port
# -t 30000 = auto-shutdown after 30s with no players (for clean restart)
# -l 2 = log level Info
CMD ["/app/atomic_server", "-p", "5521", "-t", "30000", "-l", "2"]

