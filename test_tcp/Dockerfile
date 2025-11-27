# Build stage
FROM debian:bookworm AS builder

# Install build tools
RUN apt-get update && apt-get install -y \
    fpc \
    make \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Copy test sources
WORKDIR /build
COPY *.pas ./

# Build test tools
RUN echo "=== Building TCP test tools ===" && \
    fpc -O3 tcp_packet_sender.pas && \
    fpc -O3 tcp_echo_server.pas && \
    ls -lh tcp_packet_sender tcp_echo_server && \
    file tcp_packet_sender tcp_echo_server

# Runtime stage
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy test tools
COPY --from=builder /build/tcp_packet_sender /app/tcp_packet_sender
COPY --from=builder /build/tcp_echo_server /app/tcp_echo_server

# Make executable
RUN chmod +x /app/tcp_packet_sender /app/tcp_echo_server

# Default: run packet sender (you can override with CMD in fly.toml)
CMD ["/app/tcp_packet_sender"]

