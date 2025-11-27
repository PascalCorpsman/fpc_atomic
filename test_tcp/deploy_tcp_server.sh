#!/bin/bash
set -e

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <time|echo>"
    echo "  time - Deploy TCP time server (port 5522)"
    echo "  echo - Deploy TCP echo server (port 5523)"
    exit 1
fi

TYPE=$1
cd "$(dirname "$0")"

if [ "$TYPE" = "time" ]; then
    APP_NAME="fpc-atomic-tcp-time-test"
    PORT=5522
    SERVER_FILE="tcp_time_server.pas"
    BINARY_NAME="tcp_time_server"
elif [ "$TYPE" = "echo" ]; then
    APP_NAME="fpc-atomic-tcp-echo-test"
    PORT=5523
    SERVER_FILE="tcp_echo_server.pas"
    BINARY_NAME="tcp_echo_server"
else
    echo "ERROR: Unknown type '$TYPE'. Use 'time' or 'echo'"
    exit 1
fi

echo "=== Deploying $TYPE server to Fly.io ==="
echo "App: $APP_NAME"
echo "Port: $PORT"
echo ""

# Create Dockerfile
cat > Dockerfile.test <<EOF
# Build stage
FROM debian:bookworm-slim AS builder

# Install dependencies
RUN apt-get update && apt-get install -y \\
    fpc \\
    libssl-dev \\
    wget \\
    && rm -rf /var/lib/apt/lists/*

# Copy source files
WORKDIR /build
COPY $SERVER_FILE /build/
COPY ../macos/third_party/lnet_src /build/lnet_src

# Build server
RUN fpc -O3 -Fu/build/lnet_src/lib -o/build/$BINARY_NAME $SERVER_FILE

# Runtime stage
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \\
    libssl3 \\
    && rm -rf /var/lib/apt/lists/*

# Copy binary
COPY --from=builder /build/$BINARY_NAME /app/$BINARY_NAME

# Run server
CMD ["/app/$BINARY_NAME", "$PORT"]
EOF

# Create fly.toml
cat > fly.test.toml <<EOF
app = "$APP_NAME"
primary_region = "ams"

[build]
  dockerfile = "Dockerfile.test"

[env]
  PORT = "$PORT"

[[services]]
  protocol = "tcp"
  internal_port = $PORT

  [[services.ports]]
    port = $PORT

  [[services.tcp_checks]]
    interval = "15s"
    timeout = "10s"
    grace_period = "5s"
EOF

echo "Deploying to Fly.io..."
flyctl deploy -c fly.test.toml

echo ""
echo "=== Deployment Complete! ==="
echo ""
echo "Server address: 37.16.31.7:$PORT"
echo ""
echo "Test from local machine:"
if [ "$TYPE" = "time" ]; then
    echo "  ./tcp_time_client_arm64 37.16.31.7 $PORT"
elif [ "$TYPE" = "echo" ]; then
    echo "  ./tcp_echo_client_arm64 37.16.31.7 $PORT"
fi
echo ""
echo "View logs:"
echo "  flyctl logs --app $APP_NAME"
echo ""

