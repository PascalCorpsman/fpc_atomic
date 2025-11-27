#!/bin/bash

set -e

echo "=== Deploying TCP Test Tools to Fly.io ==="
echo ""

# Check if fly app exists
if ! flyctl status -a fpc-atomic-tcp-tests &>/dev/null; then
  echo "Creating new Fly.io app..."
  flyctl apps create fpc-atomic-tcp-tests --org personal
  
  echo "Allocating IPv4 address..."
  flyctl ips allocate-v4 -a fpc-atomic-tcp-tests
fi

echo "Building and deploying..."
flyctl deploy -a fpc-atomic-tcp-tests

echo ""
echo "=== Deployment Complete! ==="
echo ""

# Get IP
IP=$(flyctl ips list -a fpc-atomic-tcp-tests -j | grep -o '"address":"[^"]*' | grep -o '[0-9.]*' | head -1)

echo "Server IP: $IP"
echo ""
echo "✅ TEST 1: Packet Stream (Burst Detection)"
echo "   Server is running tcp_packet_sender on port 5599"
echo "   On your Mac: cd test_tcp && ./bin/tcp_packet_receiver $IP 5599"
echo ""
echo "✅ TEST 2: Echo (RTT Measurement)"
echo "   To run echo server, connect via SSH and start it:"
echo "   flyctl ssh console -a fpc-atomic-tcp-tests"
echo "   Then: /app/tcp_echo_server"
echo "   On your Mac: cd test_tcp && ./bin/tcp_echo_client $IP 5600"
echo ""
echo "📊 View logs: flyctl logs -a fpc-atomic-tcp-tests"
echo ""

