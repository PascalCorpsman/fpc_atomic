#!/bin/bash

set -e

echo "=== Building TCP Test Tools ==="
echo ""

# Create output directory
mkdir -p bin

# Build packet sender (server)
echo "Building tcp_packet_sender (server)..."
fpc -O3 -FE./bin tcp_packet_sender.pas
echo "✅ tcp_packet_sender built"
echo ""

# Build packet receiver (client)
echo "Building tcp_packet_receiver (client)..."
fpc -O3 -FE./bin tcp_packet_receiver.pas
echo "✅ tcp_packet_receiver built"
echo ""

# Build echo server
echo "Building tcp_echo_server..."
fpc -O3 -FE./bin tcp_echo_server.pas
echo "✅ tcp_echo_server built"
echo ""

# Build echo client
echo "Building tcp_echo_client..."
fpc -O3 -FE./bin tcp_echo_client.pas
echo "✅ tcp_echo_client built"
echo ""

echo "=== Build Complete! ==="
echo ""
echo "Executables in: test_tcp/bin/"
echo ""
echo "TEST 1: Packet Stream (Burst Detection)"
echo "  On Fly.io server: ./bin/tcp_packet_sender"
echo "  On your Mac:      ./bin/tcp_packet_receiver 37.16.31.7 5599"
echo ""
echo "TEST 2: Echo (RTT Measurement)"
echo "  On Fly.io server: ./bin/tcp_echo_server"
echo "  On your Mac:      ./bin/tcp_echo_client 37.16.31.7 5600"
echo ""
