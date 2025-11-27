#!/bin/bash

echo "=== Local TCP Test ==="
echo ""
echo "This will test TCP tools on localhost before deploying to Fly.io"
echo ""

# Kill any existing servers
killall tcp_packet_sender 2>/dev/null || true
killall tcp_echo_server 2>/dev/null || true
sleep 1

echo "Starting TEST 1: Packet Stream (Burst Detection)"
echo "=============================================="
echo ""

# Start packet sender in background
./bin/tcp_packet_sender &
SENDER_PID=$!
echo "Started tcp_packet_sender (PID: $SENDER_PID)"
sleep 2

# Run receiver for 5 seconds
echo "Running tcp_packet_receiver for 5 seconds..."
timeout 5 ./bin/tcp_packet_receiver 127.0.0.1 5599 || true

# Kill sender
kill $SENDER_PID 2>/dev/null || true
echo ""
echo "✅ TEST 1 complete! Check tcp_receiver.log for results."
echo ""
sleep 2

echo "Starting TEST 2: Echo (RTT Measurement)"
echo "========================================"
echo ""

# Start echo server in background
./bin/tcp_echo_server &
ECHO_PID=$!
echo "Started tcp_echo_server (PID: $ECHO_PID)"
sleep 2

# Run echo client
echo "Running tcp_echo_client..."
./bin/tcp_echo_client 127.0.0.1 5600

# Kill echo server
kill $ECHO_PID 2>/dev/null || true
echo ""
echo "✅ TEST 2 complete! Check tcp_echo_client.log for results."
echo ""

echo "=== All local tests complete! ==="
echo ""
echo "📊 Logs:"
echo "   - tcp_receiver.log"
echo "   - tcp_echo_client.log"
echo ""
echo "If tests passed, deploy to Fly.io:"
echo "   ./deploy_tests.sh"
echo ""
