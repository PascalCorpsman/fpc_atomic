# TCP Test Server - Fly.io Deployment

## 🚀 Deploy Time Server

```bash
cd test_tcp
./deploy_tcp_server.sh time
```

**Server address:** `37.16.31.7:5522`

**Test from local machine:**
```bash
./tcp_time_client_arm64 37.16.31.7 5522
```

**View logs:**
```bash
flyctl logs --app fpc-atomic-tcp-time-test
```

**Stop server:**
```bash
flyctl apps destroy fpc-atomic-tcp-time-test
```

---

## 🔄 Deploy Echo Server

```bash
cd test_tcp
./deploy_tcp_server.sh echo
```

**Server address:** `37.16.31.7:5523`

**Test from local machine:**
```bash
./tcp_echo_client_arm64 37.16.31.7 5523
```

**View logs:**
```bash
flyctl logs --app fpc-atomic-tcp-echo-test
```

**Stop server:**
```bash
flyctl apps destroy fpc-atomic-tcp-echo-test
```

---

## 📊 What to Look For

### Time Test (Bursting Detection)
✅ **GOOD:**
- Interval ~20ms (±5ms)
- No "BURST DETECTED!" messages
- Low latency (<50ms)

⚠️ **BAD (explains game slowdown):**
- Interval varies wildly (0-100ms+)
- Frequent "BURST DETECTED!" messages
- High burst rate (>10%)

### Echo Test (Latency Measurement)
✅ **GOOD:**
- RTT <50ms
- Packet loss <1%
- Stable RTT (min/max similar)

⚠️ **BAD:**
- RTT >100ms
- Packet loss >5%
- RTT varies wildly

---

## 🐛 Troubleshooting

### Server fails to deploy
```bash
# Check status
flyctl status --app fpc-atomic-tcp-time-test

# View logs
flyctl logs --app fpc-atomic-tcp-time-test

# Restart
flyctl apps restart fpc-atomic-tcp-time-test
```

### Client can't connect
- Check firewall (System Settings → Network → Firewall)
- Verify server is running: `flyctl status --app <app-name>`
- Try from different network (mobile hotspot)

### High packet loss
- Likely ISP/router issue
- Try different network
- Check router QoS settings

---

## 💡 Next Steps

Based on test results:

### If bursting detected:
1. **Implement client-side interpolation** in game
2. Server sends authoritative state
3. Client interpolates between updates
4. Time runs smoothly even with bursts

### If high latency but no bursting:
1. Game is OK, just high ping
2. Consider regional servers
3. Implement lag compensation

### If packet loss:
1. Check router settings
2. Consider different ISP
3. Use mobile hotspot for testing

