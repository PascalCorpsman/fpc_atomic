# TCP Test Tools

Nástroje pro testování TCP připojení a detekci packet burstingu.

## 🚀 Quick Start

### 1️⃣ Zkompiluj nástroje

```bash
cd test_tcp
chmod +x *.sh
./build_tcp_tests.sh
```

**Výstup:**
```
✅ tcp_packet_sender built
✅ tcp_packet_receiver built
✅ tcp_echo_server built
✅ tcp_echo_client built
```

### 2️⃣ Lokální test (ověření funkčnosti)

```bash
./test_local.sh
```

Toto spustí oba testy na `localhost` a ověří, že nástroje fungují správně.

### 3️⃣ Deploy na Fly.io

```bash
./deploy_tests.sh
```

Server se deployne na Fly.io a zobrazí IP adresu pro připojení.

## 📊 TEST 1: Packet Stream (Burst Detection)

**Účel:** Zjistit, jestli TCP bufferuje packety (Delayed ACK, Nagle's algorithm).

**Server posílá packety každých 20ms → Client měří intervaly mezi příjmy**

### Na Fly.io serveru:

```bash
# Deploy pomocí Dockerfile nebo spusť lokálně
./bin/tcp_packet_sender
```

### Na tvém Macu:

```bash
./bin/tcp_packet_receiver 37.16.31.7 5599
```

### Co sledovat:

- ✅ **Interval = 20ms** → TCP funguje perfektně
- 🔴 **Interval > 50ms** → BURST! TCP bufferuje packety
- 📊 **Log:** `tcp_receiver.log` s detailními statistikami

**Příklad problematického výstupu:**
```
[12:34:56.100] Received 50 packets, last interval=22ms, bursts=0, lost=0
[12:34:56.890] 🔴 BURST DETECTED: seq=52, interval=88ms (expected 20ms)
[12:34:56.910] Received 100 packets, last interval=20ms, bursts=1, lost=0
```

---

## 🏓 TEST 2: Echo (RTT Measurement)

**Účel:** Změřit Round-Trip Time (RTT) a stabilitu spojení.

**Client posílá packet → Server okamžitě vrací → Client měří čas**

### Na Fly.io serveru:

```bash
./bin/tcp_echo_server
```

### Na tvém Macu:

```bash
./bin/tcp_echo_client 37.16.31.7 5600
```

### Co sledovat:

- ✅ **RTT < 50ms** → Normální evropská latence
- ⚠️ **RTT > 100ms** → Vysoká latence nebo buffering
- 📊 **Min/Max/Avg RTT** → Stabilita spojení
- 📊 **Log:** `tcp_echo_client.log` s detailními statistikami

**Příklad výstupu:**
```
[12:34:56.100] Sent=20, Received=20, Last RTT=32ms, Avg RTT=34.2ms
[12:34:57.150] 🔴 HIGH RTT: seq=35, RTT=145ms
```

---

## 📈 Interpretace výsledků

### Pokud TEST 1 ukáže bursting:

```
Bursts detected: 15 (interval > 50ms)
Max interval: 120ms
```

**➡️ TCP bufferuje packety!** Příčiny:
- Delayed ACK (TCP defaultní chování)
- Router agregace packetů
- ISP throttling

**Řešení:**
- ✅ `TCP_NODELAY` je už nastaveno
- ✅ Zkus změnit `SO_SNDBUF` / `SO_RCVBUF`
- ✅ Client-side prediction/interpolace (doporučuji!)

### Pokud TEST 2 ukáže vysokou RTT:

```
Min RTT: 28ms
Max RTT: 234ms
Avg RTT: 67ms
```

**➡️ Nestabilní spojení!** Příčiny:
- Vysoká latence k Fly.io
- Router QoS (Quality of Service)
- WiFi interference

**Řešení:**
- ✅ Client-side interpolace (hra bude plynulá i při lagu)
- ✅ Zvětšit `SynchronizeTimeout` (už máš 400ms)

---

## 🚀 Deploy na Fly.io

### Packet Sender:

```dockerfile
# flyio_tcp/Dockerfile - přidat:
COPY --from=builder /build/test_tcp/bin/tcp_packet_sender /app/tcp_packet_sender

# fly.toml - přidat service:
[[services.ports]]
  port = 5599
  handlers = []
```

### Echo Server:

```dockerfile
COPY --from=builder /build/test_tcp/bin/tcp_echo_server /app/tcp_echo_server

# fly.toml - přidat service:
[[services.ports]]
  port = 5600
  handlers = []
```

**Nebo spusť manuálně přes `flyctl ssh console`:**

```bash
flyctl ssh console -a fpc-atomic-tcp-server
cd /app
./tcp_packet_sender
# nebo
./tcp_echo_server
```

---

## 📝 Očekávané výsledky

### Ideální scénář:
```
=== STATISTICS ===
Packets received: 1500
Packets lost: 0
Loss rate: 0.00%
Min interval: 19ms
Max interval: 24ms
Avg interval: 20.1ms
Bursts detected (>50ms): 0
```

### Realistický scénář (buffering):
```
=== STATISTICS ===
Packets received: 1487
Packets lost: 13
Loss rate: 0.86%
Min interval: 0ms    ← Burst: několik packetů najednou!
Max interval: 147ms  ← Burst: dlouhá pauza
Avg interval: 20.3ms ← Průměr vypadá OK, ale...
Bursts detected (>50ms): 23  ← 23x přišly packety opožděně!
```

**➡️ Právě toto způsobuje "zpomalení" hry!**

---

## 🎯 Další kroky

Po spuštění testů:
1. Pošli mi výstupy (console + log soubory)
2. Vyhodnotíme, jestli je problém v TCP nebo někde jinde
3. Pokud je buffering, implementuju client-side interpolaci
