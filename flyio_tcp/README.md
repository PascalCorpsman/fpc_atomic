# Deployment FPC Atomic Server na Fly.io

## 📋 Požadavky

1. **Fly.io CLI** - nainstalovaný a přihlášený
   ```bash
   # Instalace (macOS)
   curl -L https://fly.io/install.sh | sh
   
   # Přihlášení
   flyctl auth login
   ```

2. **Docker** - pro lokální testování (volitelné)

## 🚀 Rychlý postup

### 1. Přejít do adresáře

```bash
cd flyio_tcp
```

### 2. Deploy na Fly.io

**První deploy (vytvoří novou aplikaci):**
```bash
flyctl launch
```

**Nebo pokud už máte aplikaci:**
```bash
flyctl deploy
```

### 3. Zobrazení informací

```bash
# Zobrazit status a IP adresu
flyctl status

# Zobrazit logy
flyctl logs

# SSH do kontejneru (pro debugging)
flyctl ssh console
```

## 📝 Co se děje při deployi?

1. **Build**: Dockerfile automaticky:
   - Zkompiluje AI knihovnu (`libai.so`) pro Linux
   - Zkompiluje server (`atomic_server`) pro Linux
   - Zkopíruje data adresář (obsahuje mapy a další herní data)

2. **Deploy**: Fly.io:
   - Vytvoří Docker image
   - Spustí server na portu 5521
   - Server se automaticky vypne po 30 sekundách nečinnosti (auto_stop_machines)
   - Server se automaticky zapne při novém připojení (auto_start_machines)

## ⚙️ Konfigurace

### Port
Server naslouchá na portu **5521** (nastaveno v `fly.toml` a `Dockerfile`).

### Timeout
Server se automaticky vypne po **30 sekundách** nečinnosti (nastaveno v `Dockerfile` jako `-t 30000`).

### Region
Defaultní region je **Frankfurt (fra)** - můžete změnit v `fly.toml`.

## 🔧 Troubleshooting

### Build selže

1. Zkontrolujte, že všechny potřebné soubory jsou v projektu:
   - `server/` - zdrojové kódy serveru
   - `units/` - jednotky
   - `macos/third_party/lnet_src/` - LNet knihovna
   - `macos/data/` - game data
   - `ai/` - AI knihovna

2. Zkuste lokální build:
   ```bash
   docker build -t fpc-atomic-test .
   ```

### Server se nespustí

1. Zkontrolujte logy:
   ```bash
   flyctl logs
   ```

2. Ověřte, že data adresář existuje a obsahuje potřebné soubory (mapy v `data/maps/`)

### Připojení klientů

1. Získejte IP adresu serveru:
   ```bash
   flyctl status
   ```

2. Použijte IP adresu a port **5521** v klientovi

## 📊 Monitoring

```bash
# Reálné logy
flyctl logs

# Status aplikace
flyctl status

# Metriky
flyctl metrics
```

## 🔗 Užitečné odkazy

- [Fly.io dokumentace](https://fly.io/docs/)
- [Fly.io CLI reference](https://fly.io/docs/flyctl/)

