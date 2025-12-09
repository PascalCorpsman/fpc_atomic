# Deployment FPC Atomic Server na Fly.io

## 🌐 Co je Fly.io?

**Fly.io** je cloudová platforma, která umožňuje spouštět aplikace na edge serverech po celém světě. Pro FPC Atomic server to znamená:

- **Server mimo lokální síť** - Server běží v cloudu, takže nemusíte mít otevřený port ve vaší lokální síti
- **Menší zátěž pro lokální síť** - Veškerý herní provoz jde přes Fly.io, ne přes vaši domácí síť
- **Větší bezpečnost** - Nemusíte otevírat porty ve firewallu nebo routeru
- **Každý si může vytvořit svůj server** - Každý hráč si může jednoduše vytvořit vlastní server pro sebe a své kamarády
- **Automatické spouštění** - Server se nemusí spouštět ručně - spouští se automaticky, když se někdo připojí, a vypíná se po 30 sekundách nečinnosti

## 📋 Požadavky

1. **Fly.io CLI** - Command Line Interface - nainstalovaný a přihlášený
   ```bash
   # Instalace (macOS)
   curl -L https://fly.io/install.sh | sh
   
   # Instalace (Linux)
   curl -L https://fly.io/install.sh | sh
   
   # Instalace (Windows - PowerShell)
   powershell -Command "iwr https://fly.io/install.ps1 -useb | iex"
   
   # Přihlášení
   flyctl auth login
   ```

2. **Git** - pro klonování repozitáře (volitelné, pokud používáte předpřipravený image)

## 🚀 Rychlý postup

### Varianta A: Použití předpřipraveného Docker image (doporučeno)

Pokud chcete použít předpřipravený Docker image z GitHub Container Registry, nemusíte mít nainstalovaný Pascal compiler ani Lazarus:

1. **Vytvořte nový adresář a přejděte do něj:**
   ```bash
   mkdir fpc-atomic-server
   cd fpc-atomic-server
   ```

2. **Stáhněte `fly.toml` z GitHubu:**
   ```bash
   # Z nejnovějšího release
   curl -L https://github.com/PavelZverina/fpc_atomic_macos/releases/latest/download/fly.toml -o fly.toml
   
   # Nebo přímo z repozitáře
   curl -L https://raw.githubusercontent.com/PavelZverina/fpc_atomic_macos/main/flyio_server/fly.toml -o fly.toml
   ```

3. **Stáhněte `fly.toml.example` a přejmenujte ho:**
   ```bash
   curl -L https://github.com/PavelZverina/fpc_atomic_macos/releases/latest/download/fly.toml.example -o fly.toml
   ```
   
   Nebo vytvořte `fly.toml` ručně:
   ```toml
   app = "fpc-atomic-tcp-server"
   primary_region = "fra"
   
   [build]
     image = "ghcr.io/PavelZverina/fpc-atomic-server:latest"
   
   [env]
     PORT = "5521"
   
   [[services]]
     protocol = "tcp"
     internal_port = 5521
     processes = ["app"]
     auto_stop_machines = true
     auto_start_machines = true
     min_machines_running = 0
   
     [[services.ports]]
       port = 5521
   ```

4. **Deploy na Fly.io:**
   ```bash
   flyctl launch
   ```

### Varianta B: Build z source kódu

Pokud chcete buildnout z source kódu (vyžaduje Pascal compiler a Lazarus):

1. **Naklonujte repozitář:**
   ```bash
   git clone https://github.com/PavelZverina/fpc_atomic_macos.git
   cd fpc_atomic_macos/flyio_server
   ```

2. **Deploy na Fly.io:**
   ```bash
   # První deploy (vytvoří novou aplikaci)
   flyctl launch
   
   # Nebo pokud už máte aplikaci
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

