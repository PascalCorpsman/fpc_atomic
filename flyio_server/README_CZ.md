# Deployment FPC Atomic Server na Fly.io

## 🌐 Co je Fly.io?

**Fly.io** je cloudová platforma, která umožňuje spouštět aplikace na edge serverech po celém světě. Pro FPC Atomic server to znamená:

- **Server mimo lokální síť** - Server běží v cloudu, takže nemusíte mít otevřený port ve vaší lokální síti
- **Menší zátěž pro lokální síť** - Veškerý herní provoz jde přes Fly.io, ne přes vaši domácí síť
- **Větší bezpečnost** - Nemusíte otevírat porty ve firewallu nebo routeru
- **Každý si může vytvořit svůj server** - Každý hráč si může jednoduše vytvořit vlastní server pro sebe a své kamarády
- **Automatické spouštění** - Server se nemusí spouštět ručně - spouští se automaticky, když se někdo připojí, a vypíná se po 30 sekundách nečinnosti

## 📋 Požadavky

1. **Fly.io Účet** - Vytvořte si bezplatný účet na [fly.io](https://fly.io)
   - Přejděte na https://fly.io a zaregistrujte se (dostupný bezplatný tarif)
   - K vytvoření účtu budete potřebovat emailovou adresu
   - Bezplatný tarif zahrnuje 3 sdílené CPU VM a 3GB trvalého úložiště

2. **Fly.io CLI** - Command Line Interface - nainstalovaný a přihlášený
   ```bash
   # Instalace (macOS)
   curl -L https://fly.io/install.sh | sh
   
   # Instalace (Linux)
   curl -L https://fly.io/install.sh | sh
   
   # Instalace (Windows - PowerShell)
   powershell -Command "iwr https://fly.io/install.ps1 -useb | iex"
   
   # Přihlášení (vyžaduje Fly.io účet)
   flyctl auth login
   ```

2. **Git** - pro klonování repozitáře (volitelné, pokud používáte předpřipravený image)

## 🚀 Rychlý postup

### Varianta A: Použití předpřipraveného Docker image s herními daty (doporučeno)

Pokud chcete použít předpřipravený Docker image z GitHub Container Registry, nemusíte mít nainstalovaný Pascal compiler ani Lazarus. Musíte ale přidat herní data extrahovaná z originálního CD.

**⚠️ DŮLEŽITÉ: Herní data jsou vyžadována**

Předpřipravený Docker image neobsahuje herní data kvůli licenčním důvodům. Musíte extrahovat data z originálního CD Atomic Bomberman a přidat je k deploymentu.

1. **Extrahujte herní data:**
   - Použijte CD Data Extractor (součást repozitáře) k extrakci dat z originálního CD
   - Tím se vytvoří adresář `data` s mapami, zdroji a zvuky

2. **Vytvořte adresář pro deployment:**
   ```bash
   mkdir fpc-atomic-server
   cd fpc-atomic-server
   ```

3. **Zkopírujte herní data:**
   ```bash
   # Zkopírujte extrahovaný adresář data
   cp -r /cesta/k/extrahovanym/datam .
   ```
   
   Adresář `data` by měl obsahovat:
   - `maps/` - herní mapy
   - `res/` - zdroje, textury, atd.
   - `sounds/` - zvukové efekty

4. **Naklonujte repozitář (pro získání deploy scriptu):**
   ```bash
   git clone https://github.com/PavelZverina/fpc_atomic_macos.git
   cd fpc_atomic/flyio_server
   ```

5. **Zkopírujte svůj adresář data:**
   ```bash
   # Zkopírujte extrahovaná data do flyio_server/
   cp -r /cesta/k/extrahovanym/datam .
   ```

6. **Deploy na Fly.io:**
   ```bash
   # Použijte deploy script, který přidá data k předpřipravenému image
   ./deploy_with_data.sh
   ```

   **Alternativa - Ruční deployment:**
   
   Pokud preferujete ruční deployment, můžete vytvořit vlastní Dockerfile:
   ```dockerfile
   FROM ghcr.io/PavelZverina/fpc-atomic-server:latest
   COPY data /app/data
   ```
   
   Pak vytvořte `fly.toml`:
   ```toml
   app = "fpc-atomic-tcp-server"
   primary_region = "fra"
   
   [build]
     dockerfile = "Dockerfile"
   
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
   
   Pak deployujte:
   ```bash
   flyctl deploy
   ```

### Varianta B: Build z source kódu

Pokud chcete buildnout z source kódu (vyžaduje Pascal compiler a Lazarus):

**⚠️ DŮLEŽITÉ: Herní data jsou vyžadována**

Před deployem musíte extrahovat herní data z originálního CD Atomic Bomberman a zkopírovat je do adresáře `flyio_server/data`.

1. **Extrahujte herní data:**
   - Použijte CD Data Extractor (součást repozitáře) k extrakci dat z originálního CD
   - Tím se vytvoří adresář `data` s mapami, zdroji a zvuky

2. **Zkopírujte data do flyio_server:**
   ```bash
   # Zkopírujte extrahovaný adresář data do flyio_server/
   cp -r /cesta/k/extrahovanym/datam flyio_server/data
   ```
   
   Adresář `flyio_server/data` by měl obsahovat:
   - `maps/` - herní mapy
   - `res/` - zdroje, textury, atd.
   - `sounds/` - zvukové efekty

3. **Naklonujte repozitář:**
   ```bash
   git clone https://github.com/PavelZverina/fpc_atomic_macos.git
   cd fpc_atomic
   ```

4. **Deploy na Fly.io:**
   
   **Nejjednodušší způsob - použijte deploy script:**
   ```bash
   cd flyio_server
   ./deploy_to_flyio.sh
   ```
   
   **Nebo ručně:**
   ```bash
   # Ujistěte se, že jste v rootu projektu
   cd /cesta/k/fpc_atomic
   
   # První deploy (vytvoří novou aplikaci)
   flyctl deploy --config flyio_server/fly.toml
   
   # Nebo pokud už máte aplikaci
   flyctl deploy --config flyio_server/fly.toml
   ```
   
   **Poznámka:** Build context musí být root projektu (ne `flyio_server/`), takže vždy spouštějte `flyctl deploy` z rootu projektu s `--config flyio_server/fly.toml`.
   
   **Poznámka:** Pokud nezahrnete herní data, server se zbuildí a poběží, ale bez herních map. Budou dostupné pouze náhodné mapy.

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

