# Deploy FPC Atomic Server to Fly.io

## 🌐 What is Fly.io?

**Fly.io** is a cloud platform that allows you to run applications on edge servers around the world. For the FPC Atomic server, this means:

- **Server outside your local network** - The server runs in the cloud, so you don't need to open ports in your local network
- **Less load on your local network** - All game traffic goes through Fly.io, not through your home network
- **Better security** - You don't need to open ports in your firewall or router
- **Everyone can create their own server** - Each player can easily create their own server for themselves and their friends
- **Automatic startup** - The server doesn't need to be started manually - it starts automatically when someone connects and shuts down after 30 seconds of inactivity

## 📋 Requirements

1. **Fly.io CLI** - Command Line Interface - installed and logged in
   ```bash
   # Installation (macOS)
   curl -L https://fly.io/install.sh | sh
   
   # Installation (Linux)
   curl -L https://fly.io/install.sh | sh
   
   # Installation (Windows - PowerShell)
   powershell -Command "iwr https://fly.io/install.ps1 -useb | iex"
   
   # Login
   flyctl auth login
   ```

2. **Git** - for cloning the repository (optional, if using pre-built image)

## 🚀 Quick Start

### Option A: Using Pre-built Docker Image (Recommended)

If you want to use a pre-built Docker image from GitHub Container Registry, you don't need to have Pascal compiler or Lazarus installed:

1. **Create a new directory and navigate to it:**
   ```bash
   mkdir fpc-atomic-server
   cd fpc-atomic-server
   ```

2. **Download `fly.toml.example` from GitHub:**
   ```bash
   # From the latest release
   curl -L https://github.com/PavelZverina/fpc_atomic_macos/releases/latest/download/fly.toml.example -o fly.toml
   ```
   
   Or create `fly.toml` manually:
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

3. **Deploy to Fly.io:**
   ```bash
   flyctl launch
   ```

### Option B: Build from Source Code

If you want to build from source code (requires Pascal compiler and Lazarus):

1. **Clone the repository:**
   ```bash
   git clone https://github.com/PavelZverina/fpc_atomic_macos.git
   cd fpc_atomic_macos/flyio_server
   ```

2. **Deploy to Fly.io:**
   ```bash
   # First deploy (creates new application)
   flyctl launch
   
   # Or if you already have an application
   flyctl deploy
   ```

### 3. View Information

```bash
# Show status and IP address
flyctl status

# Show logs
flyctl logs

# SSH into container (for debugging)
flyctl ssh console
```

## 📝 What Happens During Deploy?

1. **Build**: The Dockerfile automatically:
   - Compiles the AI library (`libai.so`) for Linux
   - Compiles the server (`atomic_server`) for Linux
   - Copies the data directory (contains maps and other game data)

2. **Deploy**: Fly.io:
   - Creates a Docker image
   - Starts the server on port 5521
   - Server automatically shuts down after 30 seconds of inactivity (auto_stop_machines)
   - Server automatically starts when a new connection arrives (auto_start_machines)

## ⚙️ Configuration

### Port
The server listens on port **5521** (set in `fly.toml` and `Dockerfile`).

### Timeout
The server automatically shuts down after **30 seconds** of inactivity (set in `Dockerfile` as `-t 30000`).

### Region
Default region is **Frankfurt (fra)** - you can change it in `fly.toml`.

## 🔧 Troubleshooting

### Build Fails

1. Check that all required files are in the project:
   - `server/` - server source code
   - `units/` - units
   - `macos/third_party/lnet_src/` - LNet library
   - `macos/app_arm64/data` or `data/` - game data
   - `ai/` - AI library

2. Try local build:
   ```bash
   docker build -t fpc-atomic-test -f flyio_server/Dockerfile .
   ```

### Server Won't Start

1. Check logs:
   ```bash
   flyctl logs
   ```

2. Verify that the data directory exists and contains required files (maps in `data/maps/`)

### Client Connection

1. Get the server IP address:
   ```bash
   flyctl status
   ```

2. Use the IP address and port **5521** in the client

## 📊 Monitoring

```bash
# Real-time logs
flyctl logs

# Application status
flyctl status

# Metrics
flyctl metrics
```

## 🔗 Useful Links

- [Fly.io Documentation](https://fly.io/docs/)
- [Fly.io CLI Reference](https://fly.io/docs/flyctl/)
