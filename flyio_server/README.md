# Deploy FPC Atomic Server to Fly.io

## 🌐 What is Fly.io?

**Fly.io** is a cloud platform that allows you to run applications on edge servers around the world. For the FPC Atomic server, this means:

- **Server outside your local network** - The server runs in the cloud, so you don't need to open ports in your local network
- **Less load on your local network** - All game traffic goes through Fly.io, not through your home network
- **Better security** - You don't need to open ports in your firewall or router
- **Everyone can create their own server** - Each player can easily create their own server for themselves and their friends
- **Automatic startup** - The server doesn't need to be started manually - it starts automatically when someone connects and shuts down after 30 seconds of inactivity

## 📋 Requirements

1. **Fly.io Account** - Create a free account at [fly.io](https://fly.io)
   - Go to https://fly.io and sign up (free tier available)
   - You'll need an email address to create an account
   - Free tier includes 3 shared-cpu VMs and 3GB persistent storage

2. **Fly.io CLI** - Command Line Interface - installed and logged in
   ```bash
   # Installation (macOS)
   curl -L https://fly.io/install.sh | sh
   
   # Installation (Linux)
   curl -L https://fly.io/install.sh | sh
   
   # Installation (Windows - PowerShell)
   powershell -Command "iwr https://fly.io/install.ps1 -useb | iex"
   
   # Login (requires Fly.io account)
   flyctl auth login
   ```

2. **Git** - for cloning the repository (optional, if using pre-built image)

## 🚀 Quick Start

### Option A: Using Pre-built Docker Image with Game Data (Recommended)

If you want to use a pre-built Docker image from GitHub Container Registry, you don't need to have Pascal compiler or Lazarus installed. However, you need to add game data extracted from the original CD.

**⚠️ IMPORTANT: Game Data Required**

The pre-built Docker image does not include game data due to licensing. You must extract data from the original Atomic Bomberman CD and add it to your deployment.

1. **Extract game data:**
   - Use the CD Data Extractor tool (included in the repository) to extract data from the original game CD
   - This will create a `data` directory with maps, resources, and sounds

2. **Create deployment directory:**
   ```bash
   mkdir fpc-atomic-server
   cd fpc-atomic-server
   ```

3. **Copy game data:**
   ```bash
   # Copy the extracted data directory
   cp -r /path/to/extracted/data .
   ```
   
   The `data` directory should contain:
   - `maps/` - game maps
   - `res/` - resources, textures, etc.
   - `sounds/` - sound effects

4. **Clone the repository (to get deploy script):**
   ```bash
   git clone https://github.com/PavelZverina/fpc_atomic_macos.git
   cd fpc_atomic/flyio_server
   ```

5. **Copy your data directory:**
   ```bash
   # Copy your extracted data to flyio_server/
   cp -r /path/to/extracted/data .
   ```

6. **Deploy to Fly.io:**
   ```bash
   # Use the deploy script that adds data to the pre-built image
   ./deploy_with_data.sh
   ```

   **Alternative - Manual deployment:**
   
   If you prefer to deploy manually, you can create a custom Dockerfile:
   ```dockerfile
   FROM ghcr.io/PavelZverina/fpc-atomic-server:latest
   COPY data /app/data
   ```
   
   Then create `fly.toml`:
   ```toml
   app = "your-app-name-here"  # Change this to your unique app name
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
   
   Then deploy:
   ```bash
   flyctl deploy
   ```

### Option B: Build from Source Code

If you want to build from source code (requires Pascal compiler and Lazarus):

**⚠️ IMPORTANT: Game Data Required**

Before deploying, you must extract game data from the original Atomic Bomberman CD and copy it to the `flyio_server/data` directory.

1. **Extract game data:**
   - Use the CD Data Extractor tool (included in the repository) to extract data from the original game CD
   - This will create a `data` directory with maps, resources, and sounds

2. **Copy data to flyio_server:**
   ```bash
   # Copy the extracted data directory to flyio_server/
   cp -r /path/to/extracted/data flyio_server/data
   ```
   
   The `flyio_server/data` directory should contain:
   - `maps/` - game maps
   - `res/` - resources, textures, etc.
   - `sounds/` - sound effects

3. **Clone the repository:**
   ```bash
   git clone https://github.com/PavelZverina/fpc_atomic_macos.git
   cd fpc_atomic
   ```

4. **Deploy to Fly.io:**
   
   **Easy way - use the deploy script:**
   ```bash
   cd flyio_server
   ./deploy_to_flyio.sh
   ```
   
   **Or manually:**
   ```bash
   # Make sure you're in the project root
   cd /path/to/fpc_atomic
   
   # First deploy (creates new application)
   flyctl deploy --config flyio_server/fly.toml
   
   # Or if you already have an application
   flyctl deploy --config flyio_server/fly.toml
   ```
   
   **Note:** The build context must be the project root (not `flyio_server/`), so always run `flyctl deploy` from the project root with `--config flyio_server/fly.toml`.
   
   **Note:** If you don't include game data, the server will build and run, but without game maps. Only random maps will be available.

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

**⚠️ IMPORTANT: Each server has its own unique address**

Each person who deploys a server on fly.io has their own unique address. If multiple people use the same app name, they will try to deploy to the same application, which won't work - each person must have their own account and application.

**IMPORTANT:** Before deploying, change the app name in `fly.toml` to something unique, for example:
```toml
app = "fpc-atomic-tcp-server-my-name"
```

**How to find the server address:**

1. **DNS address (recommended):** `<app-name>.fly.dev`
   - For example: `your-app-name-here.fly.dev`
   - This address doesn't change and is more reliable than an IP address
   - You can find it from the app name in `fly.toml` or from:
     ```bash
     flyctl status
     ```

2. **IP address (alternative):**
   ```bash
   flyctl status
   ```
   - IP address may change, so it's better to use DNS name

**Connecting in the client:**

1. Press **"j"** in the main menu
2. Enter the server address:
   - **DNS address:** `<app-name>.fly.dev` (e.g., `your-app-name-here.fly.dev`)
   - **Or IP address:** from `flyctl status`
3. Enter port: **5521**
4. Press **OK**

**Alternative - Command line parameters:**
```bash
./fpc_atomic -ip your-app-name-here.fly.dev -port 5521
```

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
