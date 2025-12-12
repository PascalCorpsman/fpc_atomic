# Deploying FPC Atomic Server to Fly.io

## 🌐 What is Fly.io?

**Fly.io** is a cloud platform that allows you to run applications on edge servers around the world. For FPC Atomic server, this means:

- **Server outside local network** - Server runs in the cloud, so you don't need to open ports in your local network
- **Less load on local network** - All game traffic goes through Fly.io, not through your home network
- **Better security** - You don't need to open ports in your firewall or router
- **Everyone can create their own server** - Each player can easily create their own server for themselves and their friends
- **Automatic startup** - Server doesn't need to be started manually - it starts automatically when someone connects, and shuts down after 30 seconds of inactivity

## 📋 Requirements

1. **Fly.io Account** - Create a free account at [fly.io](https://fly.io)
   - Go to https://fly.io and register (free tier available)
   - You'll need an email address to create an account
   - Free tier includes 3 shared CPU VMs and 3GB persistent storage

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

3. **Git** - for cloning the repository (optional, if using pre-built image)

## 📋 Which deploy script to use?

There are two deploy scripts in the repository:

- **`deploy_prebuilt.sh`** (recommended) - Faster option
  - Uses pre-built Docker image from GitHub Container Registry
  - Faster (doesn't compile from source)
  - Requires game data (will fail without it)
  - Use if you have game data and want a quick deploy

- **`build_and_deploy.sh`** - Slower option
  - Builds server from Pascal source code
  - Slower (compiles during build process)
  - Can work without game data (warns but continues)
  - Use if you want to build from current source code or don't have game data

## 🚀 Quick Start

### Option A: Deploy from pre-built Docker image (recommended) - `deploy_prebuilt.sh`

**Faster option** - uses pre-built Docker image from GitHub Container Registry. You don't need to have Pascal compiler or Lazarus installed. However, you must add game data extracted from the original CD.

**⚠️ IMPORTANT: Game data is required**

The pre-built Docker image doesn't contain game data due to licensing reasons. You must extract data from the original Atomic Bomberman CD and add it to the deployment.

1. **Extract game data:**
   - Use CD Data Extractor (part of the repository) to extract data from the original CD
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

4. **Clone the repository (to get the deploy script):**
   ```bash
   git clone https://github.com/your-username/fpc_atomic.git
   cd fpc_atomic/flyio_server
   ```

5. **Copy your data directory:**
   ```bash
   # Copy extracted data to flyio_server/
   cp -r /path/to/extracted/data .
   ```

6. **Deploy to Fly.io:**
   ```bash
   # Use the deploy script, which will add data to the pre-built image
   ./deploy_prebuilt.sh
   ```

   **Alternative - Manual deployment:**
   
   If you prefer manual deployment, you can create your own Dockerfile:
   ```dockerfile
   FROM ghcr.io/your-username/fpc-atomic-server:latest
   COPY data /app/data
   ```
   
   Then create `fly.toml`:
   ```toml
   app = "your-app-name-here"  # Change this to your unique application name
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

### Option B: Build from source code and deploy - `build_and_deploy.sh`

**Slower option** - builds server from Pascal source code and then deploys it. Requires Pascal compiler (but it's already in Dockerfile, so you don't need anything installed locally). Can work even without game data (server will run, but without maps).

**⚠️ IMPORTANT: Game data is required**

Before deploying, you must extract game data from the original Atomic Bomberman CD and copy it to the `flyio_server/data` directory.

1. **Extract game data:**
   - Use CD Data Extractor (part of the repository) to extract data from the original CD
   - This will create a `data` directory with maps, resources, and sounds

2. **Copy data to flyio_server:**
   ```bash
   # Copy extracted data directory to flyio_server/
   cp -r /path/to/extracted/data flyio_server/data
   ```
   
   The `flyio_server/data` directory should contain:
   - `maps/` - game maps
   - `res/` - resources, textures, etc.
   - `sounds/` - sound effects

3. **Clone the repository:**
   ```bash
   git clone https://github.com/your-username/fpc_atomic.git
   cd fpc_atomic
   ```

4. **Set application name in .env file:**
   
   Create or edit `.env` file in the project root:
   ```bash
   # In project root (not in flyio_server/)
   echo "FLY_APP_NAME=fpc-atomic-tcp-server-my-name" >> .env
   ```
   
   **Important:** The application name is stored in the `.env` file (which is in `.gitignore`), so you can commit `fly.toml` to git without revealing your application name.

5. **Deploy to Fly.io:**
   
   **Easiest way - use the build and deploy script:**
   ```bash
   cd flyio_server
   ./build_and_deploy.sh
   ```
   
   **What this script does:**
   - Builds server from Pascal source code (compilation happens in Docker image)
   - Deploys the build to Fly.io
   - Can work without game data (warns but continues)
   
   **Or manually:**
   ```bash
   # Make sure you're in the project root
   cd /path/to/fpc_atomic
   
   # First deploy (creates new application)
   flyctl deploy --config flyio_server/fly.toml
   
   # Or if you already have an application
   flyctl deploy --config flyio_server/fly.toml
   ```
   
   **Note:** Build context must be the project root (not `flyio_server/`), so always run `flyctl deploy` from the project root with `--config flyio_server/fly.toml`.
   
   **Note:** If you don't include game data, the server will build and run, but without game maps. Only random maps will be available.

### 3. View information

```bash
# Show status and IP address
flyctl status

# Show logs
flyctl logs

# SSH into container (for debugging)
flyctl ssh console
```

## 📝 What happens during deployment?

1. **Build**: Dockerfile automatically:
   - Compiles AI library (`libai.so`) for Linux
   - Compiles server (`atomic_server`) for Linux
   - Copies data directory (contains maps and other game data)

2. **Deploy**: Fly.io:
   - Creates Docker image
   - Starts server on port 5521
   - Server automatically shuts down after 30 seconds of inactivity (auto_stop_machines)
   - Server automatically starts on new connection (auto_start_machines)

## ⚙️ Configuration

### Port
Server listens on port **5521** (set in `fly.toml` and `Dockerfile`).

### Timeout
Server automatically shuts down after **30 seconds** of inactivity (set in `Dockerfile` as `-t 30000`).

### Region
Default region is **Frankfurt (fra)** - you can change it in `fly.toml`.

## 🔧 Troubleshooting

### Build fails

1. Check that all required files are in the project:
   - `server/` - server source code
   - `units/` - units
   - `ai/` - AI library
   - `data/` - game data

2. Try local build:
   ```bash
   docker build -t fpc-atomic-test -f flyio_server/Dockerfile .
   ```

### Server doesn't start

1. Check logs:
   ```bash
   flyctl logs
   ```

2. Verify that data directory exists and contains required files (maps in `data/maps/`)

### Client connections

**⚠️ IMPORTANT: Each server has its own address**

Everyone who deploys a server on fly.io has their own unique address. If multiple people use the same application name, they will try to deploy to the same application, which won't work - everyone must have their own account and application.

**IMPORTANT:** Before deploying, change the application name in `fly.toml` to something unique, for example:
```toml
app = "fpc-atomic-tcp-server-my-name"
```

**How to find server address:**

1. **DNS address (recommended):** `<app-name>.fly.dev`
   - For example: `your-app-name-here.fly.dev`
   - This address doesn't change and is more reliable than IP address
   - You can find it from the application name in `fly.toml` or from:
     ```bash
     flyctl status
     ```

2. **IP address (alternative):**
   ```bash
   flyctl status
   ```
   - IP address may change, so it's better to use DNS name

**Connecting in client:**

1. Press **"j"** in the main menu
2. Enter server address:
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

## 🔗 Useful links

- [Fly.io documentation](https://fly.io/docs/)
- [Fly.io CLI reference](https://fly.io/docs/flyctl/)
