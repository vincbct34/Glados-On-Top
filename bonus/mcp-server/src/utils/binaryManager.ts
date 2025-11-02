import * as fs from "fs";
import * as path from "path";
import * as https from "https";
import { fileURLToPath } from "url";
import { GithubRelease, GithubAsset, BinariesState, BinaryInfo } from "../types.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const GITHUB_REPO = "vincbct34/Glados-On-Top";
const GITHUB_API_URL = `https://api.github.com/repos/${GITHUB_REPO}/releases/latest`;
const BINARIES_DIR = path.join(__dirname, "..", "..", "binaries");
const STATE_FILE = path.join(BINARIES_DIR, "state.json");

// Intervalle de mise à jour : 1 minute
const UPDATE_INTERVAL = 120 * 1000;

let updateTimer: NodeJS.Timeout | null = null;

/**
 * Récupère la dernière release depuis GitHub
 */
async function fetchLatestRelease(): Promise<GithubRelease> {
  return new Promise((resolve, reject) => {
    const options = {
      headers: {
        "User-Agent": "Ratatouille-MCP-Server",
      },
    };

    https.get(GITHUB_API_URL, options, (res) => {
      let data = "";

      res.on("data", (chunk) => {
        data += chunk;
      });

      res.on("end", () => {
        if (res.statusCode === 200) {
          resolve(JSON.parse(data));
        } else {
          reject(new Error(`GitHub API returned status ${res.statusCode}: ${data}`));
        }
      });
    }).on("error", reject);
  });
}

/**
 * Télécharge un fichier depuis une URL
 */
async function downloadFile(url: string, destination: string): Promise<void> {
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(destination);

    https.get(url, (response) => {
      if (response.statusCode === 302 || response.statusCode === 301) {
        // Redirection
        const redirectUrl = response.headers.location;
        if (redirectUrl) {
          https.get(redirectUrl, (redirectResponse) => {
            redirectResponse.pipe(file);
            file.on("finish", () => {
              file.close();
              // Rendre le fichier exécutable
              fs.chmodSync(destination, 0o755);
              resolve();
            });
          }).on("error", (err) => {
            fs.unlinkSync(destination);
            reject(err);
          });
        }
      } else {
        response.pipe(file);
        file.on("finish", () => {
          file.close();
          // Rendre le fichier exécutable
          fs.chmodSync(destination, 0o755);
          resolve();
        });
      }
    }).on("error", (err) => {
      fs.unlinkSync(destination);
      reject(err);
    });
  });
}

/**
 * Lit l'état actuel des binaires
 */
function readState(): BinariesState {
  if (!fs.existsSync(STATE_FILE)) {
    return {};
  }

  try {
    const data = fs.readFileSync(STATE_FILE, "utf-8");
    const state = JSON.parse(data);
    
    // Reconvertir les dates
    if (state.compiler?.lastUpdate) {
      state.compiler.lastUpdate = new Date(state.compiler.lastUpdate);
    }
    if (state.vm?.lastUpdate) {
      state.vm.lastUpdate = new Date(state.vm.lastUpdate);
    }
    
    return state;
  } catch (error) {
    console.error("Erreur lors de la lecture de l'état:", error);
    return {};
  }
}

/**
 * Sauvegarde l'état des binaires
 */
function saveState(state: BinariesState): void {
  fs.writeFileSync(STATE_FILE, JSON.stringify(state, null, 2));
}

/**
 * Trouve un asset par nom (pattern matching)
 */
function findAsset(assets: GithubAsset[], pattern: RegExp): GithubAsset | undefined {
  return assets.find((asset) => pattern.test(asset.name));
}

/**
 * Télécharge ou met à jour les binaires
 */
async function updateBinaries(): Promise<void> {
  try {
    console.error("🔍 Vérification des mises à jour...");
    
    const release = await fetchLatestRelease();
    const state = readState();
    const newState: BinariesState = { ...state };
    
    // Déterminer le système d'exploitation pour choisir le bon binaire
    const platform = process.platform;
    let compilerPattern: RegExp;
    let vmPattern: RegExp;
    
    // Patterns plus flexibles pour matcher différents noms de binaires
    if (platform === "darwin") {
      // macOS
      compilerPattern = /^glados(?!.*vm).*mac.*|^glados(?!.*vm).*darwin.*|^glados$/i;
      vmPattern = /vm.*mac.*|vm.*darwin.*|glados.*vm/i;
    } else if (platform === "linux") {
      compilerPattern = /^glados(?!.*vm).*linux.*|^glados$/i;
      vmPattern = /vm.*linux.*|glados.*vm/i;
    } else if (platform === "win32") {
      compilerPattern = /^glados(?!.*vm).*win.*|^glados(?!.*vm).*\.exe$|^glados\.exe$/i;
      vmPattern = /vm.*win.*|vm.*\.exe$|glados.*vm/i;
    } else {
      // Par défaut, essayer de trouver n'importe quel binaire
      compilerPattern = /^glados$/i;
      vmPattern = /vm/i;
    }

    // Chercher le compilateur
    const compilerAsset = findAsset(release.assets, compilerPattern);
    if (compilerAsset) {
      const shouldUpdate = !state.compiler || state.compiler.version !== release.tag_name;
      
      if (shouldUpdate) {
        console.error(`📥 Téléchargement du compilateur ${release.tag_name}...`);
        const compilerPath = path.join(BINARIES_DIR, "glados-compiler");
        await downloadFile(compilerAsset.browser_download_url, compilerPath);
        
        newState.compiler = {
          version: release.tag_name,
          path: compilerPath,
          lastUpdate: new Date(),
        };
        
        console.error(`✅ Compilateur ${release.tag_name} téléchargé avec succès`);
      }
    }

    // Chercher la VM
    const vmAsset = findAsset(release.assets, vmPattern);
    if (vmAsset) {
      const shouldUpdate = !state.vm || state.vm.version !== release.tag_name;
      
      if (shouldUpdate) {
        console.error(`📥 Téléchargement de la VM ${release.tag_name}...`);
        const vmPath = path.join(BINARIES_DIR, "glados-vm");
        await downloadFile(vmAsset.browser_download_url, vmPath);
        
        newState.vm = {
          version: release.tag_name,
          path: vmPath,
          lastUpdate: new Date(),
        };
        
        console.error(`✅ VM ${release.tag_name} téléchargée avec succès`);
      }
    }

    saveState(newState);
    
    if (!compilerAsset && !vmAsset) {
      console.error("⚠️  Aucun binaire compatible trouvé dans la release");
    } else if (state.compiler?.version === release.tag_name && state.vm?.version === release.tag_name) {
      console.error("✓ Les binaires sont à jour");
    }
  } catch (error) {
    console.error("❌ Erreur lors de la mise à jour des binaires:", error);
  }
}

/**
 * Initialise le système de mise à jour des binaires
 */
export async function initBinariesManager(): Promise<void> {
  // Créer le dossier des binaires s'il n'existe pas
  if (!fs.existsSync(BINARIES_DIR)) {
    fs.mkdirSync(BINARIES_DIR, { recursive: true });
  }

  // Première mise à jour au démarrage
  await updateBinaries();

  // Planifier les mises à jour périodiques
  updateTimer = setInterval(() => {
    updateBinaries();
  }, UPDATE_INTERVAL);

  console.error(`⏰ Mises à jour automatiques configurées (intervalle: ${UPDATE_INTERVAL / 1000}s)`);
}

/**
 * Arrête le gestionnaire de binaires
 */
export function stopBinariesManager(): void {
  if (updateTimer) {
    clearInterval(updateTimer);
    updateTimer = null;
    console.error("🛑 Mises à jour automatiques arrêtées");
  }
}

/**
 * Récupère l'état actuel des binaires
 */
export function getBinariesState(): BinariesState {
  return readState();
}

/**
 * Récupère le chemin du compilateur
 */
export function getCompilerPath(): string | undefined {
  const state = readState();
  return state.compiler?.path;
}

/**
 * Récupère le chemin de la VM
 */
export function getVMPath(): string | undefined {
  const state = readState();
  return state.vm?.path;
}
