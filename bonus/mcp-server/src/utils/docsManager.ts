import * as fs from "fs";
import * as path from "path";
import * as https from "https";
import { fileURLToPath } from "url";
import { GithubContent, DocsState, DocInfo } from "../types.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const GITHUB_REPO = "vincbct34/Glados-On-Top";
const GITHUB_API_BASE = `https://api.github.com/repos/${GITHUB_REPO}/contents`;
const DOCS_DIR = path.join(__dirname, "..", "..", "docs");
const STATE_FILE = path.join(DOCS_DIR, "state.json");

// Intervalle de mise à jour : 120 secondes
const UPDATE_INTERVAL = 120 * 1000;

let updateTimer: NodeJS.Timeout | null = null;
let onUpdateCallback: (() => void) | null = null;

/**
 * Récupère le contenu d'un dossier depuis GitHub (récursif)
 */
async function fetchGithubContents(folderPath: string): Promise<GithubContent[]> {
  return new Promise((resolve, reject) => {
    const url = `${GITHUB_API_BASE}/${folderPath}`;
    const options = {
      headers: {
        "User-Agent": "Ratatouille-MCP-Server",
      },
    };

    https.get(url, options, (res) => {
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
 * Récupère tous les fichiers markdown récursivement
 */
async function getAllMarkdownFiles(folderPath: string = "docs"): Promise<GithubContent[]> {
  const contents = await fetchGithubContents(folderPath);
  const markdownFiles: GithubContent[] = [];

  for (const item of contents) {
    if (item.type === "file" && item.name.endsWith(".md")) {
      markdownFiles.push(item);
    } else if (item.type === "dir") {
      // Récursion pour les sous-dossiers
      const subFiles = await getAllMarkdownFiles(item.path);
      markdownFiles.push(...subFiles);
    }
  }

  return markdownFiles;
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
 * Lit l'état actuel des documents
 */
function readState(): DocsState {
  if (!fs.existsSync(STATE_FILE)) {
    return {};
  }

  try {
    const data = fs.readFileSync(STATE_FILE, "utf-8");
    const state = JSON.parse(data);

    // Reconvertir les dates
    Object.keys(state).forEach((key) => {
      if (state[key].lastUpdate) {
        state[key].lastUpdate = new Date(state[key].lastUpdate);
      }
    });

    return state;
  } catch (error) {
    console.error("Erreur lors de la lecture de l'état des docs:", error);
    return {};
  }
}

/**
 * Sauvegarde l'état des documents
 */
function saveState(state: DocsState): void {
  fs.writeFileSync(STATE_FILE, JSON.stringify(state, null, 2));
}

/**
 * Télécharge ou met à jour les documents markdown
 */
async function updateDocs(): Promise<void> {
  try {
    console.error("🔍 Vérification des mises à jour de la documentation...");

    const markdownFiles = await getAllMarkdownFiles();
    const state = readState();
    const newState: DocsState = {};
    let updatedCount = 0;
    let newCount = 0;

    for (const file of markdownFiles) {
      if (!file.download_url) {
        console.error(`⚠️  Pas d'URL de téléchargement pour ${file.name}`);
        continue;
      }

      const docName = file.name;
      const shouldUpdate = !state[docName] || state[docName].sha !== file.sha;

      if (shouldUpdate) {
        console.error(`📥 Téléchargement de ${file.path}...`);

        // Créer les sous-dossiers si nécessaire
        const relativePath = file.path.replace(/^docs\//, "");
        const localPath = path.join(DOCS_DIR, relativePath);
        const localDir = path.dirname(localPath);

        if (!fs.existsSync(localDir)) {
          fs.mkdirSync(localDir, { recursive: true });
        }

        await downloadFile(file.download_url, localPath);

        newState[docName] = {
          sha: file.sha,
          path: localPath,
          lastUpdate: new Date(),
        };

        if (state[docName]) {
          updatedCount++;
        } else {
          newCount++;
        }

        console.error(`✅ ${file.name} téléchargé avec succès`);
      } else {
        // Conserver l'état existant
        newState[docName] = state[docName];
      }
    }

    // Supprimer les fichiers qui ne sont plus sur GitHub
    for (const docName of Object.keys(state)) {
      if (!newState[docName]) {
        const filePath = state[docName].path;
        if (fs.existsSync(filePath)) {
          fs.unlinkSync(filePath);
          console.error(`🗑️  ${docName} supprimé (plus présent sur GitHub)`);
        }
      }
    }

    saveState(newState);

    if (newCount > 0 || updatedCount > 0) {
      console.error(`✅ Documentation mise à jour : ${newCount} nouveaux, ${updatedCount} mis à jour`);

      // Appeler le callback de mise à jour si défini
      if (onUpdateCallback) {
        onUpdateCallback();
      }
    } else {
      console.error("✓ La documentation est à jour");
    }
  } catch (error) {
    console.error("❌ Erreur lors de la mise à jour de la documentation:", error);
  }
}

/**
 * Initialise le système de mise à jour des documents
 */
export async function initDocsManager(onUpdate?: () => void): Promise<void> {
  // Enregistrer le callback de mise à jour
  if (onUpdate) {
    onUpdateCallback = onUpdate;
  }

  // Créer le dossier des docs s'il n'existe pas
  if (!fs.existsSync(DOCS_DIR)) {
    fs.mkdirSync(DOCS_DIR, { recursive: true });
  }

  // Première mise à jour au démarrage
  await updateDocs();

  // Planifier les mises à jour périodiques
  updateTimer = setInterval(() => {
    updateDocs();
  }, UPDATE_INTERVAL);

  console.error(`⏰ Mises à jour automatiques de la documentation configurées (intervalle: ${UPDATE_INTERVAL / 1000}s)`);
}

/**
 * Arrête le gestionnaire de documentation
 */
export function stopDocsManager(): void {
  if (updateTimer) {
    clearInterval(updateTimer);
    updateTimer = null;
    console.error("🛑 Mises à jour automatiques de la documentation arrêtées");
  }
}

/**
 * Récupère l'état actuel des documents
 */
export function getDocsState(): DocsState {
  return readState();
}

/**
 * Récupère la liste des documents disponibles
 */
export function getAvailableDocs(): string[] {
  const state = readState();
  return Object.keys(state);
}

/**
 * Force une mise à jour immédiate de la documentation
 */
export async function forceUpdateDocs(): Promise<void> {
  await updateDocs();
}
