import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Chemin vers les fichiers de documentation
export const DOCS_PATH = path.join(__dirname, "..", "..", "docs");

/**
 * Lit un fichier de documentation
 */
export function readDocFile(filename: string): string {
  try {
    const filePath = path.join(DOCS_PATH, filename);
    return fs.readFileSync(filePath, "utf-8");
  } catch (error) {
    return `Erreur lors de la lecture du fichier ${filename}: ${error}`;
  }
}

/**
 * Recherche un terme dans un texte et retourne les lignes correspondantes avec contexte
 */
export function searchInText(text: string, query: string): string {
  const lines = text.split("\n");
  const results: string[] = [];
  const queryLower = query.toLowerCase();

  lines.forEach((line, index) => {
    if (line.toLowerCase().includes(queryLower)) {
      // Ajouter le contexte (ligne précédente et suivante si disponibles)
      const start = Math.max(0, index - 1);
      const end = Math.min(lines.length, index + 2);
      const context = lines.slice(start, end).join("\n");
      results.push(`Ligne ${index + 1}:\n${context}\n`);
    }
  });

  return results.length > 0
    ? results.join("\n---\n")
    : `Aucun résultat trouvé pour "${query}"`;
}
