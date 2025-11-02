import * as fs from "fs";
import * as path from "path";
import { ToolDefinition } from "../types.js";
import { getDocsState } from "../utils/docsManager.js";
import { searchInText } from "../utils/fileReader.js";

/**
 * Génère dynamiquement les tools pour tous les fichiers markdown disponibles
 */
export function generateDocTools(): ToolDefinition[] {
  const docsState = getDocsState();
  const tools: ToolDefinition[] = [];

  for (const [docName, docInfo] of Object.entries(docsState)) {
    // Retirer l'extension .md et convertir en minuscules
    const baseName = docName.replace(/\.md$/i, "").toLowerCase();
    const toolName = `get_ratatouille_${baseName}`;

    // Créer une description lisible à partir du nom du fichier
    const readableName = docName.replace(/\.md$/i, "").replace(/[-_]/g, " ");

    const tool: ToolDefinition = {
      tool: {
        name: toolName,
        description: `Récupère la documentation ${readableName} du projet Ratatouille. Ce fichier contient des informations détaillées sur ${readableName}.`,
        inputSchema: {
          type: "object",
          properties: {
            topic: {
              type: "string",
              description: "Sujet spécifique à rechercher dans ce document (optionnel).",
            },
          },
        },
      },
      handler: async (args) => {
        try {
          const content = fs.readFileSync(docInfo.path, "utf-8");
          const topic = args?.topic as string | undefined;

          if (topic) {
            const filtered = searchInText(content, topic);
            return `# ${readableName} (filtré pour: ${topic})\n\n${filtered}`;
          }

          return content;
        } catch (error) {
          return `Erreur lors de la lecture du document ${docName}: ${error}`;
        }
      },
    };

    tools.push(tool);
  }

  return tools;
}
