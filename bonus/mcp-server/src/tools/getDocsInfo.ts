import { ToolDefinition } from "../types.js";
import { getDocsState } from "../utils/docsManager.js";

export const getDocsInfoTool: ToolDefinition = {
  tool: {
    name: "get_ratatouille_docs_info",
    description:
      "Récupère la liste de tous les documents Ratatouille disponibles avec leurs métadonnées (version SHA, date de dernière mise à jour, chemin). Utilisez cet outil pour voir quels documents sont disponibles et leur état.",
    inputSchema: {
      type: "object",
      properties: {},
    },
  },
  handler: async () => {
    const docsState = getDocsState();
    const docsList = Object.entries(docsState);

    if (docsList.length === 0) {
      return "Aucun document disponible. Le système est peut-être en cours d'initialisation.";
    }

    let result = "# Documentation Ratatouille disponible\n\n";
    result += `**Nombre total de documents:** ${docsList.length}\n\n`;
    result += "## Documents\n\n";

    for (const [docName, docInfo] of docsList) {
      result += `### ${docName}\n`;
      result += `- **SHA:** ${docInfo.sha}\n`;
      result += `- **Chemin local:** ${docInfo.path}\n`;
      result += `- **Dernière mise à jour:** ${docInfo.lastUpdate.toISOString()}\n`;
      result += `- **Tool MCP:** \`get_ratatouille_${docName.replace(/\.md$/i, "").toLowerCase()}\`\n\n`;
    }

    result += "\n## Utilisation\n\n";
    result += "Pour consulter un document, utilisez le tool correspondant avec son nom.\n";
    result += "Exemple: `get_ratatouille_architecture` pour lire ARCHITECTURE.md\n";

    return result;
  },
};
