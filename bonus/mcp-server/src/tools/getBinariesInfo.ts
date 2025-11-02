import { ToolDefinition } from "../types.js";
import { getBinariesState } from "../utils/binaryManager.js";

export const getBinariesInfoTool: ToolDefinition = {
  tool: {
    name: "get_binaries_info",
    description:
      "Récupère les informations sur les binaires Ratatouille (compilateur et VM) actuellement disponibles, incluant les versions et les dates de dernière mise à jour.",
    inputSchema: {
      type: "object",
      properties: {},
    },
  },
  handler: async (args) => {
    const state = getBinariesState();

    let result = "# Informations sur les binaires Ratatouille\n\n";

    if (state.compiler) {
      result += `## Compilateur 🔨\n`;
      result += `- **Version:** ${state.compiler.version}\n`;
      result += `- **Chemin:** ${state.compiler.path}\n`;
      result += `- **Dernière mise à jour:** ${state.compiler.lastUpdate.toLocaleString()}\n\n`;
    } else {
      result += `## Compilateur 🔨\n`;
      result += `- **Status:** ❌ Non disponible\n\n`;
    }

    if (state.vm) {
      result += `## Machine Virtuelle 🖥️\n`;
      result += `- **Version:** ${state.vm.version}\n`;
      result += `- **Chemin:** ${state.vm.path}\n`;
      result += `- **Dernière mise à jour:** ${state.vm.lastUpdate.toLocaleString()}\n\n`;
    } else {
      result += `## Machine Virtuelle 🖥️\n`;
      result += `- **Status:** ⚠️  Non disponible (en attente du téléchargement ou non présente dans la release)\n\n`;
    }

    result += `---\n\n`;
    result += `**Repository:** https://github.com/vincbct34/Glados-On-Top\n`;
    result += `**Mise à jour automatique:** Toutes les 120 secondes\n`;

    return result;
  },
};
