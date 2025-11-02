import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import { tools, toolHandlers, regenerateTools } from "./tools/index.js";
import { initBinariesManager, stopBinariesManager } from "./utils/binaryManager.js";
import { initDocsManager, stopDocsManager } from "./utils/docsManager.js";

// Configuration du serveur MCP
const server = new Server(
  {
    name: "ratatouille-mcp-server",
    version: "1.0.0",
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Handler pour lister les outils
server.setRequestHandler(ListToolsRequestSchema, async () => {
  return { tools };
});

// Handler pour appeler les outils
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;

  try {
    const handler = toolHandlers.get(name);
    
    if (!handler) {
      throw new Error(`Outil inconnu: ${name}`);
    }

    const result = await handler(args || {});

    return {
      content: [
        {
          type: "text",
          text: result,
        },
      ],
    };
  } catch (error) {
    return {
      content: [
        {
          type: "text",
          text: `Erreur: ${error}`,
        },
      ],
      isError: true,
    };
  }
});

// Démarrage du serveur
async function main() {
  // Initialiser le gestionnaire de documentation
  console.error("📚 Initialisation du gestionnaire de documentation...");
  await initDocsManager(() => {
    // Régénérer les tools après chaque mise à jour de la documentation
    console.error("🔄 Régénération des tools suite à la mise à jour de la documentation...");
    regenerateTools();
  });

  // Régénérer les tools après le téléchargement initial des docs
  regenerateTools();

  // Initialiser le gestionnaire de binaires
  console.error("🚀 Initialisation du gestionnaire de binaires...");
  await initBinariesManager();

  // Démarrer le serveur MCP
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("✅ Serveur MCP Ratatouille démarré avec succès");

  // Gérer l'arrêt propre
  process.on("SIGINT", () => {
    console.error("\n🛑 Arrêt du serveur...");
    stopDocsManager();
    stopBinariesManager();
    process.exit(0);
  });

  process.on("SIGTERM", () => {
    console.error("\n🛑 Arrêt du serveur...");
    stopDocsManager();
    stopBinariesManager();
    process.exit(0);
  });
}

main().catch((error) => {
  console.error("❌ Erreur fatale:", error);
  stopDocsManager();
  stopBinariesManager();
  process.exit(1);
});
