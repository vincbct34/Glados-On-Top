import { ToolDefinition } from "../types.js";
import { generateDocTools } from "./dynamicDocsTools.js";
import { getDocsInfoTool } from "./getDocsInfo.js";
import { searchDocsTool } from "./searchDocs.js";
import { compileCodeTool } from "./compileCode.js";
import { runCodeTool } from "./runCode.js";
import { getBinariesInfoTool } from "./getBinariesInfo.js";

/**
 * Génère la liste de tous les outils disponibles dans le serveur MCP
 */
function generateToolDefinitions(): ToolDefinition[] {
  const dynamicDocTools = generateDocTools();

  return [
    // Documentation - générée dynamiquement
    ...dynamicDocTools,
    getDocsInfoTool,
    searchDocsTool,
    // Compilation et exécution
    compileCodeTool,
    runCodeTool,
    getBinariesInfoTool,
  ];
}

/**
 * Liste de tous les outils disponibles dans le serveur MCP
 */
export let toolDefinitions: ToolDefinition[] = generateToolDefinitions();

/**
 * Map des handlers par nom d'outil pour un accès rapide
 */
export let toolHandlers = new Map(
  toolDefinitions.map((def) => [def.tool.name, def.handler])
);

/**
 * Liste des outils (sans les handlers) pour l'exposition via MCP
 */
export let tools = toolDefinitions.map((def) => def.tool);

/**
 * Régénère les outils (à appeler après une mise à jour de la documentation)
 */
export function regenerateTools(): void {
  toolDefinitions = generateToolDefinitions();
  toolHandlers = new Map(
    toolDefinitions.map((def) => [def.tool.name, def.handler])
  );
  tools = toolDefinitions.map((def) => def.tool);
}
