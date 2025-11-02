import * as fs from "fs";
import { ToolDefinition } from "../types.js";
import { searchInText } from "../utils/fileReader.js";
import { getDocsState } from "../utils/docsManager.js";

export const searchDocsTool: ToolDefinition = {
  tool: {
    name: "search_ratatouille_docs",
    description:
      "Recherche dans toute la documentation Ratatouille un terme ou concept spécifique. Retourne les sections pertinentes de tous les documents disponibles.",
    inputSchema: {
      type: "object",
      properties: {
        query: {
          type: "string",
          description: "Terme ou concept à rechercher dans la documentation",
        },
      },
      required: ["query"],
    },
  },
  handler: async (args) => {
    const query = args?.query as string;
    if (!query) {
      throw new Error("Le paramètre 'query' est requis");
    }

    const docsState = getDocsState();
    const results: Array<{ file: string; content: string }> = [];

    // Rechercher dans tous les documents disponibles
    for (const [docName, docInfo] of Object.entries(docsState)) {
      try {
        const content = fs.readFileSync(docInfo.path, "utf-8");
        const searchResult = searchInText(content, query);

        results.push({
          file: docName,
          content: searchResult,
        });
      } catch (error) {
        console.error(`Erreur lors de la lecture de ${docName}:`, error);
      }
    }

    const formattedResults = results
      .filter((r) => !r.content.includes("Aucun résultat"))
      .map((r) => `## ${r.file}\n\n${r.content}`)
      .join("\n\n");

    return (
      formattedResults ||
      `Aucun résultat trouvé pour "${query}" dans la documentation Ratatouille`
    );
  },
};
