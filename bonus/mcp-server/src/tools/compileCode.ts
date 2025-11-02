import { ToolDefinition } from "../types.js";
import { getCompilerPath, getBinariesState } from "../utils/binaryManager.js";
import { spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

export const compileCodeTool: ToolDefinition = {
  tool: {
    name: "compile_ratatouille_code",
    description:
      "Compile du code Ratatouille en utilisant le compilateur officiel. Retourne le résultat de la compilation ou les erreurs.",
    inputSchema: {
      type: "object",
      properties: {
        code: {
          type: "string",
          description: "Le code source Ratatouille à compiler",
        },
        filename: {
          type: "string",
          description:
            "Nom du fichier source (optionnel, par défaut: 'input.gld')",
        },
      },
      required: ["code"],
    },
  },
  handler: async (args) => {
    const code = args?.code as string;
    const filename = (args?.filename as string) || "input.gld";

    if (!code) {
      throw new Error("Le paramètre 'code' est requis");
    }

    const compilerPath = getCompilerPath();
    if (!compilerPath) {
      throw new Error(
        "Le compilateur n'est pas disponible. Veuillez attendre le téléchargement."
      );
    }

    if (!fs.existsSync(compilerPath)) {
      throw new Error("Le compilateur n'est pas trouvé à l'emplacement attendu");
    }

    // Créer un fichier temporaire
    const tmpDir = os.tmpdir();
    const tmpFile = path.join(tmpDir, filename);
    fs.writeFileSync(tmpFile, code);

    return new Promise((resolve, reject) => {
      const compiler = spawn(compilerPath, [tmpFile]);
      let stdout = "";
      let stderr = "";

      compiler.stdout.on("data", (data) => {
        stdout += data.toString();
      });

      compiler.stderr.on("data", (data) => {
        stderr += data.toString();
      });

      compiler.on("close", (code) => {
        // Nettoyer le fichier temporaire
        try {
          fs.unlinkSync(tmpFile);
        } catch (e) {
          // Ignorer les erreurs de nettoyage
        }

        const state = getBinariesState();
        const version = state.compiler?.version || "unknown";

        if (code === 0) {
          resolve(
            `# Compilation réussie ✅\n\n**Version du compilateur:** ${version}\n\n## Sortie:\n\`\`\`\n${stdout}\n\`\`\``
          );
        } else {
          resolve(
            `# Erreur de compilation ❌\n\n**Version du compilateur:** ${version}\n**Code de sortie:** ${code}\n\n## Erreurs:\n\`\`\`\n${stderr}\n\`\`\`\n\n## Sortie:\n\`\`\`\n${stdout}\n\`\`\``
          );
        }
      });

      compiler.on("error", (err) => {
        try {
          fs.unlinkSync(tmpFile);
        } catch (e) {
          // Ignorer
        }
        reject(new Error(`Erreur lors de l'exécution du compilateur: ${err.message}`));
      });
    });
  },
};
