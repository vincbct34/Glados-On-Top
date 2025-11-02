import { ToolDefinition } from "../types.js";
import { getVMPath, getBinariesState } from "../utils/binaryManager.js";
import { spawn } from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

export const runCodeTool: ToolDefinition = {
  tool: {
    name: "run_ratatouille_code",
    description:
      "Exécute du bytecode Ratatouille en utilisant la machine virtuelle officielle. Retourne le résultat de l'exécution.",
    inputSchema: {
      type: "object",
      properties: {
        bytecode: {
          type: "string",
          description:
            "Le bytecode Ratatouille à exécuter (généralement obtenu via compile_ratatouille_code)",
        },
        filename: {
          type: "string",
          description:
            "Nom du fichier bytecode (optionnel, par défaut: 'program.glb')",
        },
        timeout: {
          type: "number",
          description:
            "Timeout en millisecondes pour l'exécution (par défaut: 5000ms)",
        },
      },
      required: ["bytecode"],
    },
  },
  handler: async (args) => {
    const bytecode = args?.bytecode as string;
    const filename = (args?.filename as string) || "program.glb";
    const timeout = (args?.timeout as number) || 5000;

    if (!bytecode) {
      throw new Error("Le paramètre 'bytecode' est requis");
    }

    const vmPath = getVMPath();
    if (!vmPath) {
      throw new Error(
        "La VM n'est pas disponible. Veuillez attendre le téléchargement ou vérifier que la release contient une VM."
      );
    }

    if (!fs.existsSync(vmPath)) {
      throw new Error("La VM n'est pas trouvée à l'emplacement attendu");
    }

    // Créer un fichier temporaire
    const tmpDir = os.tmpdir();
    const tmpFile = path.join(tmpDir, filename);
    fs.writeFileSync(tmpFile, bytecode);

    return new Promise((resolve, reject) => {
      const vm = spawn(vmPath, [tmpFile]);
      let stdout = "";
      let stderr = "";
      let killed = false;

      // Timeout de sécurité
      const timer = setTimeout(() => {
        killed = true;
        vm.kill();
      }, timeout);

      vm.stdout.on("data", (data) => {
        stdout += data.toString();
      });

      vm.stderr.on("data", (data) => {
        stderr += data.toString();
      });

      vm.on("close", (code) => {
        clearTimeout(timer);

        // Nettoyer le fichier temporaire
        try {
          fs.unlinkSync(tmpFile);
        } catch (e) {
          // Ignorer les erreurs de nettoyage
        }

        const state = getBinariesState();
        const version = state.vm?.version || "unknown";

        if (killed) {
          resolve(
            `# Exécution interrompue ⏱️\n\n**Version de la VM:** ${version}\n\n## Raison:\nTimeout après ${timeout}ms\n\n## Sortie partielle:\n\`\`\`\n${stdout}\n\`\`\``
          );
        } else if (code === 0) {
          resolve(
            `# Exécution réussie ✅\n\n**Version de la VM:** ${version}\n\n## Sortie:\n\`\`\`\n${stdout}\n\`\`\``
          );
        } else {
          resolve(
            `# Erreur d'exécution ❌\n\n**Version de la VM:** ${version}\n**Code de sortie:** ${code}\n\n## Erreurs:\n\`\`\`\n${stderr}\n\`\`\`\n\n## Sortie:\n\`\`\`\n${stdout}\n\`\`\``
          );
        }
      });

      vm.on("error", (err) => {
        clearTimeout(timer);
        try {
          fs.unlinkSync(tmpFile);
        } catch (e) {
          // Ignorer
        }
        reject(new Error(`Erreur lors de l'exécution de la VM: ${err.message}`));
      });
    });
  },
};
