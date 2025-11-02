import { TextDocument } from 'vscode-languageserver-textdocument';

export interface Symbol {
    name: string;
    kind: 'proc' | 'func' | 'variable' | 'parameter' | 'atom';
    line: number;
    column: number;
    endLine: number;
    endColumn: number;
    type?: string;
    documentation?: string;
}

export interface DocumentSymbols {
    procs: Map<string, Symbol>;
    funcs: Map<string, Symbol>;
    variables: Map<string, Symbol>;
    atoms: Set<string>;
    imports: Map<string, string[]>; // modulePath -> imported symbols
}

export class DocumentAnalyzer {
    private document: TextDocument;

    constructor(document: TextDocument) {
        this.document = document;
    }

    public analyze(): DocumentSymbols {
        const symbols: DocumentSymbols = {
            procs: new Map(),
            funcs: new Map(),
            variables: new Map(),
            atoms: new Set(),
            imports: new Map()
        };

        const text = this.document.getText();
        const lines = text.split('\n');

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            
            // Parse proc definitions
            const procMatch = line.match(/\bproc\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(/);
            if (procMatch) {
                const name = procMatch[1];
                const column = line.indexOf(procMatch[0]) + 5; // After 'proc '
                
                // Extract parameters
                const paramsMatch = line.match(/\(([^)]*)\)/);
                const params = paramsMatch ? this.parseParameters(paramsMatch[1]) : [];
                
                symbols.procs.set(name, {
                    name,
                    kind: 'proc',
                    line: i,
                    column,
                    endLine: i,
                    endColumn: column + name.length,
                    documentation: this.generateProcDocumentation(name, params)
                });

                // Add parameters as variables
                params.forEach(param => {
                    symbols.variables.set(param.name, {
                        name: param.name,
                        kind: 'parameter',
                        line: i,
                        column: 0,
                        endLine: i,
                        endColumn: 0,
                        type: param.type,
                        documentation: `Parameter of proc ${name}`
                    });
                });
            }

            // Parse func definitions
            const funcMatch = line.match(/\bfunc\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(/);
            if (funcMatch) {
                const name = funcMatch[1];
                const column = line.indexOf(funcMatch[0]) + 5; // After 'func '
                
                const paramsMatch = line.match(/\(([^)]*)\)/);
                const params = paramsMatch ? this.parseParameters(paramsMatch[1]) : [];
                
                symbols.funcs.set(name, {
                    name,
                    kind: 'func',
                    line: i,
                    column,
                    endLine: i,
                    endColumn: column + name.length,
                    documentation: this.generateFuncDocumentation(name, params)
                });
            }

            // Parse let/const declarations
            const letMatch = line.match(/\b(let|const)\s+([a-zA-Z_][a-zA-Z0-9_]*)(?:\s*<\s*([a-zA-Z0-9_]+)\s*>)?/);
            if (letMatch) {
                const name = letMatch[2];
                const type = letMatch[3];
                const column = line.indexOf(name);
                
                symbols.variables.set(name, {
                    name,
                    kind: 'variable',
                    line: i,
                    column,
                    endLine: i,
                    endColumn: column + name.length,
                    type,
                    documentation: type ? `Variable of type ${type}` : 'Variable'
                });
            }

            // Parse atoms
            const atomMatches = line.matchAll(/:([a-zA-Z_][a-zA-Z0-9_]*)/g);
            for (const match of atomMatches) {
                symbols.atoms.add(match[1]);
            }

            // Parse imports
            const importMatch = line.match(/\bimport\s+(?:\{([^}]+)\}|([a-zA-Z_][a-zA-Z0-9_]*)|\*)\s+from\s+"([^"]+)"/);
            if (importMatch) {
                const modulePath = importMatch[3];
                let importedSymbols: string[] = [];
                
                if (importMatch[1]) {
                    // import {A, B} from "module"
                    importedSymbols = importMatch[1].split(',').map((s: string) => s.trim());
                } else if (importMatch[2]) {
                    // import A from "module"
                    importedSymbols = [importMatch[2]];
                } else {
                    // import * from "module"
                    importedSymbols = ['*'];
                }
                
                symbols.imports.set(modulePath, importedSymbols);
            }
        }

        return symbols;
    }

    private parseParameters(paramsStr: string): Array<{ name: string; type?: string }> {
        if (!paramsStr.trim()) return [];
        
        return paramsStr.split(',').map(param => {
            const trimmed = param.trim();
            const typeMatch = trimmed.match(/([a-zA-Z_][a-zA-Z0-9_]*)\s*<\s*([a-zA-Z0-9_]+)\s*>/);
            
            if (typeMatch) {
                return { name: typeMatch[1], type: typeMatch[2] };
            }
            return { name: trimmed };
        });
    }

    private generateProcDocumentation(name: string, params: Array<{ name: string; type?: string }>): string {
        const paramStr = params.map(p => p.type ? `${p.name}: ${p.type}` : p.name).join(', ');
        return `**proc** ${name}(${paramStr})\n\nActor-based process that can receive and send messages.`;
    }

    private generateFuncDocumentation(name: string, params: Array<{ name: string; type?: string }>): string {
        const paramStr = params.map(p => p.type ? `${p.name}: ${p.type}` : p.name).join(', ');
        return `**func** ${name}(${paramStr})\n\nPure function that returns a computed value.`;
    }

    public getSymbolAtPosition(line: number, character: number): Symbol | undefined {
        const symbols = this.analyze();
        
        // Check all symbol types
        const allSymbols = [
            ...Array.from(symbols.procs.values()),
            ...Array.from(symbols.funcs.values()),
            ...Array.from(symbols.variables.values())
        ];

        for (const symbol of allSymbols) {
            if (symbol.line === line && 
                character >= symbol.column && 
                character <= symbol.endColumn) {
                return symbol;
            }
        }

        return undefined;
    }
}
