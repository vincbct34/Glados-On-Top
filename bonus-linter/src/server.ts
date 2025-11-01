import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    InitializeParams,
    DidChangeConfigurationNotification,
    CompletionItem,
    CompletionItemKind,
    TextDocumentPositionParams,
    TextDocumentSyncKind,
    InitializeResult,
    Hover,
    MarkupKind,
    Definition,
    Location,
    Position,
    Range
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';
import { DocumentAnalyzer } from './analyzer';

// Create a connection for the server
const connection = createConnection(ProposedFeatures.all);

// Log to client console
connection.console.log('🐀 Ratatouille Language Server starting...');

// Create a simple text document manager
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;

// Store analyzed documents
const documentAnalyzers = new Map<string, DocumentAnalyzer>();

connection.onInitialize((params: InitializeParams) => {
    connection.console.log('🐀 onInitialize called');
    const capabilities = params.capabilities;

    hasConfigurationCapability = !!(
        capabilities.workspace && !!capabilities.workspace.configuration
    );
    hasWorkspaceFolderCapability = !!(
        capabilities.workspace && !!capabilities.workspace.workspaceFolders
    );
    
    connection.console.log('🐀 Configuration capability: ' + hasConfigurationCapability);
    connection.console.log('🐀 Workspace folder capability: ' + hasWorkspaceFolderCapability);

    const result: InitializeResult = {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            completionProvider: {
                resolveProvider: true,
                triggerCharacters: [':', '.', ' ']
            },
            hoverProvider: true,
            definitionProvider: true
        }
    };

    if (hasWorkspaceFolderCapability) {
        result.capabilities.workspace = {
            workspaceFolders: {
                supported: true
            }
        };
    }
    return result;
});

connection.onInitialized(() => {
    connection.console.log('🐀 Server initialized successfully!');
    if (hasConfigurationCapability) {
        connection.client.register(DidChangeConfigurationNotification.type, undefined);
    }
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent((change: { document: TextDocument }) => {
    const analyzer = new DocumentAnalyzer(change.document);
    documentAnalyzers.set(change.document.uri, analyzer);
});

documents.onDidClose((e: { document: TextDocument }) => {
    documentAnalyzers.delete(e.document.uri);
});

// Hover provider
connection.onHover((params: TextDocumentPositionParams): Hover | null => {
    connection.console.log('🐀 onHover called at line ' + params.position.line + ', char ' + params.position.character);
    const document = documents.get(params.textDocument.uri);
    if (!document) {
        connection.console.log('🐀 onHover: document not found');
        return null;
    }

    const analyzer = documentAnalyzers.get(params.textDocument.uri);
    if (!analyzer) return null;

    const symbols = analyzer.analyze();
    const line = params.position.line;
    const character = params.position.character;
    const lineText = document.getText({
        start: { line, character: 0 },
        end: { line: line + 1, character: 0 }
    }).trim();

    // Get word at position
    const wordRange = getWordRangeAtPosition(document, params.position);
    if (!wordRange) {
        connection.console.log('🐀 onHover: no word at position');
        return null;
    }

    const word = document.getText(wordRange);
    connection.console.log('🐀 onHover: word = "' + word + '"');

    // Check if it's a proc
    if (symbols.procs.has(word)) {
        const symbol = symbols.procs.get(word)!;
        return {
            contents: {
                kind: MarkupKind.Markdown,
                value: symbol.documentation || `**proc** ${word}`
            }
        };
    }

    // Check if it's a func
    if (symbols.funcs.has(word)) {
        const symbol = symbols.funcs.get(word)!;
        return {
            contents: {
                kind: MarkupKind.Markdown,
                value: symbol.documentation || `**func** ${word}`
            }
        };
    }

    // Check if it's a variable
    if (symbols.variables.has(word)) {
        const symbol = symbols.variables.get(word)!;
        const typeInfo = symbol.type ? `: ${symbol.type}` : '';
        return {
            contents: {
                kind: MarkupKind.Markdown,
                value: `**${symbol.kind}** ${word}${typeInfo}\n\n${symbol.documentation || ''}`
            }
        };
    }

    // Check if it's an atom
    if (word.startsWith(':') && symbols.atoms.has(word.substring(1))) {
        return {
            contents: {
                kind: MarkupKind.Markdown,
                value: `**atom** ${word}\n\nAtomic constant value used for pattern matching and messages.`
            }
        };
    }

    // Check for keywords
    const keywordDocs = getKeywordDocumentation(word);
    if (keywordDocs) {
        return {
            contents: {
                kind: MarkupKind.Markdown,
                value: keywordDocs
            }
        };
    }

    return null;
});

// Completion provider
connection.onCompletion((params: TextDocumentPositionParams): CompletionItem[] => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];

    const analyzer = documentAnalyzers.get(params.textDocument.uri);
    if (!analyzer) return [];

    const symbols = analyzer.analyze();
    const completions: CompletionItem[] = [];

    // Add procs
    symbols.procs.forEach((symbol, name) => {
        completions.push({
            label: name,
            kind: CompletionItemKind.Function,
            detail: 'proc',
            documentation: symbol.documentation
        });
    });

    // Add funcs
    symbols.funcs.forEach((symbol, name) => {
        completions.push({
            label: name,
            kind: CompletionItemKind.Function,
            detail: 'func',
            documentation: symbol.documentation
        });
    });

    // Add variables
    symbols.variables.forEach((symbol, name) => {
        completions.push({
            label: name,
            kind: symbol.kind === 'parameter' ? CompletionItemKind.Variable : CompletionItemKind.Variable,
            detail: symbol.type || symbol.kind,
            documentation: symbol.documentation
        });
    });

    // Add atoms
    symbols.atoms.forEach(atom => {
        completions.push({
            label: `:${atom}`,
            kind: CompletionItemKind.Constant,
            detail: 'atom'
        });
    });

    // Add keywords
    const keywords = [
        'proc', 'func', 'receive', 'spawn', 'state', 'let', 'const', 'self',
        'if', 'then', 'else', 'match', 'import', 'from', 'scast', 'rcast',
        'Just', 'None', 'Left', 'Right', 'true', 'false'
    ];

    keywords.forEach(keyword => {
        completions.push({
            label: keyword,
            kind: CompletionItemKind.Keyword,
            detail: 'keyword'
        });
    });

    // Add types
    const types = [
        'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
        'Int', 'Float', 'String', 'Bool', 'Char', 'Pid', 'Maybe', 'Either', 'Array', 'Tuple'
    ];

    types.forEach(type => {
        completions.push({
            label: type,
            kind: CompletionItemKind.TypeParameter,
            detail: 'type'
        });
    });

    return completions;
});

// Go to definition
connection.onDefinition((params: TextDocumentPositionParams): Definition | null => {
    connection.console.log('🐀 onDefinition called at line ' + params.position.line + ', char ' + params.position.character);
    const document = documents.get(params.textDocument.uri);
    if (!document) {
        connection.console.log('🐀 onDefinition: document not found');
        return null;
    }

    const analyzer = documentAnalyzers.get(params.textDocument.uri);
    if (!analyzer) return null;

    const symbols = analyzer.analyze();
    const wordRange = getWordRangeAtPosition(document, params.position);
    if (!wordRange) {
        connection.console.log('🐀 onDefinition: no word at position');
        return null;
    }

    const word = document.getText(wordRange);
    connection.console.log('🐀 onDefinition: word = "' + word + '"');

    // Check if it's a proc
    if (symbols.procs.has(word)) {
        const symbol = symbols.procs.get(word)!;
        return Location.create(params.textDocument.uri, {
            start: { line: symbol.line, character: symbol.column },
            end: { line: symbol.endLine, character: symbol.endColumn }
        });
    }

    // Check if it's a func
    if (symbols.funcs.has(word)) {
        const symbol = symbols.funcs.get(word)!;
        return Location.create(params.textDocument.uri, {
            start: { line: symbol.line, character: symbol.column },
            end: { line: symbol.endLine, character: symbol.endColumn }
        });
    }

    // Check if it's a variable
    if (symbols.variables.has(word)) {
        const symbol = symbols.variables.get(word)!;
        return Location.create(params.textDocument.uri, {
            start: { line: symbol.line, character: symbol.column },
            end: { line: symbol.endLine, character: symbol.endColumn }
        });
    }

    return null;
});

// Helper function to get word range at position
function getWordRangeAtPosition(document: TextDocument, position: Position): Range | null {
    const line = document.getText({
        start: { line: position.line, character: 0 },
        end: { line: position.line + 1, character: 0 }
    });

    let start = position.character;
    let end = position.character;

    // Handle atoms starting with :
    if (start > 0 && line[start - 1] === ':') {
        start--;
    }

    // Find word boundaries
    while (start > 0 && /[a-zA-Z0-9_:]/.test(line[start - 1])) {
        start--;
    }
    while (end < line.length && /[a-zA-Z0-9_]/.test(line[end])) {
        end++;
    }

    if (start === end) return null;

    return {
        start: { line: position.line, character: start },
        end: { line: position.line, character: end }
    };
}

// Keyword documentation
function getKeywordDocumentation(word: string): string | null {
    const docs: Record<string, string> = {
        'proc': '**proc** name(params) { ... }\n\nDefines an actor-based process that can receive and send messages.',
        'func': '**func** name(params) { expr }\n\nDefines a pure function that returns a computed value.',
        'receive': '**receive** { | pattern -> expr }\n\nPattern match on incoming messages.',
        'spawn': '**spawn** ProcName(args)\n\nCreates a new process and returns its PID.',
        'state': '**state:** expr\n\nInitializes the internal state of a process.',
        'let': '**let** name = expr\n\nDeclares a variable.',
        'const': '**const** name = expr\n\nDeclares a constant variable.',
        'self': '**self**\n\nReturns the current process PID.',
        'if': '**if** condition **then** expr **else** expr\n\nConditional expression.',
        'then': 'Part of if expression.',
        'else': 'Part of if expression.',
        'match': '**match** expr { | pattern -> value }\n\nPattern matching expression.',
        'import': '**import** {items} **from** "module.rat"\n\nImports definitions from another module.',
        'from': 'Used with import statement.',
        'scast': '**scast**<Type>(expr)\n\nSafe type cast with validation.',
        'rcast': '**rcast**<Type>(expr)\n\nReinterpretation cast (unsafe).',
        'Just': '**Just**(value)\n\nMaybe type constructor for present value.',
        'None': '**None**\n\nMaybe type constructor for absent value.',
        'Left': '**Left**(value)\n\nEither type constructor for error value.',
        'Right': '**Right**(value)\n\nEither type constructor for success value.',
        'print': '**print**(value)\n\nBuilt-in function to print values.'
    };

    return docs[word] || null;
}

// Make the text document manager listen on the connection
documents.listen(connection);

// Listen on the connection
connection.console.log('🐀 Server is now listening for requests...');
connection.listen();
