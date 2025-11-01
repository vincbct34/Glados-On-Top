import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    console.log('Ratatouille extension is now active!');

    // The server is implemented in node
    const serverModule = context.asAbsolutePath(
        path.join('out', 'server.js')
    );

    // The debug options for the server
    const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions: ServerOptions = {
        run: { module: serverModule, transport: TransportKind.ipc },
        debug: {
            module: serverModule,
            transport: TransportKind.ipc,
            options: debugOptions
        }
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for Ratatouille documents
        documentSelector: [{ scheme: 'file', language: 'ratatouille' }],
        synchronize: {
            // Notify the server about file changes to '.rat' files contained in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.rat')
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'ratatouilleLanguageServer',
        'Ratatouille Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
