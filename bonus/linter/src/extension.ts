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
    // Extension activation
    vscode.window.showInformationMessage('🐀 Ratatouille Language Support v2.0.3 activated');
    console.log('🐀 Ratatouille extension v2.0.3 is now active!');
    console.log('🐀 Context:', context);
    
    // Register restart command
    const restartCommand = vscode.commands.registerCommand('ratatouille.restartServer', () => {
        vscode.window.showInformationMessage('🐀 Restarting Ratatouille Language Server...');
        if (client) {
            client.stop().then(() => {
                client.start();
            });
        }
    });
    context.subscriptions.push(restartCommand);
    
    // The server is implemented in node
    const serverModule = context.asAbsolutePath(
        path.join('out', 'server.js')
    );
    
    console.log('🐀 Server module path:', serverModule);
    
    // Check if file exists
    const fs = require('fs');
    if (!fs.existsSync(serverModule)) {
        const msg = '🐀 ERROR: server.js not found at ' + serverModule;
        console.error(msg);
        vscode.window.showErrorMessage(msg);
        return;
    }
    console.log('🐀 server.js exists!');

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
    console.log('🐀 Starting Language Server...');
    vscode.window.showInformationMessage('🐀 Starting Ratatouille Language Server...');
    client.start().then(() => {
        console.log('🐀 Language Server started successfully!');
        vscode.window.showInformationMessage('🐀 Ratatouille Language Server ready!');
    }).catch(err => {
        console.error('🐀 Failed to start Language Server:', err);
        vscode.window.showErrorMessage('🐀 Failed to start Language Server: ' + err);
    });
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
