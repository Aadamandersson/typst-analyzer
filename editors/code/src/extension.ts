import { ExtensionContext, Uri, workspace } from 'vscode';

import {
    Executable,
    LanguageClient, LanguageClientOptions, ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined = undefined;

export function activate(ctx: ExtensionContext) {
    let config = workspace.getConfiguration("typst-analyzer");
    console.log(config);

    const run: Executable = {
        command: getServer(ctx),
    };

    const serverOptions: ServerOptions = {
        run,
        debug: run
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "typst" }],
    };

    client = new LanguageClient("typst-analyzer", "Typst Analyzer Language Server", serverOptions, clientOptions);
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

function getServer(ctx: ExtensionContext): string {
    const debugPath = process.env.__TA_LSP_SERVER_DEBUG;
    if (debugPath) {
        return debugPath;
    }
    const ext = process.platform === "win32" ? ".exe" : "";
    const server = Uri.joinPath(ctx.globalStorageUri, "server", `typst-analyzer${ext}`);
    return server.fsPath;
}
