use lsp_server::{Connection, Message, Notification};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
    },
    Diagnostic, DiagnosticSeverity, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

pub(crate) fn run() -> eyre::Result<()> {
    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server.
    let (initialize_id, _) = connection.initialize_start()?;
    let server_capabilities = server_capabilities();

    let initialize_result = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "typst-analyzer",
            "version": "0.1"
        }
    });

    connection.initialize_finish(initialize_id, initialize_result)?;
    main_loop(connection)?;
    io_threads.join()?;
    Ok(())
}

// TODO: move all of this
pub fn main_loop(connection: Connection) -> eyre::Result<()> {
    LspServer::new(connection).run()
}

struct LspServer {
    connection: Connection,
}

impl LspServer {
    fn new(connection: Connection) -> Self {
        Self { connection }
    }

    fn run(&mut self) -> eyre::Result<()> {
        for msg in &self.connection.receiver {
            match msg {
                Message::Request(req) => {
                    eprintln!("got request: {req:?}");
                    if self.connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                }
                Message::Response(resp) => {
                    eprintln!("got response: {resp:?}");
                }
                Message::Notification(not) => {
                    eprintln!("got notification: {not:?}");
                    if let Some(params) = cast_notification::<DidOpenTextDocument>(&not) {
                        self.validate(
                            &params.text_document.text,
                            params.text_document.uri,
                            params.text_document.version,
                        )?;
                    } else if let Some(params) = cast_notification::<DidChangeTextDocument>(&not) {
                        let change = params.content_changes.into_iter().next().unwrap();
                        self.validate(
                            &change.text,
                            params.text_document.uri,
                            params.text_document.version,
                        )?;
                    } else if let Some(_params) = cast_notification::<DidCloseTextDocument>(&not) {
                    } else {
                        eprintln!("unhandled notification: {not:?}");
                    }
                }
            }
        }
        Ok(())
    }

    fn validate(&self, src: &str, url: Url, version: i32) -> eyre::Result<()> {
        let (_, errors) = syntax::parser::parse(src);
        let mut diags = vec![];
        for err in errors {
            let range = err.range();

            let start = Position {
                line: 0, // TODO
                character: range.start().into(),
            };

            let end = Position {
                line: 0, // TODO
                character: range.end().into(),
            };

            let diag = Diagnostic::new(
                Range { start, end },
                Some(DiagnosticSeverity::ERROR),
                None,
                None,
                err.message().to_string(),
                None,
                None,
            );

            diags.push(diag);
        }

        let params = PublishDiagnosticsParams {
            uri: url,
            diagnostics: diags,
            version: Some(version),
        };

        let not = new_notifcation::<PublishDiagnostics>(params);
        self.connection.sender.send(Message::Notification(not))?;
        Ok(())
    }
}

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..ServerCapabilities::default()
    }
}

fn cast_notification<T>(not: &Notification) -> Option<T::Params>
where
    T: lsp_types::notification::Notification,
{
    if not.method == T::METHOD {
        let params = serde_json::from_value(not.params.clone()).unwrap_or_else(|err| {
            panic!(
                "Invalid notification\nMethod: {}\n error: {}",
                not.method, err
            )
        });
        Some(params)
    } else {
        None
    }
}

fn new_notifcation<T>(params: T::Params) -> Notification
where
    T: lsp_types::notification::Notification,
{
    Notification {
        method: T::METHOD.to_owned(),
        params: serde_json::to_value(&params).unwrap(),
    }
}
