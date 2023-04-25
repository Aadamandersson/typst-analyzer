use lsp_server::{Connection, Message, Notification};
use lsp_types::{ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, notification::{DidOpenTextDocument, DidChangeTextDocument, DidCloseTextDocument}, InitializeParams};

pub(crate) fn run() -> eyre::Result<()> {
    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server
    let (initialize_id, initialize_params) = connection.initialize_start()?;
    let server_capabilities = server_capabilities();

    let initialize_result = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "typst-analyzer",
            "version": "0.1"
        }
    });

    connection.initialize_finish(initialize_id, initialize_result)?;
    main_loop(connection, initialize_params)?;
    io_threads.join()?;
    Ok(())
}

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..ServerCapabilities::default()
    }
}

fn main_loop(connection: Connection, params: serde_json::Value) -> eyre::Result<()> {
    let _params: InitializeParams = serde_json::from_value(params)?;
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                eprintln!("got request: {req:?}");
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
            }
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                if let Some(_params) = cast_notification::<DidOpenTextDocument>(&not) {
                    todo!()
                } else if let Some(_params) = cast_notification::<DidChangeTextDocument>(&not) {
                    todo!()
                } else if let Some(_params) = cast_notification::<DidCloseTextDocument>(&not) {
                    todo!()
                } else {
                    eprintln!("unhandled notification: {not:?}");
                }
            }
        }
    }
    Ok(())
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
