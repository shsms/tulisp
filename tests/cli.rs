//! The `tulisp` binary as a Unix filter.

use std::io::Write;
use std::process::{Command, Stdio};

fn tulisp() -> Command {
    Command::new(env!("CARGO_BIN_EXE_tulisp"))
}

#[test]
fn a_closed_stdout_ends_the_program_quietly() {
    let dir = std::env::temp_dir().join(format!("tulisp-cli-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let script = dir.join("many-lines.lisp");
    // Far more than a pipe buffer holds, so the writes block until
    // the reader closes and then fail with EPIPE.
    std::fs::File::create(&script)
        .unwrap()
        .write_all(b"(dotimes (i 100000) (print i))")
        .unwrap();
    let mut child = tulisp()
        .arg(&script)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    // Close the read end without reading anything.
    drop(child.stdout.take());
    let output = child.wait_with_output().unwrap();
    std::fs::remove_dir_all(&dir).unwrap();
    assert!(output.status.success(), "{:?}", output.status);
    assert!(
        output.stderr.is_empty(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn an_error_goes_to_stderr_with_a_failure_status() {
    let output = tulisp().arg("/nonexistent/file.lisp").output().unwrap();
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("Unable to read file"));
}
