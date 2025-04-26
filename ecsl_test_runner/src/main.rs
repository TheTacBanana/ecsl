use ecsl_diagnostics::Diagnostics;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use glob::glob;
use log::info;
use std::process::Command;

fn main() -> anyhow::Result<()> {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .format_target(false)
        .format_timestamp(None)
        .format_file(false)
        .format_line_number(false)
        .init();

    info!("ECSL Test Runner");

    let diag = Diagnostics::new();

    let mut files = Vec::new();
    for entry in glob("ecsl_tests/**/*.ecsl".into())? {
        if let Ok(full_path) = entry {
            files.push(full_path);
        }
    }

    // Create temp project
    _ = Command::new("bundle")
        .arg("new")
        .arg("temp")
        .spawn()
        .unwrap()
        .wait();

    for file in files {
        std::fs::copy(&file, "temp/src/main.ecsl").unwrap();
        let status = Command::new("bundle")
            .current_dir("temp/")
            .arg("run")
            .output()
            .unwrap();

        if status.status.success() {
            diag.push_error(
                EcslError::new(ErrorLevel::Note, "Test Success").with_path(|_| file.clone()),
            );
        } else {
            diag.push_error(
                EcslError::new(ErrorLevel::Error, "Failed Test")
                    .with_path(|_| file.clone())
                    .with_note(|_| {
                        let mut s = String::from("\n");
                        s.push_str(String::from_utf8(status.stderr.clone()).unwrap().as_str());
                        s
                    }),
            );
        }
    }

    std::fs::remove_dir_all("./temp")?;

    _ = diag.finish_stage(|_| ());

    anyhow::Ok(())
}
