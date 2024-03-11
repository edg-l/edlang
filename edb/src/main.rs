use std::{collections::HashMap, path::PathBuf};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use config::{Config, Package, Profile};
use git2::{IndexAddOption, Repository, RepositoryInitOptions};

mod config;

#[derive(Parser, Debug)]
#[command(author, version, about = "edlang builder", long_about = None, bin_name = "edlang")]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Initialize a project
    New {
        path: PathBuf,

        /// The name of the project, defaults to the directory name
        #[arg(long)]
        name: Option<String>,

        /// Use a binary (application) template [default]
        #[arg(long, group = "binary", default_value_t = true)]
        bin: bool,

        #[arg(long, group = "binary")]
        lib: bool,
    },
    /// Build a project
    Build {
        /// Build for release with all optimizations.
        #[arg(short, long, default_value_t = false)]
        release: bool,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::New {
            path,
            name,
            bin,
            lib,
        } => {
            let name = name.unwrap_or_else(|| {
                path.file_name()
                    .context("Failed to get project name")
                    .unwrap()
                    .to_string_lossy()
                    .to_string()
            });

            if !path.exists() {
                std::fs::create_dir_all(&path).context("failed to create the project directory")?;
                std::fs::create_dir_all(path.join("src")).context("failed to create src/")?;
            }

            let config_path = path.join("Ed.toml");

            let mut profiles = HashMap::new();

            profiles.insert(
                "release".to_string(),
                Profile {
                    release: true,
                    opt_level: 3,
                    debug_info: false,
                },
            );

            profiles.insert(
                "dev".to_string(),
                Profile {
                    release: false,
                    opt_level: 0,
                    debug_info: true,
                },
            );

            let config = Config {
                package: Package {
                    name: name.clone(),
                    version: "0.1.0".to_string(),
                    license: "AGPL-3.0-only".to_string(),
                },
                profile: profiles,
            };

            std::fs::write(config_path, toml::to_string_pretty(&config)?)
                .context("failed to write Ed.toml")?;
            std::fs::write(path.join(".gitignore"), "/target-ed\n")
                .context("failed to write .gitignore")?;
            std::fs::write(path.join(".gitattributes"), "*.ed linguist-language=Rust\n")
                .context("failed to write .gitattributes")?;

            if bin {
                std::fs::write(
                    path.join("src").join("main.ed"),
                    format!(
                        r#"
mod {} {{
    pub fn main() -> i32 {{
        return 0;
    }}
}}"#,
                        name
                    ),
                )?;
            }

            if lib {
                std::fs::write(
                    path.join("src").join("lib.ed"),
                    format!(
                        r#"
mod {} {{
    pub fn hello_world() -> i32 {{
        return 0;
    }}
}}"#,
                        name
                    ),
                )?;
            }

            {
                let mut repo = Repository::init(&path).context("failed to create repository")?;
                let sig = repo.signature()?;
                let tree_id = {
                    let mut index = repo.index()?;

                    index.add_all(["*"], IndexAddOption::DEFAULT, None)?;

                    index.write_tree()?
                };

                let tree = repo.find_tree(tree_id).context("failed to find git tree")?;
                repo.commit(Some("HEAD"), &sig, &sig, "Initial commit", &tree, &[])
                    .context("failed to create initial commit")?;
            }
        }
        Commands::Build { release } => todo!(),
    }

    Ok(())
}
