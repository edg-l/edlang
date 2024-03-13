use std::{collections::HashMap, fs::File, io::Read, path::PathBuf, time::Instant};

use anyhow::{bail, Context, Result};
use clap::{Parser, Subcommand};
use config::{Config, Package, Profile};
use edlang_driver::{
    compile,
    linker::{link_binary, link_shared_lib},
    CompilerArgs,
};
use git2::{IndexAddOption, Repository};
use owo_colors::OwoColorize;

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

        /// Use a library template
        #[arg(long, group = "binary")]
        lib: bool,
    },
    /// Build a project
    Build {
        /// Build for release with all optimizations.
        #[arg(short, long, default_value_t = false)]
        release: bool,

        /// Override the profile to use.
        #[arg(short, long)]
        profile: Option<String>,
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
            std::fs::write(path.join(".gitignore"), "/build\n")
                .context("failed to write .gitignore")?;
            std::fs::write(path.join(".gitattributes"), "*.ed linguist-language=Rust\n")
                .context("failed to write .gitattributes")?;

            if bin {
                std::fs::write(
                    path.join("src").join("main.ed"),
                    r#"pub fn main() -> i32 {
    return 0;
}"#,
                )?;
            }

            if lib {
                std::fs::write(
                    path.join("src").join("lib.ed"),
                    r#"pub fn main() -> i32 {
    return 0;
}"#,
                )?;
            }

            {
                let repo = Repository::init(&path).context("failed to create repository")?;
                let sig = repo.signature()?;
                let tree_id = {
                    let mut index = repo.index()?;

                    index.add_all(["."].iter(), IndexAddOption::DEFAULT, None)?;
                    index.write()?;
                    index.write_tree()?
                };

                let tree = repo.find_tree(tree_id).context("failed to find git tree")?;
                repo.commit(Some("HEAD"), &sig, &sig, "Initial commit", &tree, &[])
                    .context("failed to create initial commit")?;
            }

            if bin {
                println!(
                    "  {} binary (application) `{}` package",
                    "Created".green().bold(),
                    name
                );
            } else {
                println!("  {} library `{}` package", "Created".green(), name);
            }
        }
        Commands::Build { release, profile } => {
            let mut current_dir = std::env::current_dir()?;
            let mut config_path = None;

            for _ in 0..3 {
                if !current_dir.join("Ed.toml").exists() {
                    current_dir = if let Some(parent) = current_dir.parent() {
                        parent.to_path_buf()
                    } else {
                        bail!("Couldn't find Ed.toml");
                    };
                } else {
                    config_path = Some(current_dir.join("Ed.toml"));
                    break;
                }
            }

            let config_path = match config_path {
                Some(x) => x,
                None => bail!("Couldn't find Ed.toml"),
            };

            let base_dir = config_path
                .parent()
                .context("couldn't get config parent dir")?;
            let mut config = File::open(&config_path).context("Failed to open Ed.toml")?;
            let mut buf = String::new();
            config.read_to_string(&mut buf)?;

            let config: Config = toml::from_str(&buf).context("failed to parse Ed.toml")?;

            println!(
                "   {} {} v{} ({})",
                "Compiling".green().bold(),
                config.package.name,
                config.package.version,
                base_dir.display()
            );

            let src_dir = base_dir.join("src");
            let target_dir = base_dir.join("build");

            if !target_dir.exists() {
                std::fs::create_dir_all(&target_dir)?;
            }

            let output = target_dir.join(config.package.name);

            let (profile, profile_name) = if let Some(profile) = profile {
                (
                    config
                        .profile
                        .get(&profile)
                        .context("Couldn't get requested profile")?,
                    profile,
                )
            } else if release {
                (
                    config
                        .profile
                        .get("release")
                        .context("Couldn't get profile: release")?,
                    "release".to_string(),
                )
            } else {
                (
                    config
                        .profile
                        .get("dev")
                        .context("Couldn't get profile: dev")?,
                    "dev".to_string(),
                )
            };

            let lib_ed = src_dir.join("lib.ed");
            let main_ed = src_dir.join("main.ed");

            let start = Instant::now();

            for file in [main_ed, lib_ed] {
                if file.exists() {
                    let is_lib = file.file_stem().unwrap() == "lib";

                    let compile_args = CompilerArgs {
                        input: file,
                        output: if is_lib {
                            let name = output.file_stem().unwrap().to_string_lossy().to_string();
                            let name = format!("lib{name}");
                            output
                                .with_file_name(name)
                                .with_extension(get_platform_library_ext())
                        } else {
                            output.clone()
                        },
                        release,
                        optlevel: Some(profile.opt_level),
                        debug_info: Some(profile.debug_info),
                        library: is_lib,
                        ast: false,
                        ir: false,
                        llvm: true,
                        asm: false,
                        object: true,
                    };
                    let object = compile(&compile_args)?;

                    if compile_args.library {
                        link_shared_lib(&[object], &compile_args.output)?;
                    } else {
                        link_binary(&[object], &compile_args.output)?;
                    }
                }
            }

            let elapsed = start.elapsed();

            println!(
                "   {} {} [{}{}] in {elapsed:?}",
                "Finished".green().bold(),
                profile_name,
                if profile.opt_level > 0 {
                    "optimized"
                } else {
                    "unoptimized"
                },
                if profile.debug_info {
                    " + debuginfo"
                } else {
                    ""
                }
            );
            /*

                       Finished dev [unoptimized + debuginfo] target(s) in 0.06s
            Running `/data2/edgar/edlang/target/debug/edb build`
                    */
        }
    }

    Ok(())
}

pub fn get_platform_library_ext() -> &'static str {
    if cfg!(target_os = "macos") {
        "dylib"
    } else if cfg!(target_os = "windows") {
        "dll"
    } else {
        "so"
    }
}
