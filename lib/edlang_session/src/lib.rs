use std::path::PathBuf;

use ariadne::Source;

#[derive(Debug, Clone)]
pub struct Session {
    pub file_path: PathBuf,
    pub debug_info: DebugInfo,
    pub optlevel: OptLevel,
    pub source: Source<String>,
    pub library: bool,
    /// The directory where to store artifacts and intermediate files such as object files.
    pub target_dir: PathBuf,
    pub output_file: PathBuf,
    pub output_llvm: bool,
    pub output_asm: bool,
}

impl Session {
    pub fn get_platform_library_ext() -> &'static str {
        if cfg!(target_os = "macos") {
            "dylib"
        } else if cfg!(target_os = "windows") {
            "dll"
        } else {
            "so"
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum OptLevel {
    None,       // -O0
    Less,       // -O1
    Default,    // -O2
    Aggressive, // -O3
}

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum DebugInfo {
    None,
    Full,
}
