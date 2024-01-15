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
