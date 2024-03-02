(function() {var implementors = {
"clap_builder":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"clap_builder/parser/enum.MatchesError.html\" title=\"enum clap_builder::parser::MatchesError\">MatchesError</a>"],["impl&lt;F: <a class=\"trait\" href=\"clap_builder/error/trait.ErrorFormatter.html\" title=\"trait clap_builder::error::ErrorFormatter\">ErrorFormatter</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"clap_builder/error/struct.Error.html\" title=\"struct clap_builder::error::Error\">Error</a>&lt;F&gt;"]],
"color_spantrace":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"color_spantrace/struct.InstallThemeError.html\" title=\"struct color_spantrace::InstallThemeError\">InstallThemeError</a>"]],
"edlang_lowering":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"edlang_lowering/errors/enum.LoweringError.html\" title=\"enum edlang_lowering::errors::LoweringError\">LoweringError</a>"]],
"either":[["impl&lt;L, R&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"either/enum.Either.html\" title=\"enum either::Either\">Either</a>&lt;L, R&gt;<div class=\"where\">where\n    L: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a>,\n    R: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a>,</div>"]],
"eyre":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"eyre/struct.InstallError.html\" title=\"struct eyre::InstallError\">InstallError</a>"]],
"inkwell":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"inkwell/support/struct.LLVMString.html\" title=\"struct inkwell::support::LLVMString\">LLVMString</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"inkwell/execution_engine/enum.RemoveModuleError.html\" title=\"enum inkwell::execution_engine::RemoveModuleError\">RemoveModuleError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"inkwell/builder/enum.BuilderError.html\" title=\"enum inkwell::builder::BuilderError\">BuilderError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"inkwell/execution_engine/enum.FunctionLookupError.html\" title=\"enum inkwell::execution_engine::FunctionLookupError\">FunctionLookupError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"inkwell/support/enum.LoadLibraryError.html\" title=\"enum inkwell::support::LoadLibraryError\">LoadLibraryError</a>"]],
"itertools":[["impl&lt;I&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"itertools/structs/struct.ExactlyOneError.html\" title=\"struct itertools::structs::ExactlyOneError\">ExactlyOneError</a>&lt;I&gt;<div class=\"where\">where\n    I: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/iter/traits/iterator/trait.Iterator.html\" title=\"trait core::iter::traits::iterator::Iterator\">Iterator</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a>,\n    I::<a class=\"associatedtype\" href=\"https://doc.rust-lang.org/1.76.0/core/iter/traits/iterator/trait.Iterator.html#associatedtype.Item\" title=\"type core::iter::traits::iterator::Iterator::Item\">Item</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a>,</div>"]],
"lalrpop_util":[["impl&lt;L, T, E&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"lalrpop_util/enum.ParseError.html\" title=\"enum lalrpop_util::ParseError\">ParseError</a>&lt;L, T, E&gt;<div class=\"where\">where\n    L: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Display.html\" title=\"trait core::fmt::Display\">Display</a>,\n    T: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Display.html\" title=\"trait core::fmt::Display\">Display</a>,\n    E: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Display.html\" title=\"trait core::fmt::Display\">Display</a>,</div>"]],
"log":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"log/struct.ParseLevelError.html\" title=\"struct log::ParseLevelError\">ParseLevelError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"log/struct.SetLoggerError.html\" title=\"struct log::SetLoggerError\">SetLoggerError</a>"]],
"proc_macro2":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"proc_macro2/struct.LexError.html\" title=\"struct proc_macro2::LexError\">LexError</a>"]],
"regex":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"regex/enum.Error.html\" title=\"enum regex::Error\">Error</a>"]],
"regex_automata":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/nfa/thompson/struct.BuildError.html\" title=\"struct regex_automata::nfa::thompson::BuildError\">BuildError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/struct.PatternSetInsertError.html\" title=\"struct regex_automata::PatternSetInsertError\">PatternSetInsertError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/util/wire/struct.DeserializeError.html\" title=\"struct regex_automata::util::wire::DeserializeError\">DeserializeError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/util/primitives/struct.PatternIDError.html\" title=\"struct regex_automata::util::primitives::PatternIDError\">PatternIDError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/util/primitives/struct.SmallIndexError.html\" title=\"struct regex_automata::util::primitives::SmallIndexError\">SmallIndexError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/util/captures/struct.GroupInfoError.html\" title=\"struct regex_automata::util::captures::GroupInfoError\">GroupInfoError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/util/wire/struct.SerializeError.html\" title=\"struct regex_automata::util::wire::SerializeError\">SerializeError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/util/look/struct.UnicodeWordBoundaryError.html\" title=\"struct regex_automata::util::look::UnicodeWordBoundaryError\">UnicodeWordBoundaryError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/struct.MatchError.html\" title=\"struct regex_automata::MatchError\">MatchError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/meta/struct.BuildError.html\" title=\"struct regex_automata::meta::BuildError\">BuildError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_automata/util/primitives/struct.StateIDError.html\" title=\"struct regex_automata::util::primitives::StateIDError\">StateIDError</a>"]],
"regex_syntax":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_syntax/ast/struct.Error.html\" title=\"struct regex_syntax::ast::Error\">Error</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_syntax/hir/struct.CaseFoldError.html\" title=\"struct regex_syntax::hir::CaseFoldError\">CaseFoldError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_syntax/struct.UnicodeWordError.html\" title=\"struct regex_syntax::UnicodeWordError\">UnicodeWordError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"regex_syntax/hir/struct.Error.html\" title=\"struct regex_syntax::hir::Error\">Error</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"regex_syntax/enum.Error.html\" title=\"enum regex_syntax::Error\">Error</a>"]],
"strsim":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"enum\" href=\"strsim/enum.StrSimError.html\" title=\"enum strsim::StrSimError\">StrSimError</a>"]],
"syn":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"syn/parse/struct.Error.html\" title=\"struct syn::parse::Error\">Error</a>"]],
"tracing_core":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_core/dispatcher/struct.SetGlobalDefaultError.html\" title=\"struct tracing_core::dispatcher::SetGlobalDefaultError\">SetGlobalDefaultError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_core/metadata/struct.ParseLevelError.html\" title=\"struct tracing_core::metadata::ParseLevelError\">ParseLevelError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_core/metadata/struct.ParseLevelFilterError.html\" title=\"struct tracing_core::metadata::ParseLevelFilterError\">ParseLevelFilterError</a>"]],
"tracing_error":[["impl&lt;E&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_error/struct.TracedError.html\" title=\"struct tracing_error::TracedError\">TracedError</a>&lt;E&gt;<div class=\"where\">where\n    E: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> + 'static,</div>"]],
"tracing_subscriber":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_subscriber/util/struct.TryInitError.html\" title=\"struct tracing_subscriber::util::TryInitError\">TryInitError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_subscriber/filter/struct.BadFieldName.html\" title=\"struct tracing_subscriber::filter::BadFieldName\">BadName</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_subscriber/reload/struct.Error.html\" title=\"struct tracing_subscriber::reload::Error\">Error</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_subscriber/filter/struct.ParseError.html\" title=\"struct tracing_subscriber::filter::ParseError\">ParseError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/error/trait.Error.html\" title=\"trait core::error::Error\">Error</a> for <a class=\"struct\" href=\"tracing_subscriber/filter/struct.FromEnvError.html\" title=\"struct tracing_subscriber::filter::FromEnvError\">FromEnvError</a>"]]
};if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()