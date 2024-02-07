use std::path::PathBuf;

#[derive(Clone, Debug)]
pub struct Opts {
    pub source_path: Vec<PathBuf>,
    pub exe_path: PathBuf,
    pub dwarf_output_path: Option<PathBuf>,
    pub c_output_path: Option<PathBuf>,
    pub r4e_output_path: Option<PathBuf>,
    pub rust_output_path: Option<PathBuf>,
    pub idc_output_path: Option<PathBuf>,
    pub til_output_path: Option<PathBuf>,
    pub strip_namespaces: bool,
    pub eager_type_export: bool,
    pub compiler_flags: Vec<String>,
    pub show_clang_errors: bool,
    pub safe_addr: bool,
    pub skip_lookup: bool,
}

impl Opts {
    pub fn load(header: &'static str) -> Self {
        use bpaf::*;

        let source_path = positional_os("SOURCE").map(PathBuf::from).many();
        let exe_path = long("exe").short('x').argument_os("EXE").map(PathBuf::from);
        let dwarf_output_path = long("dwarf-output")
            .short('o')
            .help("DWARF file to write")
            .argument_os("DWARF")
            .map(PathBuf::from)
            .optional();
        let c_output_path = long("c-output")
            .help("C header with offsets to write")
            .argument_os("C")
            .map(PathBuf::from)
            .optional();
        let r4e_output_path = long("r4e-output")
            .help("red4ext relocation implementations")
            .argument_os("R4E")
            .map(PathBuf::from)
            .optional();
        let idc_output_path = long("idc-output")
            .help("IDC type definition file to write")
            .argument("IDC")
            .from_str::<PathBuf>()
            .optional();
        let til_output_path = long("til-output")
            .help("TIL file to write")
            .argument("TIL")
            .from_str::<PathBuf>()
            .optional();
        let rust_output_path = long("rust-output")
            .help("Rust file with offsets to write")
            .argument_os("RUST")
            .map(PathBuf::from)
            .optional();
        let strip_namespaces = long("strip-namespaces")
            .help("Strip namespaces from type names")
            .switch();
        let eager_type_export = long("eager-type-export")
            .help("Export all types found in the sources")
            .switch();
        let compiler_flags = long("compiler-flag")
            .short('f')
            .help("Flags to pass to the compiler")
            .argument("FLAGS")
            .many();
        let show_clang_errors = long("show-clang-errors")
            .short('s')
            .help("Show all of the clang compiler messages")
            .switch();
        let safe_addr = long("safe-addr").help("Wrap defines in conditionals").switch();
        let skip_lookup = long("skip-lookup")
            .help("Export only processed types")
            .switch();

        let parser = construct!(Opts {
            source_path,
            exe_path,
            dwarf_output_path,
            c_output_path,
            r4e_output_path,
            idc_output_path,
            til_output_path,
            rust_output_path,
            strip_namespaces,
            eager_type_export
            compiler_flags,
            show_clang_errors,
            safe_addr,
            skip_lookup,
        });

        Info::default().descr(header).for_parser(parser).run()
    }
}
