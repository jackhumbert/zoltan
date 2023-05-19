use zoltan::ustr::Ustr;

pub trait RedName {
    fn get_context_name(&self) -> Ustr;
}

impl RedName for clang::Entity<'_> {
    fn get_context_name(&self) -> Ustr {
        let mut cur = Some(*self);
        let mut names: Vec<String> = vec![];

        while let Some(parent) = cur {
            match parent.get_kind() {
                // clang::EntityKind::TranslationUnit => {}
                // clang::EntityKind::Namespace if self.strip_namespaces => {}
                _ => {
                    let parent_name = parent.get_display_name().unwrap_or(
                        parent
                            .get_name_raw()
                            .map(|s| s.as_str().into())
                            .unwrap_or("unk".into()),
                    );
                    if !parent_name.contains("/") && !parent_name.contains("\\") {
                        names.insert(0, parent_name.into());
                    }
                }
            }
            cur = parent.get_lexical_parent();
        }

        names.join("::").into()
    }
}
