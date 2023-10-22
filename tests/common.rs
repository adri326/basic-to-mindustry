use std::path::Path;

pub fn read_basic_files(path: impl AsRef<Path>) -> impl Iterator<Item = (String, String)> {
    path.as_ref().read_dir().unwrap().filter_map(|entry| {
        let Ok(entry) = entry else {
                return None;
            };

        if entry
            .file_name()
            .into_string()
            .map(|name| name.ends_with(".mbas"))
            .unwrap_or(false)
        {
            let file_name = entry.file_name().into_string().unwrap();
            let file = std::fs::read_to_string(entry.path()).unwrap_or_else(|e| {
                panic!("Error opening {:?}: {:?}", file_name, e);
            });
            Some((file_name, file))
        } else {
            None
        }
    })
}
