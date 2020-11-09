use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use std::rc::Rc;

pub struct FileCache {
    import_path: Vec<PathBuf>,
    by_name: HashMap<String, usize>,
    by_path: HashMap<PathBuf, usize>,
    files: Vec<Rc<String>>,
}

impl Default for FileCache {
    fn default() -> Self {
        FileCache::new()
    }
}

impl FileCache {
    pub fn new() -> Self {
        FileCache {
            import_path: Vec::new(),
            by_name: HashMap::new(),
            by_path: HashMap::new(),
            files: Vec::new(),
        }
    }

    pub fn add_import_path(&mut self, path: PathBuf) {
        self.import_path.push(path);
    }

    pub fn set_file_contents(&mut self, filename: String, contents: String) {
        let pos = self.files.len();
        self.files.push(Rc::new(contents));
        self.by_name.insert(filename, pos);
    }

    pub fn get_file_contents(&mut self, filename: &str) -> Rc<String> {
        let pos = self
            .by_name
            .get(filename)
            .expect("file should exist in cache already");

        self.files[*pos].clone()
    }

    pub fn populate_cache(&mut self, filename: &str) -> Result<(), String> {
        if self.by_name.contains_key(filename) {
            return Ok(());
        }

        if let Some(path) = self.resolve_file(filename) {
            if let Some(pos) = self.by_path.get(&path) {
                self.by_name.insert(filename.to_string(), *pos);
                return Ok(());
            }
            let mut f = match File::open(&path) {
                Err(err_info) => {
                    return Err(format!(
                        "无法打开 ‘{}’: {}",
                        filename,
                        err_info.to_string()
                    ));
                }
                Ok(file) => file,
            };

            let mut contents = String::new();
            if let Err(e) = f.read_to_string(&mut contents) {
                return Err(format!(
                    "读取‘{}’失败: {}",
                    filename,
                    e.to_string()
                ));
            }

            let pos = self.files.len();

            self.files.push(Rc::new(contents));

            self.by_name.insert(filename.to_string(), pos);
            self.by_path.insert(path, pos);

            Ok(())
        } else {
            Err(format!("找不到 ‘{}’", filename))
        }
    }
    fn resolve_file(&self, filename: &str) -> Option<PathBuf> {
        let path = PathBuf::from(filename);
        for i in &self.import_path {
            if let Ok(p) = i.join(path.clone()).canonicalize() {
                if p.starts_with(i) {
                    return Some(p);
                }
            }
        }
        None
    }
}
