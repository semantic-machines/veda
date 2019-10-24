use crc32fast::Hasher;
use std::fs::*;
use std::io::{Error, ErrorKind, Seek, SeekFrom, Write};
use std::path::Path;

pub struct ModuleInfo {
    _base_path: String,
    name: String,
    ff_info: File,
    is_ready: bool,
    is_writer: bool,
}

impl ModuleInfo {
    pub fn new(base_path: &str, info_name: &str, is_writer: bool) -> std::io::Result<ModuleInfo> {
        if !Path::new(&base_path).exists() {
            if let Err(e) = create_dir_all(base_path.to_owned()) {
                error!("queue:{} create path, err={}", info_name, e);
                return Err(e);
            }
        }

        let file_name_info = base_path.to_owned() + "/module-info/" + info_name + "_info";

        match OpenOptions::new().read(true).write(is_writer).create(true).open(file_name_info) {
            Ok(ff) => {
                return Ok(ModuleInfo {
                    _base_path: base_path.to_owned(),
                    name: info_name.to_owned(),
                    ff_info: ff,
                    is_ready: true,
                    is_writer: is_writer,
                });
            }
            Err(e) => Err(e),
        }
    }

    pub fn put_info(&mut self, op_id: i64, committed_op_id: i64) -> std::io::Result<()> {
        if !self.is_ready {
            return Err(Error::new(ErrorKind::Other, "module_info not ready"));
        }

        if !self.is_writer {
            return Err(Error::new(ErrorKind::Other, "module_info open as read only"));
        }

        //self.ff_info.set_len(0)?;
        self.ff_info.seek(SeekFrom::Start(0))?;

        let p = format!("{};{};{};", self.name, op_id, committed_op_id);
        let mut hash = Hasher::new();
        hash.update(p.as_bytes());

        if let Err(e) = self.ff_info.write(format!("{}{:X}\n", p, hash.finalize()).as_bytes()) {
            error!("fail put info push, set queue.ready = false, err={}", e);
            self.is_ready = false;
            return Err(e);
        }

        //self.ff_info.sync_data();
        info! ("op_id={}", op_id);

        Ok(())
    }
}
