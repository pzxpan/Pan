package fs;

pub enum OpenOptions {
    Read,
    Write,
    Append,
    Truncate,
    Create,
    CreateNew,
}

pub fun read_file(path:string): string {}

pub fun write_file(path:string,content:string) {}

pub fun create_file(path:string) {}

pub fun append_file(path:string,content:string) {}

pub fun delete_file(path:string) {}

pub fun file_exists(path:string) :bool{}










