use std::fmt::Write;
use std::string::String;

use crate::types::*;
use crate::formatter::Formatter;

fn write_char(f: &mut Formatter, c: char, n: usize) {
    for _ in 0..n {
        f.write_char(c).unwrap();
    }
}


fn write_from<I>(fmt: &mut Formatter, f: I, n: usize) -> usize
    where I: Iterator<Item=char>
{
    // eexaust f or run out of n, return chars written
    if n == 0 {
        return 0;
    }
    let mut n_written: usize = 0;
    for c in f {
        fmt.write_char(c).unwrap();
        n_written += 1;
        if n_written == n {
            return n_written;
        }
    }
    n_written
}

impl<'a, 'b> Formatter<'a, 'b> {
    pub fn str(&mut self, s: &str) -> Result<()> {
        if !(self.ty() == None || self.ty() == Some('s')) {
            let mut msg = String::new();
            write!(msg, "Unknown format code {:?} for object of type 'str'", self.ty()).unwrap();
            return Err(FmtError::TypeError(msg));
        } else if self.alternate() {
            return Err(FmtError::TypeError("Alternate form (#) not allowed in string \
                                            format specifier".to_string()));
        } else if self.thousands() {
            return Err(FmtError::TypeError("Cannot specify ',' with 's'".to_string()));
        } else if self.sign().is_unspecified() {
            return Err(FmtError::TypeError("Sign not allowed in string format specifier"
                .to_string()));
        }
        self.str_unchecked(s)
    }

    pub fn str_unchecked(&mut self, s: &str) -> Result<()> {
        let fill = self.fill();
        let width = self.width();
        let precision = self.precision();
        // precision will limit length
        let len = match precision {
            Some(p) => if p < s.len() {
                p
            } else {
                s.len()
            },
            None => s.len(),
        };

        let mut chars = s.chars();
        let mut pad: usize = 0;
        if let Some(mut width) = width {
            if width > len {
                let align = self.align();
                match align {
                    Alignment::Left => pad = width - len,
                    Alignment::Center => {
                        width -= len;
                        pad = width / 2;
                        write_char(self, fill, pad);
                        pad += width % 2;
                    }
                    Alignment::Right => {
                        write_char(self, fill, width - len);
                    }
                    Alignment::Equal => return Err(FmtError::Invalid(
                        "sign aware zero padding and Align '=' not yet supported".to_string())),
                }
            }
        }
        write_from(self, &mut chars, len);
        write_char(self, fill, pad);
        Ok(())
    }
}

pub fn check(fmtstr: &str) -> Result<Vec<i32>> {
    let mut v: Vec<i32> = Vec::new();
    let mut out = String::new();
    let mut bytes_read: usize = 0;
    let mut opening_brace: usize = 0;
    let mut closing_brace: bool = false;
    let mut reading_fmt = false;
    let mut remaining = fmtstr;
    let mut index = 0;
    for c in fmtstr.chars() {
        bytes_read += c.len_utf8();
        if c == '{' {
            if reading_fmt && opening_brace == bytes_read - 2 {
                // found {{
                out.push(c);
                reading_fmt = false;
            } else if !reading_fmt {
                // found a first {
                reading_fmt = true;
                opening_brace = bytes_read - 1;
            } else {
                // found a { after finding an opening brace, error!
                out.clear();
                out.write_str("extra { found").unwrap();
                return Err(FmtError::Invalid(out));
            }
        } else if c == '}' {
            if !reading_fmt && !closing_brace {
                // found a '}' that isn't after a '{'
                closing_brace = true;
            } else if closing_brace {
                // found "}}"
                out.push(c);
                closing_brace = false;
            } else {
                // found a format string
                // discard before opening brace
                let (_, r) = remaining.split_at(opening_brace);

                // get the fmt pattern and remaining
                let (fmt_pattern, r) = r.split_at(bytes_read - opening_brace);
                remaining = r;

                // discard the braces
                let (_, fmt_pattern) = fmt_pattern.split_at(1);
                let (fmt_pattern, _) = fmt_pattern.split_at(fmt_pattern.len() - 1);
                // use the closure to write the formatted string
                let fmt = (Formatter::from_str(fmt_pattern, &mut out, index))?;
                if fmt.need_int_type() {
                    v.push(1);
                } else if fmt.need_float_type() {
                    v.push(2);
                } else {
                    v.push(3);
                }
                reading_fmt = false;
                bytes_read = 0;
            }
        } else if closing_brace {
            return Err(FmtError::Invalid("Single '}' encountered in format string".to_string()));
        } else if !reading_fmt {
            out.push(c)
        } // else we are currently reading a format string, so don't push
    }
    if closing_brace {
        return Err(FmtError::Invalid("Single '}' encountered in format string".to_string()));
    } else if reading_fmt {
        return Err(FmtError::Invalid("Expected '}' before end of string".to_string()));
    }
    out.shrink_to_fit();
    Ok(v)
}

pub fn strfmt_map<F>(fmtstr: &str, f: &F) -> Result<String>
    where F: Fn(Formatter) -> Result<()>
{
    let mut out = String::new();
    let mut bytes_read: usize = 0;
    let mut opening_brace: usize = 0;
    let mut closing_brace: bool = false;
    let mut reading_fmt = false;
    let mut remaining = fmtstr;
    let mut index = 0;
    for c in fmtstr.chars() {
        bytes_read += c.len_utf8();
        if c == '{' {
            if reading_fmt && opening_brace == bytes_read - 2 {
                // found {{
                out.push(c);
                reading_fmt = false;
            } else if !reading_fmt {
                // found a first {
                reading_fmt = true;
                opening_brace = bytes_read - 1;
            } else {
                // found a { after finding an opening brace, error!
                out.clear();
                out.write_str("extra { found").unwrap();
                return Err(FmtError::Invalid(out));
            }
        } else if c == '}' {
            if !reading_fmt && !closing_brace {
                // found a '}' that isn't after a '{'
                closing_brace = true;
            } else if closing_brace {
                // found "}}"
                out.push(c);
                closing_brace = false;
            } else {
                // found a format string
                // discard before opening brace
                let (_, r) = remaining.split_at(opening_brace);

                // get the fmt pattern and remaining
                let (fmt_pattern, r) = r.split_at(bytes_read - opening_brace);
                remaining = r;

                // discard the braces
                let (_, fmt_pattern) = fmt_pattern.split_at(1);
                let (fmt_pattern, _) = fmt_pattern.split_at(fmt_pattern.len() - 1);
                // use the closure to write the formatted string
                let fmt = (Formatter::from_str(fmt_pattern, &mut out, index))?;
                (f(fmt))?;

                index += 1;
                reading_fmt = false;
                bytes_read = 0;
            }
        } else if closing_brace {
            return Err(FmtError::Invalid("Single '}' encountered in format string".to_string()));
        } else if !reading_fmt {
            out.push(c)
        } // else we are currently reading a format string, so don't push
    }
    if closing_brace {
        return Err(FmtError::Invalid("Single '}' encountered in format string".to_string()));
    } else if reading_fmt {
        return Err(FmtError::Invalid("Expected '}' before end of string".to_string()));
    }
    out.shrink_to_fit();
    Ok(out)
}
