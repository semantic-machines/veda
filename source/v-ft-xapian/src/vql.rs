use std::fmt;
use std::str::from_utf8;

//  expression
//  "==", "!="
//  "===" : поиск в подклассах
//  "=*" : полнотекстовый поиск
//  "&&", "||",
//  ">", "<", ">=", "<=",

#[derive(Debug, PartialEq)]
pub enum Decor {
    NONE,
    QUOTED,
    RANGE,
}

#[derive(Debug)]
pub struct TTA {
    pub(crate) op: String,
    pub(crate) token_decor: Decor,
    pub(crate) l: Option<Box<TTA>>,
    pub(crate) r: Option<Box<TTA>>,
    count: i32,
}

impl fmt::Display for TTA {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "op={}", self.op)?;
        if let Some(l) = &self.l {
            write!(f, "\nL={}", format!("{}", l))?;
        }
        if let Some(r) = &self.r {
            write!(f, "\nR={}", format!("{}", r))?;
        }
        Ok(())
    }
}

impl TTA {
    pub fn new(op: &str, l: Option<TTA>, r: Option<TTA>, token_decor: Decor) -> Self {
        let l1 = if let Some(ll) = l {
            Some(Box::new(ll))
        } else {
            None
        };

        let r1 = if let Some(rr) = r {
            Some(Box::new(rr))
        } else {
            None
        };

        Self {
            op: op.to_owned(),
            token_decor,
            l: l1,
            r: r1,
            count: 0,
        }
    }

    pub fn parse_expr(src: &str) -> Option<TTA> {
        let mut st: Vec<TTA> = vec![];
        let mut op: Vec<&str> = vec![];

        let s = src.as_bytes();

        let mut i = 0;
        while i < s.len() {
            if !delim(s[i]) {
                if s[i] == b'(' {
                    op.push("(");
                } else if s[i] == b')' {
                    while !op.is_empty() {
                        if let Some(last) = op.last() {
                            if *last == "(" {
                                break;
                            }
                            if let Some(p) = op.pop() {
                                process_op(&mut st, p);
                            }
                        }
                    }
                    op.pop();
                } else {
                    let mut e = i + 2;
                    if e >= s.len() {
                        e = s.len() - 1;
                    }

                    if s[i] == b'=' && s[e] == b'=' {
                        e += 1;
                    }

                    let cur_op = is_op(&s[i..e]);
                    if !cur_op.is_empty() {
                        while !op.is_empty() {
                            if let Some(last) = op.last() {
                                if priority(last) < priority(cur_op) {
                                    break;
                                }
                            }
                            if let Some(p) = op.pop() {
                                process_op(&mut st, p);
                            }
                        }
                        op.push(cur_op);
                        i += cur_op.len() - 1;
                    } else {
                        let operand;

                        while i < s.len() && s[i] == b' ' {
                            i += 1;
                        }

                        let cur_tag = s[i];

                        match cur_tag {
                            b'\'' | b'`' | b'[' => {
                                let closed_tag = if cur_tag == b'\'' || cur_tag == b'`' {
                                    cur_tag
                                } else {
                                    b']'
                                };

                                i += 1;
                                let bp = i;
                                while i < s.len() && s[i] != closed_tag {
                                    i += 1;
                                }

                                operand = from_utf8(&s[bp..i]);

                                if let Ok(op) = operand {
                                    if cur_tag == b'[' {
                                        st.push(TTA::new(op, None, None, Decor::RANGE));
                                    } else {
                                        st.push(TTA::new(op, None, None, Decor::QUOTED));
                                    }
                                }
                            }
                            _ => {
                                // no quote
                                let bp = i;
                                while i < s.len()
                                    && s[i] != b' '
                                    && s[i] != b'&'
                                    && s[i] != b'|'
                                    && s[i] != b'='
                                    && s[i] != b'<'
                                    && s[i] != b'>'
                                    && s[i] != b'!'
                                    && s[i] != b'-'
                                    && s[i] != b' '
                                {
                                    i += 1;
                                }

                                let ep = i;

                                if s[ep - 1] == b'(' || s[ep - 1] == b')' {
                                    //ep = i - 1;

                                    if s[i - 2] != b'\'' {
                                        operand = from_utf8(&s[bp..i - 1]);
                                    } else {
                                        operand = from_utf8(&s[bp..i - 2]);
                                    }

                                    i -= 2;
                                } else {
                                    operand = from_utf8(&s[bp..i]);
                                }

                                if let Ok(op) = operand {
                                    st.push(TTA::new(op, None, None, Decor::NONE));
                                }
                            }
                        }
                    }
                }
            }
            i += 1;
        }
        while !op.is_empty() {
            if let Some(operand) = op.pop() {
                process_op(&mut st, operand);
            }
        }

        st.pop()
    }
}

fn delim(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

fn process_op(st: &mut Vec<TTA>, op: &str) {
    let r = st.pop();
    let l = st.pop();

    match op {
        "<" | ">" | "==" | "===" | "!=" | "=*" | "=+" | ">=" | "<=" | "||" | "&&" => {
            st.push(TTA::new(op, l, r, Decor::NONE));
        }
        _ => {}
    }
}

fn is_op(c: &[u8]) -> &str {
    match c.len() {
        1 => {
            if c[0] == b'>' {
                return ">";
            }

            if c[0] == b'<' {
                return "<";
            }
        }
        2 => match (c[0], c[1]) {
            (b'>', b'=') => return ">=",
            (b'<', b'=') => return "<=",
            (b'=', b'=') => return "==",
            (b'!', b'=') => return "!=",
            (b'=', b'*') => return "=*",
            (b'=', b'+') => return "=+",
            (b'|', b'|') => return "||",
            (b'&', b'&') => return "&&",
            _ => {
                if c[0] == b'>' && c[1] != b'=' {
                    return ">";
                }

                if c[0] == b'<' && c[1] != b'=' {
                    return "<";
                }
            }
        },
        3 => {
            if c[0] == b'=' && c[1] == b'=' && c[2] == b'=' {
                return "===";
            }
        }
        _ => {
            return "";
        }
    }
    ""
}

fn priority(op: &str) -> i32 {
    if op == "<" || op == "<=" || op == ">" || op == "=>" {
        return 4;
    }

    if op == "==" || op == "!=" || op == "=*" || op == "=+" || op == "===" {
        return 3;
    }

    if op == "&&" {
        return 2;
    }

    if op == "||" {
        return 1;
    }

    -1
}
