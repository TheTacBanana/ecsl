use cfgrammar::Span;
use ecsl_ast::expr::Literal;
use log::debug;

pub struct Constant {
    pub span: Span,
    pub kind: ConstantKind,
}

impl Constant {
    pub fn from_literal(lit: Literal, s: &str, span: Span) -> Constant {
        Constant {
            span,
            kind: match lit {
                Literal::Int => ConstantKind::Int(s.parse().unwrap()),
                Literal::Float => ConstantKind::Float(s.parse().unwrap()),
                Literal::Bool => ConstantKind::Bool(match s {
                    "true" => true,
                    "false" => false,
                    _ => panic!("Cannot parse {:?} as bool", s),
                }),
                Literal::Char => ConstantKind::Char({
                    let c = &s.to_string()[1..(s.len() - 1)];
                    let mut chars = c.chars();
                    match (chars.next().unwrap(), chars.next()) {
                        ('\\', Some('t')) => '\t',
                        ('\\', Some('n')) => '\n',
                        ('\\', Some('r')) => '\r',
                        ('\\', Some('\'')) => '\'',
                        ('\\', Some('\"')) => '\"',
                        ('\\', Some('\\')) => '\\',
                        (c, None) => c,
                        _ => panic!(),
                    }
                }),
                Literal::String => ConstantKind::String({
                    debug!("TODO: String escape codes");
                    s.to_string()[1..(s.len() - 1)].as_bytes().to_vec()
                }),
            },
        }
    }
}

pub enum ConstantKind {
    Int(i32),
    Float(f32),
    Bool(bool),
    Char(char),
    String(Vec<u8>),
}
