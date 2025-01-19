use cfgrammar::Span;
use ecsl_ast_derive::AST;
use std::str::FromStr;

#[derive(Debug, Clone, AST)]
pub struct Attributes {
    pub attributes: Vec<Attribute>,
}

impl Attributes {
    pub fn new() -> Self {
        Self {
            attributes: Vec::new(),
        }
    }

    pub fn from_vec(v: Vec<Attribute>) -> Self {
        Self { attributes: v }
    }

    pub fn has_attribute(&self, attr: &AttributeKind) -> bool {
        self.attributes.iter().find(|x| x.kind == *attr).is_some()
    }

    pub fn remove_attribute(&mut self, attr: &AttributeKind) {
        self.attributes.retain(|a| a.kind != *attr);
    }

    pub fn attributes(&self) -> impl Iterator<Item = &Attribute> {
        self.attributes.iter()
    }
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub span: Span,
    pub kind: AttributeKind,
}

impl Attribute {
    pub fn new(span: Span, kind: AttributeKind) -> Self {
        Self { span, kind }
    }

    pub fn is_unknown(&self) -> bool {
        match &self.kind {
            AttributeKind::Marker(marker) => *marker == AttributeMarker::Unknown,
            AttributeKind::Value(value, _) => *value == AttributeValue::Unknown,
        }
    }

    pub fn is_std_only(&self) -> bool {
        match &self.kind {
            AttributeKind::Marker(marker) => marker.is_std_only(),
            AttributeKind::Value(value, _) => value.is_std_only(),
        }
    }

    pub fn to_str(&self) -> &str {
        match self.kind {
            AttributeKind::Marker(ref marker) => marker.as_ref(),
            AttributeKind::Value(ref value, _) => value.as_ref(),
        }
    }
}

impl PartialEq for Attribute {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Clone)]
pub enum AttributeKind {
    Marker(AttributeMarker),
    Value(AttributeValue, u32),
}

impl PartialEq for AttributeKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Marker(l0), Self::Marker(r0)) => l0 == r0,
            (Self::Value(l0, _), Self::Value(r0, _)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, strum::EnumString, strum::AsRefStr)]
#[strum(serialize_all = "snake_case")]
pub enum AttributeMarker {
    Prelude,
    Copy,
    AllowCasing,

    #[default]
    #[strum(disabled)]
    Unknown,
}

impl AttributeMarker {
    pub fn from_string(s: &str) -> AttributeMarker {
        AttributeMarker::from_str(s).unwrap_or_default()
    }

    pub fn is_std_only(&self) -> bool {
        match self {
            AttributeMarker::Prelude => true,
            AttributeMarker::Copy => true,
            AttributeMarker::AllowCasing => false,
            AttributeMarker::Unknown => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, strum::EnumString, strum::AsRefStr)]
#[strum(serialize_all = "snake_case")]
pub enum AttributeValue {
    Builtin,

    #[default]
    #[strum(disabled)]
    Unknown,
}

impl AttributeValue {
    pub fn from_string(s: &str) -> AttributeValue {
        AttributeValue::from_str(s).unwrap_or_default()
    }

    pub fn is_std_only(&self) -> bool {
        match self {
            AttributeValue::Builtin => true,
            AttributeValue::Unknown => false,
        }
    }
}
