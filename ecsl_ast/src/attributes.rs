use cfgrammar::Span;
use ecsl_ast_derive::AST;
use std::str::FromStr;

#[derive(Debug, Clone, AST, Eq, Hash)]
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

    pub fn get_marker(&self, marker: AttributeMarker) -> bool {
        self.attributes()
            .find(|&x| match x.kind {
                AttributeKind::Marker(m) => m == marker,
                _ => false,
            })
            .is_some()
    }

    pub fn get_value(&self, value: AttributeValue) -> Option<usize> {
        self.attributes().find_map(|x| match x.kind {
            AttributeKind::Value(v, i) if v == value => Some(i),
            _ => None,
        })
    }

    pub fn remove_attribute(&mut self, attr: &AttributeKind) {
        self.attributes.retain(|a| a.kind != *attr);
    }

    pub fn attributes(&self) -> impl Iterator<Item = &Attribute> {
        self.attributes.iter()
    }
}

#[derive(Debug, Clone, Eq, Hash)]
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

#[derive(Debug, Clone, Eq, Hash)]
pub enum AttributeKind {
    Marker(AttributeMarker),
    Value(AttributeValue, usize),
}

impl std::fmt::Display for AttributeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttributeKind::Marker(attribute_marker) => write!(f, "{}", attribute_marker.as_ref()),
            AttributeKind::Value(attribute_value, n) => {
                write!(f, "{}({})", attribute_value.as_ref(), n)
            }
        }
    }
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
    Terminator,
    RequireCompGenerics,

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
            AttributeMarker::Terminator => true,
            AttributeMarker::RequireCompGenerics => true,
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
