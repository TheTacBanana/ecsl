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

    pub fn has_attribute(&self, attr: &Attribute) -> bool {
        self.attributes.contains(attr)
    }

    pub fn remove_attribute(&mut self, attr: &Attribute) {
        self.attributes.retain(|a| a != attr);
    }

    pub fn attributes(&self) -> impl Iterator<Item = &Attribute> {
        self.attributes.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Marker(AttributeMarker),
    Value(AttributeValue, u32),
}

impl Attribute {
    pub fn is_unknown(&self) -> bool {
        match self {
            Attribute::Marker(marker) => *marker == AttributeMarker::Unknown,
            Attribute::Value(value, _) => *value == AttributeValue::Unknown,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, strum::EnumString)]
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, strum::EnumString)]
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
}
