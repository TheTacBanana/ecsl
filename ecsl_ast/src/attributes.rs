use ecsl_ast_derive::AST;

#[derive(Debug, Clone, AST)]
pub struct Attributes {
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attribute {
    Marker(String),
}

impl Attributes {
    pub fn from_vec(v: Vec<Attribute>) -> Self {
        Self { attributes: v }
    }

    pub fn get_attribute(&self, attr: &Attribute) -> bool {
        self.attributes.contains(attr)
    }

    pub fn attributes(&self) -> impl Iterator<Item = &Attribute> {
        self.attributes.iter()
    }
}
