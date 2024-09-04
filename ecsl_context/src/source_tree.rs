use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    hash::Hash,
    path::PathBuf,
};

use ecsl_source::SourceFile;
use ecsl_span::{generate_index_type, index::SourceFileID};

/// A trie which stores Source Files and associated data
#[derive(Debug)]
pub struct SourceTree<T: Sized> {
    // Stored Data
    files: Vec<SourceFile>,
    assoc_data: Vec<T>,

    // Path Data
    mappings: Vec<HashMap<String, NodeOrData>>,
}

generate_index_type!(NodeID);

#[derive(Debug, Clone, Copy)]
pub enum NodeOrData {
    // Link to the next node in the path
    Node(NodeID),
    // Link to the source file with that name
    Source(SourceFileID),
    // Link to both the next node and source file if they have the same name
    NodeAndSource(NodeID, SourceFileID),
}

impl<T: Debug> SourceTree<T> {
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            assoc_data: Vec::new(),
            mappings: vec![
                HashMap::new()
            ],
        }
    }

    pub fn insert_from_root(
        &mut self,
        path: &PathBuf,
        source: SourceFile,
        data: T,
    ) -> SourceFileID {
        let source_id = SourceFileID::new(self.files.len());
        self.files.push(source);
        self.assoc_data.push(data);

        let mut path = path.clone();
        path.set_extension("");
        let mut path_segements = path
            .iter()
            .map(|osstr| osstr.to_str().unwrap().to_owned())
            .collect::<Vec<_>>();
        let drain = path_segements.drain(..).enumerate();
        let len = drain.len();

        let mut cur_node = NodeID::ZERO;
        for (i, segement) in drain {
            // Boolean for if last
            let last = i == len - 1;

            // Get next node as it might be needed
            let next_node = self.next_node();

            // Ensure the `cur_node` exists
            if cur_node.inner() >= self.mappings.len() {
                self.mappings
                    .extend((self.mappings.len()..=cur_node.inner()).map(|_| HashMap::default()))
            }
            let node = self.mappings.get_mut(cur_node.inner()).unwrap();

            // If last then create Source Node
            if last {
                // Perform actions depending on the contents of the entry
                match node.entry(segement) {
                    Entry::Occupied(mut o) => match o.get_mut() {
                        NodeOrData::Node(id) => {
                            *o.get_mut() = NodeOrData::NodeAndSource(*id, source_id);
                        }
                        e => panic!("Encountered {:?} when inserting {:?}", e, source_id),
                    },
                    Entry::Vacant(v) => {
                        v.insert(NodeOrData::Source(source_id));
                    }
                }

            // Else extend trie
            } else {
                // Perform actions depending on the contents of the entry
                match node.entry(segement) {
                    Entry::Occupied(mut o) => match o.get_mut() {
                        NodeOrData::Node(id) | NodeOrData::NodeAndSource(id, _) => {
                            cur_node = *id;
                        }
                        NodeOrData::Source(id) => {
                            *o.get_mut() = NodeOrData::NodeAndSource(next_node, *id);
                            cur_node = next_node;
                        }
                    },
                    Entry::Vacant(v) => {
                        v.insert(NodeOrData::Node(next_node));
                        cur_node = next_node;
                    }
                }
            }
        }

        source_id
    }

    pub fn get_source_by_id(&self, id: SourceFileID) -> &SourceFile {
        todo!()
    }

    pub fn get_source_by_path(&self, path: &PathBuf) -> &SourceFile {
        todo!()
    }

    fn next_node(&self) -> NodeID {
        NodeID::new(self.mappings.len())
    }

    fn create_edge(&mut self, from: NodeID, edge: String) -> NodeID {
        // self.
        todo!()
    }
}

#[cfg(test)]
pub mod test {
    use std::path::PathBuf;

    use ecsl_diagnostics::Diagnostics;
    use ecsl_source::SourceFile;
    use ecsl_span::index::SourceFileID;

    use super::SourceTree;

    #[test]
    pub fn navigate() {
        let mut diag = Diagnostics::new();
        let mut tree: SourceTree<()> = SourceTree::new();

        let path = "folder/file.ecsl".into();
        let source =
            SourceFile::from_string(&mut diag, "this is a file".to_owned(), SourceFileID::new(0));
        let id = tree.insert_from_root(&path, source, ());

        let path = "folder/folder2/file.ecsl".into();
        let source = SourceFile::from_string(
            &mut diag,
            "this is also a file".to_owned(),
            SourceFileID::new(1),
        );
        let id = tree.insert_from_root(&path, source, ());

        println!("{:#?}", tree);

        panic!()
    }
}
