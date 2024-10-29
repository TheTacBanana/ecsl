use cfgrammar::Span;
use ecsl_index::LineNumberColumn;

#[derive(Debug, Clone)]
pub struct LineNumbers {
    offsets: Vec<usize>,
}

impl From<&str> for LineNumbers {
    fn from(value: &str) -> Self {
        let mut line_start = 0;
        let mut offsets = Vec::new();
        for line in value.split("\n") {
            offsets.push(line_start);
            line_start += line.len() + "\n".len();
        }

        offsets.push(value.len());

        LineNumbers { offsets }
    }
}

impl LineNumbers {
    pub fn max_byte(&self) -> usize {
        *self.offsets.last().unwrap()
    }

    pub fn line_number(&self, pos: usize) -> usize {
        if pos > self.max_byte() {
            panic!("{pos:?} is outside the range {:?}", 0..self.max_byte())
        }

        let mut min = 0;
        let mut max = self.offsets.len();
        loop {
            let index = min + (max - min) / 2;

            if pos >= self.offsets[index] {
                if index == self.offsets.len() - 1 || pos < self.offsets[index + 1] {
                    return index;
                } else {
                    min = index;
                }
            } else {
                max = index;
            }
        }
    }

    pub fn column_number(&self, pos: usize) -> usize {
        let start = self.line_start(pos);
        pos - start
    }

    pub fn line_number_column(&self, pos: usize) -> LineNumberColumn {
        let line = self.line_number(pos);
        let start = self.offsets[self.line_number(pos)];
        LineNumberColumn::new(line, pos - start)
    }

    pub fn line_start(&self, pos: usize) -> usize {
        self.offsets[self.line_number(pos)]
    }

    pub fn line_end(&self, pos: usize) -> usize {
        let ln = self.line_number(pos) + 1;
        if ln >= self.offsets.len() {
            return self.max_byte();
        }
        self.offsets[ln]
    }

    /// Returns an inclusive range [s..=e]
    pub fn get_lines_from_span(&self, span: Span) -> (usize, usize) {
        let first_line = self.line_number(span.start());
        let last_line = self.line_number(span.end());

        (first_line, last_line)
    }

    /// Returns an inclusive range [s..=e]
    pub fn byte_slice(&self, line: usize) -> Option<(usize, usize)> {
        let len = self.offsets.len();
        if line >= len {
            return None;
        }

        if line == len - 1 {
            Some((self.offsets[len - 1], self.max_byte()))
        } else {
            Some((self.offsets[line], self.offsets[line + 1] - 1))
        }
    }
}

#[cfg(test)]
pub mod test {
    use super::LineNumbers;

    #[test]
    pub fn empty_file() {
        let string = "";

        let line_numbers = LineNumbers::from(string);

        assert_eq!(line_numbers.max_byte(), 0);
    }

    #[test]
    pub fn line_numbers() {
        let string = "hi\nhow\ndo";

        let line_numbers = LineNumbers::from(string);

        assert_eq!(line_numbers.offsets, vec![0, 3, 7, 9,]);

        assert_eq!(line_numbers.line_number(0), 0);

        assert_eq!(line_numbers.line_number(4), 1);
    }

    #[test]
    pub fn first_byte() {
        let string = "hi\nhow\ndo";

        let line_numbers = LineNumbers::from(string);

        let one = line_numbers.byte_slice(0).unwrap();
        let two = line_numbers.byte_slice(1).unwrap();
        let three = line_numbers.byte_slice(2).unwrap();

        assert_eq!(string.to_owned()[one.0..=one.1], "hi\n".to_owned());

        assert_eq!(string.to_owned()[two.0..=two.1], "how\n".to_owned());

        assert_eq!(string.to_owned()[three.0..=three.1], "do".to_owned());
    }
}
