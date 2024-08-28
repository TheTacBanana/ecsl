use ecsl_span::{index::{BytePos, LineNumber}, LineNumberColumn, Span};

#[derive(Debug, Clone)]
pub struct LineNumbers {
    offsets: Vec<BytePos>,
}

impl From<&str> for LineNumbers {
    fn from(value: &str) -> Self {
        let mut line_start = 0;
        let mut offsets = Vec::new();
        for line in value.split("\n") {
            offsets.push(BytePos::new(line_start));
            line_start += line.len() + "\n".len();
        }

        offsets.push(BytePos::new(value.len()));

        LineNumbers { offsets }
    }
}

impl LineNumbers {
    pub fn max_byte(&self) -> BytePos {
        BytePos::new(self.offsets.last().unwrap().inner())
    }

    pub fn line_number(&self, pos: BytePos) -> LineNumber {
        if pos > self.max_byte() {
            panic!("{pos:?} is outside the range {:?}", 0..self.max_byte().inner())
        }

        let mut min = 0;
        let mut max = self.offsets.len();
        loop {
            let index = min + (max - min) / 2;

            if pos >= self.offsets[index] {
                if index == self.offsets.len() - 1 || pos < self.offsets[index + 1] {
                    return LineNumber::new(index);
                } else {
                    min = index;
                }
            } else {
                max = index;
            }
        }
    }

    pub fn column_number(&self, pos: BytePos) -> usize {
        let start = self.line_start(pos);
        (pos - start).inner()
    }

    pub fn line_number_column(&self, pos: BytePos) -> LineNumberColumn {
        let line = self.line_number(pos);
        let start = self.offsets[self.line_number(pos).inner()];
        LineNumberColumn::new(line, (pos - start).inner())
    }

    pub fn line_start(&self, pos: BytePos) -> BytePos {
        self.offsets[self.line_number(pos).inner()]
    }

    pub fn line_end(&self, pos: BytePos) -> BytePos {
        let ln = self.line_number(pos).inner() + 1;
        if ln  >= self.offsets.len() {
            return self.max_byte()
        }
        BytePos::new(self.offsets[ln].inner())
    }

    /// Returns an inclusive range [s..=e]
    pub fn get_lines_from_span(&self, span: Span) -> (LineNumber, LineNumber) {
        let first_line = self.line_number(span.start());
        let last_line = self.line_number(span.end());

        (first_line, last_line)
    }

    /// Returns an inclusive range [s..=e]
    pub fn byte_slice(&self, line: LineNumber) -> Option<(BytePos, BytePos)> {
        let len = self.offsets.len();
        if line.inner() >= len {
            return None;
        }

        if line.inner() == len - 1 {
            Some((self.offsets[len - 1], self.max_byte()))
        } else {
            Some((self.offsets[line.inner()], self.offsets[line.inner() + 1] - BytePos::ONE))
        }
    }
}

#[cfg(test)]
pub mod test {

    use ecsl_span::index::{BytePos, LineNumber};

    use super::LineNumbers;

    #[test]
    pub fn empty_file() {
        let string = "";

        let line_numbers = LineNumbers::from(string);

        assert_eq!(line_numbers.max_byte(), BytePos::ZERO);
    }

    #[test]
    pub fn line_numbers() {
        let string = "hi\nhow\ndo";

        let line_numbers = LineNumbers::from(string);

        assert_eq!(
            line_numbers.offsets,
            vec![
                BytePos::new(0),
                BytePos::new(3),
                BytePos::new(7),
                BytePos::new(9),
            ]
        );

        assert_eq!(
            line_numbers.line_number(BytePos::new(0)),
            LineNumber::new(0)
        );

        assert_eq!(
            line_numbers.line_number(BytePos::new(4)),
            LineNumber::new(1)
        );
    }

    #[test]
    pub fn first_byte() {
        let string = "hi\nhow\ndo";

        let line_numbers = LineNumbers::from(string);

        let one = line_numbers.byte_slice(LineNumber::new(0)).unwrap();
        let two = line_numbers.byte_slice(LineNumber::new(1)).unwrap();
        let three = line_numbers.byte_slice(LineNumber::new(2)).unwrap();

        assert_eq!(
            string.to_owned()[one.0.inner()..=one.1.inner()],
            "hi\n".to_owned()
        );

        assert_eq!(
            string.to_owned()[two.0.inner()..=two.1.inner()],
            "how\n".to_owned()
        );

        assert_eq!(
            string.to_owned()[three.0.inner()..=three.1.inner()],
            "do".to_owned()
        );
    }
}
