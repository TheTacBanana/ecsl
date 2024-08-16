use std::ops::{Range, RangeInclusive};

use ecsl_error::snippet::Snippet;
use ecsl_span::{BytePos, LineNumber, Span};

#[derive(Debug, Clone)]
pub struct LineNumbers {
    offsets: Vec<BytePos>,
}

impl From<&str> for LineNumbers {
    fn from(value: &str) -> Self {
        let mut line_start = 0;
        let mut offsets = Vec::new();
        for line in value.split("\n") {
            offsets.push(BytePos::new(line_start as u32));
            line_start += line.len() + "\n".len();
        }

        offsets.push(BytePos::new(value.len() as u32));

        LineNumbers { offsets }
    }
}

impl LineNumbers {
    pub fn max(&self) -> BytePos {
        BytePos::new(**self.offsets.last().unwrap() - 1)
    }

    pub fn line_number(&self, pos: BytePos) -> LineNumber {
        if pos < BytePos::ZERO || pos > self.max() {
            panic!("{pos:?} is outside the range {:?}", 0..*self.max())
        }

        let mut min = 0;
        let mut max = self.offsets.len();
        loop {
            let index = min + (max - min) / 2;

            if pos >= self.offsets[index] {
                if index == self.offsets.len() - 1 || pos < self.offsets[index + 1] {
                    return LineNumber::new(index as u32);
                } else {
                    min = index;
                }
            } else {
                max = index;
            }
        }
    }

    pub fn line_start(&self, pos: BytePos) -> BytePos {
        self.offsets[*self.line_number(pos) as usize]
    }

    pub fn line_end(&self, pos: BytePos) -> BytePos {
        let ln = (*self.line_number(pos) + 1) as usize;
        if ln  >= self.offsets.len() {
            return self.max()
        }
        BytePos::new(*self.offsets[ln] - 1)
    }

    /// Returns an inclusive range [s..=e]
    pub fn get_lines_from_span(&self, span: Span) -> (LineNumber, LineNumber) {
        let first_line = self.line_number(span.start());
        let last_line = self.line_number(span.end());

        (first_line, last_line)
    }

    /// Returns an exclusive range [s..e)
    pub fn byte_slice(&self, line: LineNumber) -> Option<Range<BytePos>> {
        let len = self.offsets.len();
        if *line as usize >= len {
            return None;
        }

        if *line as usize == len - 1 {
            Some(self.offsets[len - 1]..self.max())
        } else {
            Some(self.offsets[*line as usize]..self.offsets[*line as usize + 1])
        }
    }
}

#[cfg(test)]
pub mod test {

    use ecsl_span::{BytePos, LineNumber};

    use super::LineNumbers;

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
            string.to_owned()[*one.start as usize..*one.end as usize],
            "hi\n".to_owned()
        );

        assert_eq!(
            string.to_owned()[*two.start as usize..*two.end as usize],
            "how\n".to_owned()
        );

        assert_eq!(
            string.to_owned()[*three.start as usize..*three.end as usize],
            "do".to_owned()
        );
    }
}
