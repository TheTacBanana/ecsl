use ecsl_span::{BytePos, LineNumber};


#[derive(Debug, Clone)]
pub struct LineNumbers {
    offsets: Vec<BytePos>,
    max_byte: BytePos,
}

impl From<&str> for LineNumbers {
    fn from(value: &str) -> Self {
        let mut line_start = 0;
        let mut offsets = Vec::new();
        for line in value.split("\n") {
            offsets.push(BytePos::new(line_start as u32));
            line_start += line.len() + "\n".len();
        }


        LineNumbers {
            offsets,
            max_byte: BytePos::new(line_start as u32 - 1),
        }
    }
}

impl LineNumbers {
    pub fn line_number(&self, pos: BytePos) -> LineNumber {
        if pos < BytePos::ZERO || pos > self.max_byte {
            panic!("{pos:?} is outside the range {:?}", 0..*self.max_byte)
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
}