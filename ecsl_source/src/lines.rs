use crate::pos::{BytePos, LineNumber};

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
            offsets.push(BytePos(line_start as u32));
            line_start += line.len() + "\n".len();
        }


        LineNumbers {
            offsets,
            max_byte: BytePos(line_start as u32 - 1),
        }
    }
}

impl LineNumbers {
    pub fn line_number(&self, pos: BytePos) -> LineNumber {
        if pos < BytePos::ZERO || pos > self.max_byte {
            panic!("{pos:?} is outside the range {:?}", 0..self.max_byte.0)
        }

        let mut min = 0;
        let mut max = self.offsets.len();
        loop {
            let index = min + (max - min) / 2;

            if pos >= self.offsets[index] {
                if index == self.offsets.len() - 1 || pos < self.offsets[index + 1] {
                    return LineNumber(index as u32);
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
    use crate::pos::{BytePos, LineNumber};

    use super::LineNumbers;

    #[test]
    pub fn line_numbers() {
        let string = "hi\nhow\ndo";

        let line_numbers = LineNumbers::from(string);

        assert_eq!(
            line_numbers.offsets,
            vec![
                BytePos(0),
                BytePos(3),
                BytePos(7),
            ]
        );

        assert_eq!(
            line_numbers.line_number(BytePos(0)),
            LineNumber(0)
        );

        assert_eq!(
            line_numbers.line_number(BytePos(4)),
            LineNumber(1)
        );
    }
}