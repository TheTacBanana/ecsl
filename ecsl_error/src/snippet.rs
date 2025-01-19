use ansi_term::{Colour, Colour::Blue};
use cfgrammar::Span;
use ecsl_index::LineNumberColumn;
use std::fmt::Write;

use crate::ErrorLevel;

#[derive(Debug, Clone)]
pub struct Snippet {
    number_padding: u32,
    formatted_string: String,
    lnc: LineNumberColumn,
}

impl Snippet {
    const PIPE_COLOUR: Colour = Blue;
    const UNDERLINE_CHAR: &'static str = "^";

    pub fn from_source_span(
        level: ErrorLevel,
        full_span: Span,
        error_span: Span,
        lines: Vec<(usize, String)>,
        lnc: LineNumberColumn,
    ) -> Result<Self, std::fmt::Error> {
        let number_padding = Self::get_number_padding(&lines);
        let mut formatted_string = String::new();

        let pipe_spacing = format!(" {: >1$}", " ", number_padding);
        let pipe = Self::PIPE_COLOUR.paint("|");
        let underline_colour = level.colour();

        let mut underline = String::new();
        let mut underline = {
            let padding = error_span.start() - full_span.start();
            let diff = error_span.end() - error_span.start();

            underline.push_str(&(0..padding).map(|_| " ").collect::<String>());
            underline.push_str(
                &(0..diff)
                    .map(|_| Snippet::UNDERLINE_CHAR)
                    .collect::<String>(),
            );
            underline.drain(..)
        };

        for (ln, string) in lines {
            writeln!(
                &mut formatted_string,
                " {} {}",
                Self::PIPE_COLOUR.paint(format!("{: >1$} |", ln, number_padding)),
                string.trim_end()
            )?;

            let underline = underline.by_ref().take(string.len());

            writeln!(
                &mut formatted_string,
                "{} {} {}",
                pipe_spacing,
                pipe,
                underline_colour.paint(underline.collect::<String>())
            )?;
        }
        let last = underline.collect::<String>();
        if !last.is_empty() {
            panic!("Underline is not empty");
        }

        Ok(Self {
            number_padding: number_padding as u32,
            formatted_string,
            lnc,
        })
    }

    fn get_number_padding(lines: &Vec<(usize, String)>) -> usize {
        lines
            .iter()
            .map(|l| l.0)
            .max_by_key(|l| l.to_string())
            .unwrap_or_default()
            .to_string()
            .len()
    }

    pub fn number_padding(&self) -> u32 {
        self.number_padding
    }

    pub fn lnc(&self) -> LineNumberColumn {
        self.lnc
    }
}

impl std::fmt::Display for Snippet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.formatted_string)?;
        Ok(())
    }
}
