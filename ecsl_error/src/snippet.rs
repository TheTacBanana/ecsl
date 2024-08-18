use ansi_term::Colour::{Blue, Red};
use ecsl_span::{LineData, SnippetLocation, Span};

#[derive(Debug, Clone)]
pub struct Snippet {
    location: SnippetLocation,
    full_span: Span,
    error_span: Span,
    lines: Vec<(LineData, String)>,
}

impl Snippet {
    pub fn from_source_span(
        location: SnippetLocation,
        full_span: Span,
        error_span: Span,
        lines: Vec<(LineData, String)>,
    ) -> Self {
        Self {
            location,
            full_span,
            error_span,
            lines,
        }
    }
}

impl std::fmt::Display for Snippet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let max_ln = &self
            .lines
            .iter()
            .max_by_key(|l| l.0.number().to_string())
            .unwrap()
            .0
            .number()
            .to_string()
            .len();

        let pipe_colour = Blue;
        let pipe_spacing = format!(" {: >1$}", " ", max_ln);
        let pipe = pipe_colour.paint("|");
        let underline_colour = Red;

        let mut underline = String::new();
        let mut underline = {
            let padding = *(self.error_span.start() - self.full_span.start()) as usize;
            let diff = *(self.error_span.end() - self.error_span.start());

            underline.push_str(&(0..padding).map(|_| " ").collect::<String>());
            underline.push_str(&(0..=diff).map(|_| "^").collect::<String>());

            println!("{:?}", underline);
            underline.drain(..)
        };

        writeln!(
            f,
            "{}{} {}",
            pipe_spacing,
            pipe_colour.paint("-->"),
            self.location
        )?;

        for (ln, string) in &self.lines {
            writeln!(
                f,
                " {} {}",
                pipe_colour.paint(format!("{: >1$} |", ln.number(), max_ln)),
                string.trim_end()
            )?;

            let underline = underline.by_ref().take(ln.length());

            writeln!(
                f,
                "{} {} {}",
                pipe_spacing,
                pipe,
                underline_colour.paint(underline.collect::<String>())
            )?;
        }

        Ok(())
    }
}
