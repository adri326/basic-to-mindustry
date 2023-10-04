use crate::cursor::Cursor;

#[derive(Clone, Debug, PartialEq, Copy, Default)]
pub struct Position {
    start: (usize, usize),
    end: Option<(usize, usize)>,
}

const MARGIN: usize = 2;

impl Position {
    pub fn point(line: usize, ch: usize) -> Self {
        Self {
            start: (line, ch),
            end: None,
        }
    }

    pub fn span_ch(line: usize, ch: usize, len: usize) -> Self {
        Self {
            start: (line, ch),
            end: Some((line, ch + len)),
        }
    }

    pub fn underline<W: std::io::Write>(
        self,
        raw: &str,
        stream: &mut W,
    ) -> Result<(), std::io::Error> {
        let start_line = self.start.0;
        let end_line = self.end.map(|pair| pair.0).unwrap_or(start_line);

        for (line_index, line) in raw.lines().enumerate() {
            if line_index + MARGIN < start_line || line_index > end_line + MARGIN {
                continue;
            }

            if line_index >= start_line && line_index <= end_line {
                writeln!(stream, "> {}", line)?;

                if start_line == end_line {
                    let len = if let Some((_, end_ch)) = self.end {
                        end_ch - self.start.1
                    } else {
                        0
                    };

                    write!(stream, "  ")?;
                    for _ in 0..self.start.1 {
                        write!(stream, " ")?;
                    }
                    write!(stream, "^")?;
                    if len > 1 {
                        for _ in 1..len {
                            write!(stream, "~")?;
                        }
                    }
                    writeln!(stream,)?;
                } else {
                    if line_index == start_line {
                        write!(stream, "  ")?;
                        for _ in 0..self.start.1 {
                            write!(stream, " ")?;
                        }
                        for _ in self.start.1..line.len() {
                            write!(stream, "~")?;
                        }
                        writeln!(stream,)?;
                    } else if line_index == end_line {
                        write!(stream, "  ")?;
                        for _ in 0..self.end.unwrap().1 {
                            write!(stream, "~")?;
                        }
                        writeln!(stream,)?;
                    } else {
                        write!(stream, "  ")?;
                        for _ in 0..line.len() {
                            write!(stream, "~")?;
                        }
                        writeln!(stream,)?;
                    }
                }
            } else {
                writeln!(stream, "* {}", line)?;
            }
        }

        Ok(())
    }

    /// Returns the position pointing to the end of the current spanning position
    pub fn at_end(&self) -> Self {
        Self {
            start: self.end.unwrap_or(self.start),
            end: None,
        }
    }

    /// Returns a position spanning from the beginning of `self` to the end of `other`
    pub fn until(&self, other: Self) -> Self {
        Self {
            start: self.start,
            end: Some(other.end.unwrap_or(other.start)),
        }
    }
}

pub(crate) fn drop_position<T>(list: &[(T, Position)]) -> Vec<&T> {
    list.iter().map(|(lhs, _rhs)| lhs).collect()
}

pub(crate) fn last_position<T>(cursor: &Cursor<'_, (T, Position)>) -> Position {
    let data = cursor.full_data();
    let offset = cursor.offset();

    match data.get(offset.saturating_sub(1)) {
        Some((_, position)) => *position,
        None => data
            .last()
            .map(|pair| pair.1)
            .unwrap_or(Position::default()),
    }
}
