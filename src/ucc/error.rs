#[derive(Debug)]
pub struct UccError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

pub type UccResult<T> = Result<T, UccError>;
