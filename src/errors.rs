pub struct EnvironmentError {
    pub message: String,
}

impl EnvironmentError {
    pub fn new(name: &str) -> Self {
        Self {
            message: format!("Undefined variable {name}").to_string(),
        }
    }
}
