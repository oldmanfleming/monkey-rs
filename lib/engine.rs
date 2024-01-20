use anyhow::Result;

pub trait Engine {
    fn run(&mut self, input: &str) -> Result<String>;
}
