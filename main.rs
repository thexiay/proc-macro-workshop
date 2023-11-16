#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use derive_builder::Builder;
type Option = ();
type Some = ();
type None = ();
type Result = ();
pub struct Command {
    executable: String,
}
pub struct CommandBuilder {
    executable: std::option::Option<String>,
}
impl CommandBuilder {
    fn executable(&mut self, executable: String) -> &mut Self {
        self.executable = std::option::Option::Some(executable);
        self
    }
}
impl Command {
    pub fn builder() -> CommandBuilder {
        CommandBuilder {
            executable: std::option::Option::None,
        }
    }
}
extern crate alloc;
impl CommandBuilder {
    pub fn build(&mut self) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
        if self.executable.is_none() {
            let err = {
                let res = ::alloc::fmt::format(
                    format_args!("{0} have not set!", "name"),
                );
                res
            };
            return std::result::Result::Err(err.into());
        }
        Ok(Command {
            executable: self.executable.clone().unwrap(),
        })
    }
}
fn main() {}
