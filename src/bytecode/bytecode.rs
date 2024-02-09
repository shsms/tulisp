use super::Instruction;

pub(crate) struct Bytecode {
    pub(crate) global: Vec<Instruction>,
}

impl Bytecode {
    pub(crate) fn new(instructions: Vec<Instruction>) -> Self {
        Self {
            global: instructions,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn print(&self) {
        println!("start:");
        for (i, instr) in self.global.iter().enumerate() {
            println!("{:<40}   # {}", instr.to_string(), i);
        }
    }
}
