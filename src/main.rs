use std::io::{self, Write};

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::new();
    let mut vm = DVM::new();

    print!("> ");
    let _ = stdout.flush();

    while stdin.read_line(&mut buf).is_ok() {
        let input = buf.trim();
        if input == "quit" {
            break;
        }

        match parse(input) {
            Ok((program, result_reg)) => {
                println!("{:?}", program);
                vm.load_program(program);
                vm.run();
                println!("{}", vm.registers[result_reg as usize]);
            }
            Err(e) => {
                println!("Error: {}", e);
            }
        }

        buf.clear();
        print!("> ");
        let _ = stdout.flush();
    }
}

#[derive(Debug)]
struct DVM {
    pc: usize,
    registers: [u32; 16],
    program: Vec<Insn>,
}

impl DVM {
    pub fn new() -> Self {
        Self {
            pc: 0,
            program: Vec::new(),
            registers: [0; 16],
        }
    }

    pub fn load_program(&mut self, program: Vec<Insn>) {
        self.program = program;
        self.pc = 0;
        self.registers = [0; 16];
    }

    pub fn run(&mut self) {
        while self.pc < self.program.len() {
            match &self.program[self.pc] {
                Insn::Add { lhs, rhs, dest } => {
                    let result =
                        self.registers[*lhs as usize].wrapping_add(self.registers[*rhs as usize]);
                    self.registers[*dest as usize] = result;
                    self.pc += 1;
                }
                Insn::Sub { lhs, rhs, dest } => {
                    let result =
                        self.registers[*lhs as usize].wrapping_sub(self.registers[*rhs as usize]);
                    self.registers[*dest as usize] = result;
                    self.pc += 1;
                }
                Insn::Mul { lhs, rhs, dest } => {
                    let result =
                        self.registers[*lhs as usize].wrapping_mul(self.registers[*rhs as usize]);
                    self.registers[*dest as usize] = result;
                    self.pc += 1;
                }
                Insn::Div { lhs, rhs, dest } => {
                    let result = self.registers[*lhs as usize] / self.registers[*rhs as usize];
                    self.registers[*dest as usize] = result;
                    self.pc += 1;
                }
                Insn::LoadImm { dest, value } => {
                    self.registers[*dest as usize] = *value;
                    self.pc += 1;
                }
                Insn::Return => break,
            }
        }
    }
}

#[derive(Debug)]
enum Insn {
    Add { lhs: u32, rhs: u32, dest: u32 },
    Sub { lhs: u32, rhs: u32, dest: u32 },
    Mul { lhs: u32, rhs: u32, dest: u32 },
    Div { lhs: u32, rhs: u32, dest: u32 },
    LoadImm { dest: u32, value: u32 },
    Return,
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(u32),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
}

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' => {
                chars.next();
            }
            '+' => {
                tokens.push(Token::Plus);
                chars.next();
            }
            '-' => {
                tokens.push(Token::Minus);
                chars.next();
            }
            '*' => {
                tokens.push(Token::Star);
                chars.next();
            }
            '/' => {
                tokens.push(Token::Slash);
                chars.next();
            }
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '0'..='9' => {
                let mut num = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch.is_ascii_digit() {
                        num.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(num.parse().unwrap()));
            }
            _ => return Err(format!("Unexpected character: {}", ch)),
        }
    }

    Ok(tokens)
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    next_reg: u32,
    insns: Vec<Insn>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            next_reg: 1,
            insns: Vec::new(),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    fn alloc_reg(&mut self) -> u32 {
        let reg = self.next_reg;
        self.next_reg += 1;
        reg
    }

    fn parse_expr(&mut self, min_prec: u32) -> Result<u32, String> {
        let mut lhs = self.parse_primary()?;

        while let Some(op) = self.peek() {
            let (prec, is_left_assoc) = match op {
                Token::Plus | Token::Minus => (1, true),
                Token::Star | Token::Slash => (2, true),
                _ => break,
            };

            if prec < min_prec {
                break;
            }

            let op = self.advance().unwrap();
            let next_min_prec = if is_left_assoc { prec + 1 } else { prec };
            let rhs = self.parse_expr(next_min_prec)?;

            let dest = self.alloc_reg();
            match op {
                Token::Plus => {
                    self.insns.push(Insn::Add { lhs, rhs, dest });
                }
                Token::Minus => {
                    self.insns.push(Insn::Sub { lhs, rhs, dest });
                }
                Token::Star => {
                    self.insns.push(Insn::Mul { lhs, rhs, dest });
                }
                Token::Slash => {
                    self.insns.push(Insn::Div { lhs, rhs, dest });
                }
                _ => unreachable!(),
            }
            lhs = dest;
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<u32, String> {
        match self.advance() {
            Some(Token::Number(n)) => {
                let reg = self.alloc_reg();
                self.insns.push(Insn::LoadImm {
                    dest: reg,
                    value: n,
                });
                Ok(reg)
            }
            Some(Token::LParen) => {
                let reg = self.parse_expr(0)?;
                match self.advance() {
                    Some(Token::RParen) => Ok(reg),
                    _ => Err("Expected ')'".to_string()),
                }
            }
            _ => Err("Expected number or '('".to_string()),
        }
    }
}

fn parse(input: &str) -> Result<(Vec<Insn>, u32), String> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let result_reg = parser.parse_expr(0)?;

    parser.insns.push(Insn::Return);
    Ok((parser.insns, result_reg))
}
