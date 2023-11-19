use bibe_asm::asm::object::Object;
/* Copyright 2023 Robert Zieba, see LICENSE file for full license. */
use proc_macro::TokenStream as RTokenStream;
use proc_macro2::TokenStream;
use quote::{ quote, ToTokens };
use bibe_instr as isa;
use bibe_asm::asm::{
	self as asm,
	emitter::{
		Emitter,
		Result,
	}
};
use bibe_asm::parser::{ instruction, tokenize };

fn stringify(tokens: &RTokenStream) -> String {
	tokens.to_string()
}

struct BinOp(isa::BinOp);
impl ToTokens for BinOp{
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.extend(match self.0 {
			isa::BinOp::Add => quote! { bibe_instr::BinOp::Add },
			isa::BinOp::Sub => quote! { bibe_instr::BinOp::Sub },

			isa::BinOp::Mul => quote! { bibe_instr::BinOp::Mul },
			isa::BinOp::Div => quote! { bibe_instr::BinOp::Div },
			isa::BinOp::Mod => quote! { bibe_instr::BinOp::Mod },

			isa::BinOp::And => quote! { bibe_instr::BinOp::And },
			isa::BinOp::Or => quote! { bibe_instr::BinOp::Or },
			isa::BinOp::Xor => quote! { bibe_instr::BinOp::Xor },

			isa::BinOp::Shl => quote! { bibe_instr::BinOp::Shl },
			isa::BinOp::Shr => quote! { bibe_instr::BinOp::Shr },
			isa::BinOp::Asl => quote! { bibe_instr::BinOp::Asl },
			isa::BinOp::Asr => quote! { bibe_instr::BinOp::Asr },
			isa::BinOp::Rol => quote! { bibe_instr::BinOp::Rol },
			isa::BinOp::Ror => quote! { bibe_instr::BinOp::Ror },

			isa::BinOp::Not => quote! { bibe_instr::BinOp::Not },
			isa::BinOp::Neg => quote! { bibe_instr::BinOp::Neg },
			isa::BinOp::Cmp => quote! { bibe_instr::BinOp::Cmp },
		});
	}
}

struct Register(isa::Register);
impl ToTokens for Register {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let r = self.0.as_u8();
		tokens.extend(quote! { bibe_instr::Register::new(#r).unwrap() })
	}
}

struct RrrInstruction<'a>(&'a isa::rrr::Instruction);
impl<'a> ToTokens for RrrInstruction<'a> {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let op = BinOp(self.0.op);
		let dest = Register(self.0.dest);
		let lhs = Register(self.0.lhs);
		let rhs = Register(self.0.rhs);

		tokens.extend(quote! {
			bibe_instr::Instruction::Rrr(bibe_instr::rrr::Instruction {
				op: #op,
				dest: #dest,
				lhs: #lhs,
				rhs: #rhs,
				shift: bibe_instr::rrr::Shift {
					kind: bibe_instr::rrr::ShiftKind::Shl,
					shift: 0,
				}
			})
		})
	}
}

struct RriCondition(isa::rri::Condition);
impl<'a> ToTokens for RriCondition {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.extend(match self.0 {
				isa::rri::Condition::Al => quote! { bibe_instr::rri::Condition::Al },
				isa::rri::Condition::Eq => quote! { bibe_instr::rri::Condition::Eq },
				isa::rri::Condition::Ne => quote! { bibe_instr::rri::Condition::Ne },
				isa::rri::Condition::Gt => quote! { bibe_instr::rri::Condition::Gt },
				isa::rri::Condition::Ge => quote! { bibe_instr::rri::Condition::Ge },
				isa::rri::Condition::Lt => quote! { bibe_instr::rri::Condition::Lt },
				isa::rri::Condition::Le => quote! { bibe_instr::rri::Condition::Le },
				isa::rri::Condition::Nv => quote! { bibe_instr::rri::Condition::Nv },
			});
	}
}

struct RriInstruction<'a>(&'a isa::rri::Instruction);
impl<'a> ToTokens for RriInstruction<'a> {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let op = BinOp(self.0.op);
		let cond = RriCondition(self.0.cond);
		let dest = Register(self.0.dest);
		let src = Register(self.0.src);
		let imm = self.0.imm;

		tokens.extend(quote! {
			bibe_instr::Instruction::Rri(bibe_instr::rri::Instruction {
				op: #op,
				cond: #cond,
				dest: #dest,
				src: #src,
				imm: #imm,
			})
		})
	}
}

struct Instruction<'a>(&'a isa::Instruction);
impl<'a> ToTokens for Instruction<'a> {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match &self.0 {
			isa::Instruction::Rrr(i) => RrrInstruction(i).to_tokens(tokens),
			isa::Instruction::Rri(i) => RriInstruction(i).to_tokens(tokens),
			_ => panic!("Unsupported instruction type"),
		}
	}
}

struct ProcEmitter {
	stream: TokenStream,
}

impl ProcEmitter {
	pub fn new() -> ProcEmitter {
		ProcEmitter {
			stream: TokenStream::new()
		}
	}

	pub fn finish(self) -> RTokenStream {
		self.stream.into()
	}
}

impl Emitter for ProcEmitter {
    fn emit_isa_instruction(&mut self, object: &Object, addr: u64, instr: &isa::Instruction) -> Result<()> {
        Instruction(instr).to_tokens(&mut self.stream);
		Ok(())
    }

    fn emit_asm_directive(&mut self, object: &Object, addr: u64, directive: &asm::Directive) -> Result<()> {
        unimplemented!()
    }
}

#[proc_macro]
pub fn asm(tokens: RTokenStream) -> RTokenStream {
	let s = stringify(&tokens);
	let (_, ts) = tokenize(&s).unwrap();
	let (_, instr_asm) = instruction(&ts).unwrap();
	let mut object = Object::new();
	let mut emitter = ProcEmitter::new();

	object.insert_statement(&asm::Statement::Instruction(instr_asm));
	emitter.emit(&object);
	emitter.finish()
}