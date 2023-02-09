use proc_macro::TokenStream as RTokenStream;
use proc_macro2::TokenStream;
use quote::{ quote, ToTokens };
use bibe_instr as bibe;
use bibe_asm::parser::{ instruction, tokenize };

fn stringify(tokens: &RTokenStream) -> String {
	tokens.to_string()
}

struct BinOp(bibe::BinOp);
impl ToTokens for BinOp{
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.extend(match self.0 {
			bibe::BinOp::Add => quote! { bibe_instr::BinOp::Add },
			bibe::BinOp::Sub => quote! { bibe_instr::BinOp::Sub },

			bibe::BinOp::Mul => quote! { bibe_instr::BinOp::Mul },
			bibe::BinOp::Div => quote! { bibe_instr::BinOp::Div },
			bibe::BinOp::Mod => quote! { bibe_instr::BinOp::Mod },

			bibe::BinOp::And => quote! { bibe_instr::BinOp::And },
			bibe::BinOp::Or => quote! { bibe_instr::BinOp::Or },
			bibe::BinOp::Xor => quote! { bibe_instr::BinOp::Xor },

			bibe::BinOp::Shl => quote! { bibe_instr::BinOp::Shl },
			bibe::BinOp::Shr => quote! { bibe_instr::BinOp::Shr },
			bibe::BinOp::Asl => quote! { bibe_instr::BinOp::Asl },
			bibe::BinOp::Asr => quote! { bibe_instr::BinOp::Asr },
			bibe::BinOp::Rol => quote! { bibe_instr::BinOp::Rol },
			bibe::BinOp::Ror => quote! { bibe_instr::BinOp::Ror },

			bibe::BinOp::Not => quote! { bibe_instr::BinOp::Not },
			bibe::BinOp::Neg => quote! { bibe_instr::BinOp::Neg },
			bibe::BinOp::Cmp => quote! { bibe_instr::BinOp::Cmp },
		});
	}
}

struct Register(bibe::Register);
impl ToTokens for Register {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let r = self.0.as_u8();
		tokens.extend(quote! { bibe_instr::Register::new(#r).unwrap() })
	}
}

struct RrrInstruction<'a>(&'a bibe::rrr::Instruction);
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

struct RriCondition(bibe::rri::Condition);
impl<'a> ToTokens for RriCondition {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.extend(match self.0 {
				bibe::rri::Condition::Al => quote! { bibe_instr::rri::Condition::Al },
				bibe::rri::Condition::Eq => quote! { bibe_instr::rri::Condition::Eq },
				bibe::rri::Condition::Ne => quote! { bibe_instr::rri::Condition::Ne },
				bibe::rri::Condition::Gt => quote! { bibe_instr::rri::Condition::Gt },
				bibe::rri::Condition::Ge => quote! { bibe_instr::rri::Condition::Ge },
				bibe::rri::Condition::Lt => quote! { bibe_instr::rri::Condition::Lt },
				bibe::rri::Condition::Le => quote! { bibe_instr::rri::Condition::Le },
				bibe::rri::Condition::Nv => quote! { bibe_instr::rri::Condition::Nv },
			});
	}
}

struct RriInstruction<'a>(&'a bibe::rri::Instruction);
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

struct Instruction<'a>(&'a bibe::Instruction);
impl<'a> ToTokens for Instruction<'a> {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match &self.0 {
			bibe::Instruction::Rrr(i) => RrrInstruction(i).to_tokens(tokens),
			bibe::Instruction::Rri(i) => RriInstruction(i).to_tokens(tokens),
			_ => panic!("Unsupported instruction type"),
		}
	}
}

#[proc_macro]
pub fn asm(tokens: RTokenStream) -> RTokenStream {
	let s = stringify(&tokens);
	let (_, ts) = tokenize(&s).unwrap();
	let (_, instruction) = instruction(&ts).unwrap();
	let mut stream = TokenStream::new();
	Instruction(&instruction).to_tokens(&mut stream);
	stream.into()
}