#![recursion_limit = "256"]
#![feature(box_patterns)]

extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;
use quote::quote;
use std::u16;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Token};

// TODO add whitepoint correction?
struct GammaCorrectionParams {
    gamma: Option<f32>,
    linear_cutoff: Option<f32>,
    linear_slope: Option<f32>,
}

impl Parse for GammaCorrectionParams {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let fvs: Punctuated<syn::FieldValue, Token![,]> =
            input.parse_terminated(syn::FieldValue::parse)?;
        let mut ret = GammaCorrectionParams {
            gamma: None,
            linear_slope: None,
            linear_cutoff: None,
        };
        for fv in fvs {
            let value = match fv.expr {
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Float(value),
                    ..
                }) => value.value() as f32,
                syn::Expr::Binary(syn::ExprBinary {
                    left:
                        box syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Float(left),
                            ..
                        }),
                    op: syn::BinOp::Div(_),
                    right:
                        box syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Float(right),
                            ..
                        }),
                    ..
                }) => (left.value() / right.value()) as f32,
                _ => panic!("only float literal values are supported"),
            };
            match fv.member {
                syn::Member::Named(ident) => match ident.to_string().as_str() {
                    "gamma" => ret.gamma = Some(value),
                    "linear_slope" => ret.linear_slope = Some(value),
                    "linear_cutoff" => ret.linear_cutoff = Some(value),
                    _ => {}
                },
                _ => panic!("only gamma, linear_slope and linear_cutoff can be specified"),
            }
        }
        Ok(ret)
    }
}

impl GammaCorrectionParams {
    fn calculate_correction(&self, target: f32) -> f32 {
        let linear_cutoff = self.linear_cutoff.unwrap_or(0.0);
        let linear_slope = self.linear_slope.unwrap_or(1.0);
        let gamma = self.gamma.unwrap_or(1.0);

        if target * linear_slope <= linear_cutoff {
            return target * linear_slope;
        } else {
            let non_linear_input = target - (linear_slope * linear_cutoff);
            let scale = 1.0 - linear_cutoff;
            return linear_cutoff + (non_linear_input / scale).powf(gamma) * scale;
        }
    }
}

#[proc_macro_hack]
pub fn gamma_lut(input: TokenStream) -> TokenStream {
    let input_clone = input.clone();
    let params = parse_macro_input!(input_clone as GammaCorrectionParams);

    let mut lut = Vec::new();
    for i in 0..257 {
        let corrected = params.calculate_correction((i << 8) as f32 / u16::MAX as f32);
        lut.push(
            (corrected * u16::MAX as f32)
                .min(u16::MAX as f32)
                .max(u16::MIN as f32) as u16,
        );
    }
    let expanded = quote! {
        [#(#lut),*];
    };
    TokenStream::from(expanded)
}
