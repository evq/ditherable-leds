#![no_std]
#![feature(const_fn)]

use core::{cmp, u16};
use generic_array::{sequence::GenericSequence, ArrayLength, GenericArray};
use smart_leds_trait::{Color, SmartLedsWrite};

pub use generic_array::typenum;

use proc_macro_hack::proc_macro_hack;

#[proc_macro_hack]
pub use gamma_lut_macro::gamma_lut;

pub const GAMMA_2_2_LUT: [u16; 257] =
    gamma_lut!(gamma: 2.2, linear_cutoff: 1.0/255.0, linear_slope: 1.0);

#[derive(Copy, Clone, Default)]
pub struct Residual {
    pub r: i16,
    pub g: i16,
    pub b: i16,
}

#[derive(Copy, Clone, Default)]
pub struct ChannelState {
    pub prev: u8,
    pub next: u8,
    pub residual: i16,
}

pub struct DitherState<N>
where
    N: ArrayLength<[ChannelState; 3]>,
{
    pub gamma_lut: Option<&'static [u16; 257]>,
    pub prev_ts: u32,
    pub next_ts: u32,
    pub interp_coefficient: u32,
    pub i: usize,
    pub pixel_states: GenericArray<[ChannelState; 3], N>,
}

pub struct DitherableLeds<LEDS, N>
where
    N: ArrayLength<[ChannelState; 3]>,
{
    pub leds: LEDS,
    pub state: DitherState<N>,
}

pub struct DitherableLedsInstant<'a, LEDS, N>
where
    N: ArrayLength<[ChannelState; 3]>,
{
    pub now: u32,
    pub leds: &'a mut DitherableLeds<LEDS, N>,
}

impl<N> Iterator for DitherState<N>
where
    N: ArrayLength<[ChannelState; 3]>,
{
    type Item = Color;

    fn next(&mut self) -> Option<Color> {
        if self.i >= self.pixel_states.len() {
            self.i = 0;
            return None;
        }

        let interp_coefficient = self.interp_coefficient;
        let gamma_lut = self.gamma_lut;

        let color: GenericArray<u8, typenum::U3> = self.pixel_states[self.i]
            .iter_mut()
            .map(|channel_state| {
                let interp_prev =
                    (channel_state.prev as u32 * (0x10000 - interp_coefficient)) * 257;
                let interp_next = (channel_state.next as u32 * (interp_coefficient)) * 257;
                let mut target = (interp_prev + interp_next) >> 16;

                if let Some(lut) = gamma_lut {
                    let index = (target >> 8) as usize;
                    let alpha = target & 0xFF;
                    let inv_alpha = 0x100 - alpha;

                    target = (lut[index] as u32 * inv_alpha + lut[index + 1] as u32 * alpha) >> 8;
                }

                let target = target as i32 + channel_state.residual as i32;

                let mut val = target + 0x80;
                if val > u16::MAX as i32 {
                    val = u16::MAX as i32;
                }

                let val = val >> 8;

                channel_state.residual = (target - (val * 257)) as i16;

                val as u8
            })
            .collect();

        self.i = self.i + 1;

        Some(Color {
            r: color[0],
            g: color[1],
            b: color[2],
        })
    }
}

impl<T, N> DitherableLeds<T, N>
where
    N: ArrayLength<[ChannelState; 3]>,
    T: SmartLedsWrite,
{
    pub fn new(leds: T, gamma_lut: Option<&'static [u16; 257]>) -> DitherableLeds<T, N>
    where
        T: SmartLedsWrite,
    {
        DitherableLeds {
            leds,
            state: DitherState {
                pixel_states: GenericArray::generate(|_i: usize| {
                    [ChannelState {
                        prev: 0,
                        next: 0,
                        residual: 0,
                    }; 3]
                }),
                next_ts: 0,
                prev_ts: 0,
                interp_coefficient: 0,
                i: 0,
                gamma_lut: gamma_lut,
            },
        }
    }

    pub fn reset(&mut self) {
        self.state.interp_coefficient = 0;
        self.state.i = 0;
    }

    pub fn now(&mut self, now: u32) -> DitherableLedsInstant<T, N> {
        DitherableLedsInstant { leds: self, now }
    }
}

impl<'a, T, N> DitherableLedsInstant<'a, T, N>
where
    N: ArrayLength<[ChannelState; 3]>,
    T: SmartLedsWrite,
{
    pub fn flush(&mut self) {
        let diff = self
            .leds
            .state
            .next_ts
            .wrapping_sub(self.leds.state.prev_ts);
        if diff > 0 {
            let elapsed = self.now.wrapping_sub(self.leds.state.next_ts);
            self.leds.state.interp_coefficient = (cmp::min(elapsed, diff) << 16) / diff;
        }

        self.leds.leds.write(&mut self.leds.state).ok();
    }
}

impl<'a, T, N> SmartLedsWrite for DitherableLedsInstant<'a, T, N>
where
    N: ArrayLength<[ChannelState; 3]>,
    T: SmartLedsWrite,
{
    type Error = ();

    fn write<I>(&mut self, iterator: I) -> Result<(), ()>
    where
        I: Iterator<Item = Color>,
    {
        self.leds.reset();

        self.leds.state.prev_ts = self.leds.state.next_ts;
        self.leds.state.next_ts = self.now;

        for (pixel_state, new_pixel) in self.leds.state.pixel_states.iter_mut().zip(iterator) {
            for (channel_state, new_val) in pixel_state
                .iter_mut()
                .zip([new_pixel.r, new_pixel.g, new_pixel.b].iter())
            {
                channel_state.prev = channel_state.next;
                channel_state.next = *new_val;
            }
        }

        Ok(())
    }
}
