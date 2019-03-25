#![no_std]
#![feature(const_fn)]

use core::{cmp, u16};
use generic_array::{sequence::GenericSequence, ArrayLength, GenericArray};
use proc_macro_hack::proc_macro_hack;
use smart_leds_trait::{Color, SmartLedsWrite};

pub use generic_array::typenum;

#[proc_macro_hack]
pub use gamma_lut_macro::gamma_lut;

pub mod color;
use color::{Color16, SmartLedsWrite16};

type GammaLUT = [u16; 257];
pub const GAMMA_2_2_LUT: GammaLUT =
    gamma_lut!(gamma: 2.2, linear_cutoff: 1.0/255.0, linear_slope: 1.0);

#[derive(Copy, Clone, Default)]
pub struct ColorChannelState {
    pub prev: u8,
    pub next: u8,
    pub residual: i16,
}

impl ColorChannelState {
    fn interpolate(&self, interp_coefficient: u32) -> u32 {
        let interp_prev = (self.prev as u32 * (0x10000 - interp_coefficient)) * 257;
        let interp_next = (self.next as u32 * (interp_coefficient)) * 257;
        ((interp_prev + interp_next) >> 16)
    }

    fn corrected_interpolate(
        &self,
        interp_coefficient: u32,
        gamma_lut: Option<&'static GammaLUT>,
    ) -> u16 {
        let target = self.interpolate(interp_coefficient);
        if let Some(lut) = gamma_lut {
            let index = (target >> 8) as usize;
            let alpha = target & 0xFF;
            let inv_alpha = 0x100 - alpha;

            return ((lut[index] as u32 * inv_alpha + lut[index + 1] as u32 * alpha) >> 8) as u16;
        }
        target as u16
    }

    fn corrected_interpolate_and_dither(
        &mut self,
        interp_coefficient: u32,
        gamma_lut: Option<&'static GammaLUT>,
    ) -> u8 {
        let target = self.corrected_interpolate(interp_coefficient, gamma_lut);

        let target = target as i32 + self.residual as i32;

        let mut val = target + 0x80;
        if val > u16::MAX as i32 {
            val = u16::MAX as i32;
        }

        let val = val >> 8;

        self.residual = (target - (val * 257)) as i16;

        val as u8
    }
}

pub struct DitherState<N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    pub gamma_lut: Option<&'static GammaLUT>,
    pub prev_ts: u32,
    pub next_ts: u32,
    pub interp_coefficient: u32,
    pub i: usize,
    pub pixel_states: GenericArray<[ColorChannelState; 3], N>,
}

impl<N> DitherState<N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    pub fn iter_color(&mut self) -> DitherStateIterColor<N> {
        DitherStateIterColor { state: self }
    }

    pub fn iter_color16(&mut self) -> DitherStateIterColor16<N> {
        DitherStateIterColor16 { state: self }
    }
}

pub struct DitherStateIterColor<'a, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    pub state: &'a mut DitherState<N>,
}

impl<'a, N> Iterator for DitherStateIterColor<'a, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    type Item = Color;

    fn next(&mut self) -> Option<Color> {
        if self.state.i >= self.state.pixel_states.len() {
            self.state.i = 0;
            return None;
        }

        let interp_coefficient = self.state.interp_coefficient;
        let gamma_lut = self.state.gamma_lut;

        let color: GenericArray<u8, typenum::U3> = self.state.pixel_states[self.state.i]
            .iter_mut()
            .map(|channel_state| {
                channel_state.corrected_interpolate_and_dither(interp_coefficient, gamma_lut)
            })
            .collect();

        self.state.i = self.state.i + 1;

        Some(Color {
            r: color[0],
            g: color[1],
            b: color[2],
        })
    }
}

pub struct DitherStateIterColor16<'a, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    pub state: &'a mut DitherState<N>,
}

impl<'a, N> Iterator for DitherStateIterColor16<'a, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    type Item = Color16;

    fn next(&mut self) -> Option<Color16> {
        if self.state.i >= self.state.pixel_states.len() {
            self.state.i = 0;
            return None;
        }

        let interp_coefficient = self.state.interp_coefficient;
        let gamma_lut = self.state.gamma_lut;

        let color: GenericArray<u16, typenum::U3> = self.state.pixel_states[self.state.i]
            .iter_mut()
            .map(|channel_state| channel_state.corrected_interpolate(interp_coefficient, gamma_lut))
            .collect();

        self.state.i = self.state.i + 1;

        Some(Color16 {
            r: color[0],
            g: color[1],
            b: color[2],
        })
    }
}

pub struct DitherableLeds<LEDS, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    pub leds: LEDS,
    pub state: DitherState<N>,
}

impl<LEDS, N> DitherableLeds<LEDS, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    pub fn new(leds: LEDS, gamma_lut: Option<&'static GammaLUT>) -> DitherableLeds<LEDS, N> {
        DitherableLeds {
            leds,
            state: DitherState {
                pixel_states: GenericArray::generate(|_i: usize| {
                    [ColorChannelState {
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

    pub fn now(&mut self, now: u32) -> DitherableLedsInstant<LEDS, N> {
        DitherableLedsInstant { leds: self, now }
    }
}

pub struct DitherableLedsInstant<'a, LEDS, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    pub now: u32,
    pub leds: &'a mut DitherableLeds<LEDS, N>,
}

impl<'a, LEDS, N> DitherableLedsInstant<'a, LEDS, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
{
    pub fn flush16(&mut self)
    where
        LEDS: SmartLedsWrite16,
    {
        let diff = self
            .leds
            .state
            .next_ts
            .wrapping_sub(self.leds.state.prev_ts);
        if diff > 0 {
            let elapsed = self.now.wrapping_sub(self.leds.state.next_ts);
            self.leds.state.interp_coefficient = (cmp::min(elapsed, diff) << 16) / diff;
        }

        self.leds
            .leds
            .write(&mut self.leds.state.iter_color16())
            .ok();
    }

    pub fn flush(&mut self)
    where
        LEDS: SmartLedsWrite,
    {
        let diff = self
            .leds
            .state
            .next_ts
            .wrapping_sub(self.leds.state.prev_ts);
        if diff > 0 {
            let elapsed = self.now.wrapping_sub(self.leds.state.next_ts);
            self.leds.state.interp_coefficient = (cmp::min(elapsed, diff) << 16) / diff;
        }

        self.leds.leds.write(&mut self.leds.state.iter_color()).ok();
    }
}

impl<'a, LEDS, N> SmartLedsWrite for DitherableLedsInstant<'a, LEDS, N>
where
    N: ArrayLength<[ColorChannelState; 3]>,
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
