#![no_std]

use core::{cmp, u16};
use generic_array::{sequence::GenericSequence, ArrayLength, GenericArray};
use proc_macro_hack::proc_macro_hack;
use smart_leds_trait::{GenericPixel, Pixel, SmartLedsWrite};

pub use generic_array::typenum;

#[proc_macro_hack]
pub use gamma_lut_macro::gamma_lut;

type GammaLUT = [u16; 257];
pub const GAMMA_2_2_LUT: GammaLUT =
    gamma_lut!(gamma: 2.2, linear_cutoff: 1.0/255.0, linear_slope: 1.0);

pub struct ColorChannelTarget {
    pub interpolated_and_corrected: u16,
    pub dithered: u8,
}

impl From<ColorChannelTarget> for u16 {
    fn from(w: ColorChannelTarget) -> u16 {
        w.interpolated_and_corrected
    }
}

impl From<ColorChannelTarget> for u8 {
    fn from(w: ColorChannelTarget) -> u8 {
        w.dithered
    }
}

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
    ) -> ColorChannelTarget {
        let interpolated_and_corrected = self.corrected_interpolate(interp_coefficient, gamma_lut);

        let target = interpolated_and_corrected as i32 + self.residual as i32;

        let mut val = target + 0x80;
        if val > u16::MAX as i32 {
            val = u16::MAX as i32;
        }

        let val = val >> 8;

        self.residual = (target - (val * 257)) as i16;

        ColorChannelTarget {
            interpolated_and_corrected,
            dithered: val as u8,
        }
    }
}

pub struct DitherState<PIXEL, N>
where
    PIXEL: Pixel,
    <PIXEL as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <PIXEL as Pixel>::ComponentType: From<ColorChannelTarget>,
    PIXEL: core::iter::FromIterator<<PIXEL as Pixel>::ComponentType>,
    N: ArrayLength<GenericArray<ColorChannelState, <PIXEL as Pixel>::ColorCount>>,
{
    pub gamma_lut: Option<&'static GammaLUT>,
    pub prev_ts: u32,
    pub next_ts: u32,
    pub interp_coefficient: u32,
    pub pixel_states:
        GenericArray<GenericArray<ColorChannelState, <PIXEL as Pixel>::ColorCount>, N>,
}

impl<PIXEL, N> DitherState<PIXEL, N>
where
    PIXEL: Pixel,
    <PIXEL as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <PIXEL as Pixel>::ComponentType: From<ColorChannelTarget>,
    PIXEL: core::iter::FromIterator<<PIXEL as Pixel>::ComponentType>,
    N: ArrayLength<GenericArray<ColorChannelState, <PIXEL as Pixel>::ColorCount>>,
{
    pub fn iter(&mut self) -> DitherStateIter<PIXEL, N> {
        DitherStateIter { i: 0, state: self }
    }
}

pub struct DitherStateIter<'a, PIXEL, N>
where
    PIXEL: Pixel,
    <PIXEL as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <PIXEL as Pixel>::ComponentType: From<ColorChannelTarget>,
    PIXEL: core::iter::FromIterator<<PIXEL as Pixel>::ComponentType>,
    N: ArrayLength<GenericArray<ColorChannelState, <PIXEL as Pixel>::ColorCount>>,
{
    pub i: usize,
    pub state: &'a mut DitherState<PIXEL, N>,
}

impl<'a, PIXEL, N> Iterator for DitherStateIter<'a, PIXEL, N>
where
    PIXEL: Pixel,
    <PIXEL as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <PIXEL as Pixel>::ComponentType: From<ColorChannelTarget>,
    PIXEL: core::iter::FromIterator<<PIXEL as Pixel>::ComponentType>,
    N: ArrayLength<GenericArray<ColorChannelState, <PIXEL as Pixel>::ColorCount>>,
{
    type Item = PIXEL;

    fn next(&mut self) -> Option<PIXEL> {
        if self.i >= self.state.pixel_states.len() {
            self.i = 0;
            return None;
        }

        let interp_coefficient = self.state.interp_coefficient;
        let gamma_lut = self.state.gamma_lut;

        let color = self.state.pixel_states[self.i]
            .iter_mut()
            .map(|channel_state| {
                channel_state
                    .corrected_interpolate_and_dither(interp_coefficient, gamma_lut)
                    .into()
            })
            .collect();

        self.i = self.i + 1;

        Some(color)
    }
}

pub struct DitherableLeds<LEDS, N>
where
    N: ArrayLength<GenericArray<ColorChannelState, <LEDS::Pixel as Pixel>::ColorCount>>,
    LEDS: SmartLedsWrite,
    <LEDS::Pixel as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <LEDS::Pixel as Pixel>::ComponentType: From<ColorChannelTarget>,
    LEDS::Pixel: core::iter::FromIterator<<LEDS::Pixel as Pixel>::ComponentType>,
    LEDS::Pixel: Pixel,
{
    pub leds: LEDS,
    pub state: DitherState<LEDS::Pixel, N>,
}

impl<LEDS, N> DitherableLeds<LEDS, N>
where
    N: ArrayLength<GenericArray<ColorChannelState, <LEDS::Pixel as Pixel>::ColorCount>>,
    LEDS: SmartLedsWrite,
    <LEDS::Pixel as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <LEDS::Pixel as Pixel>::ComponentType: From<ColorChannelTarget>,
    LEDS::Pixel: core::iter::FromIterator<<LEDS::Pixel as Pixel>::ComponentType>,
    LEDS::Pixel: Pixel,
{
    pub fn new(leds: LEDS, gamma_lut: Option<&'static GammaLUT>) -> DitherableLeds<LEDS, N> {
        DitherableLeds {
            leds,
            state: DitherState {
                pixel_states: GenericArray::generate(|_i: usize| {
                    GenericArray::generate(|_i: usize| ColorChannelState {
                        prev: 0,
                        next: 0,
                        residual: 0,
                    })
                }),
                next_ts: 0,
                prev_ts: 0,
                interp_coefficient: 0,
                gamma_lut: gamma_lut,
            },
        }
    }

    pub fn now(&mut self, now: u32) -> DitherableLedsInstant<LEDS, N> {
        DitherableLedsInstant { leds: self, now }
    }
}

pub struct DitherableLedsInstant<'a, LEDS, N>
where
    N: ArrayLength<GenericArray<ColorChannelState, <LEDS::Pixel as Pixel>::ColorCount>>,
    LEDS: SmartLedsWrite,
    <LEDS::Pixel as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <LEDS::Pixel as Pixel>::ComponentType: From<ColorChannelTarget>,
    LEDS::Pixel: core::iter::FromIterator<<LEDS::Pixel as Pixel>::ComponentType>,
    LEDS::Pixel: Pixel,
{
    pub now: u32,
    pub leds: &'a mut DitherableLeds<LEDS, N>,
}

impl<'a, LEDS, N> DitherableLedsInstant<'a, LEDS, N>
where
    N: ArrayLength<GenericArray<ColorChannelState, <LEDS::Pixel as Pixel>::ColorCount>>,
    LEDS: SmartLedsWrite,
    <LEDS::Pixel as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <LEDS::Pixel as Pixel>::ComponentType: From<ColorChannelTarget>,
    LEDS::Pixel: core::iter::FromIterator<<LEDS::Pixel as Pixel>::ComponentType>,
    LEDS::Pixel: Pixel,
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

        self.leds.leds.write(&mut self.leds.state.iter()).ok();
    }
}

impl<'a, LEDS, N> SmartLedsWrite for DitherableLedsInstant<'a, LEDS, N>
where
    N: ArrayLength<GenericArray<ColorChannelState, <LEDS::Pixel as Pixel>::ColorCount>>,
    LEDS: SmartLedsWrite,
    <LEDS::Pixel as Pixel>::ColorCount: generic_array::ArrayLength<ColorChannelState>,
    <LEDS::Pixel as Pixel>::ColorCount: generic_array::ArrayLength<u8>,
    <LEDS::Pixel as Pixel>::ComponentType: From<ColorChannelTarget>,
    LEDS::Pixel: core::iter::FromIterator<<LEDS::Pixel as Pixel>::ComponentType>,
    LEDS::Pixel: Pixel,
{
    type Pixel = GenericPixel<<LEDS::Pixel as Pixel>::ColorCount, u8>;
    type Error = ();

    fn write<T, I>(&mut self, iterator: T) -> Result<(), ()>
    where
        T: Iterator<Item = I>,
        I: Into<Self::Pixel>,
    {
        self.leds.state.prev_ts = self.leds.state.next_ts;
        self.leds.state.next_ts = self.now;

        for (pixel_state, new_pixel) in self.leds.state.pixel_states.iter_mut().zip(iterator) {
            let new_pixel = new_pixel.into();
            for (channel_state, new_val) in pixel_state.iter_mut().zip(new_pixel.iter()) {
                channel_state.prev = channel_state.next;
                channel_state.next = *new_val;
            }
        }

        Ok(())
    }
}
