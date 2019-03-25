pub struct Color16 {
    pub r: u16,
    pub g: u16,
    pub b: u16,
}

pub trait SmartLedsWrite16 {
    type Error;
    fn write<T>(&mut self, iterator: T) -> Result<(), Self::Error>
    where
        T: Iterator<Item = Color16>;
}
