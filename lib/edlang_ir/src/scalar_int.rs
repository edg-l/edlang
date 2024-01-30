use std::num::NonZeroU8;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
#[repr(packed(1))]
pub struct ScalarInt {
    pub data: u128,
    /// Size in bytes
    pub size: NonZeroU8,
}

impl ScalarInt {
    /// Size in bytes
    #[inline]
    pub fn size(self) -> u64 {
        self.size.get().into()
    }

    #[inline]
    pub fn try_from_int(i: impl Into<i128>, size_in_bytes: u64) -> Option<Self> {
        let i = i.into();
        // `into` performed sign extension, we have to truncate
        let truncated = truncate(size_in_bytes, i as u128);
        if sign_extend(size_in_bytes, truncated) as i128 == i {
            Some(Self {
                data: truncated,
                size: NonZeroU8::new(size_in_bytes as u8).unwrap(),
            })
        } else {
            None
        }
    }
}

#[inline]
pub fn sign_extend(size: u64, value: u128) -> u128 {
    let size = size * 8;
    if size == 0 {
        // Truncated until nothing is left.
        return 0;
    }
    // Sign-extend it.
    let shift = 128 - size;
    // Shift the unsigned value to the left, then shift back to the right as signed
    // (essentially fills with sign bit on the left).
    (((value << shift) as i128) >> shift) as u128
}

/// Truncates `value` to `self` bits.
#[inline]
pub fn truncate(size: u64, value: u128) -> u128 {
    let size = size * 8;
    if size == 0 {
        // Truncated until nothing is left.
        return 0;
    }
    let shift = 128 - size;
    // Truncate (shift left to drop out leftover values, shift right to fill with zeroes).
    (value << shift) >> shift
}
