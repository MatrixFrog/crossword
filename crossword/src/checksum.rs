//! Checksum logic. See
//! <https://gist.github.com/sliminality/dab21fa834eae0a70193c7cd69c356d5#checksums>

#[must_use]
pub(crate) fn checksum_region(base: &[u8], input_checksum: u16) -> u16 {
    let mut checksum = input_checksum;
    for &byte in base {
        if checksum & 0x0001_u16 != 0 {
            checksum = (checksum >> 1) + 0x8000
        } else {
            checksum = checksum >> 1;
        }
        checksum = checksum.overflowing_add(byte as u16).0;
    }
    checksum
}

/// For metadata (title, author, copyright, or notes), we do nothing if the string is
/// empty, but if it's not empty we include the \0 byte in the calculation.
#[must_use]
pub(crate) fn checksum_metadata_string(s: &[u8], input_checksum: u16) -> u16 {
    if s == b"\0" {
        return input_checksum;
    }

    checksum_region(s, input_checksum)
}

/// For clues, we do not include the trailing \0 byte.
#[must_use]
pub(crate) fn checksum_clue(s: &[u8], input_checksum: u16) -> u16 {
    checksum_region(&s[0..s.len() - 1], input_checksum)
}
