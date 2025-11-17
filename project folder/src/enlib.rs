use heapless::{String, Vec};
type KeyBuf    = String<16>;
type TextBuf   = String<64>;
type ShiftBuf  = Vec<u8, 16>;

/// Four-Square cipher implementation (5×5, I/J merged).
pub struct FourSquare {
    /// Top-Left is plain “ABCDE…Z” (with I/J merged), not used in encryption.
    top_left: [[char; 5]; 5],
    /// Top-Right keyed with k1
    top_right: [[char; 5]; 5],
    /// Bottom-Left keyed with k2
    bottom_left: [[char; 5]; 5],
    /// Bottom-Right plain square
    bottom_right: [[char; 5]; 5],
}

impl FourSquare {
    /// Build a 5×5 square from a keyword: drop duplicates,
    /// then fill with the rest of the A-Z (I/J merged).
    fn make_square(keyword: &str) -> [[char;5];5] {
        let mut seen = [false; 26];
        let mut v: Vec<char, 25> = Vec::new();

        for c in keyword.chars() {
            let uc = c.to_ascii_uppercase();
            let idx = if uc=='J' { b'I'-b'A' } else { uc as u8 - b'A' } as usize;
            if idx < 26 && !seen[idx] {
                seen[idx] = true;
                v.push(if uc=='J' { 'I' } else { uc });
            }
        }

        for letter in b'A'..=b'Z' {
            let c = letter as char;
            let idx = if c=='J' { b'I'-b'A' } else { letter - b'A' } as usize;
            if idx < 26 && !seen[idx] {
                seen[idx] = true;
                v.push(if c=='J' { 'I' } else { c });
            }
        }

        // fill into 5×5 row-major
        let mut square = [['A';5];5];
        for (i, ch) in v.into_iter().enumerate() {
            square[i/5][i%5] = ch;
        }
        square
    }

    /// Construct a FourSquare cipher with keys k1,k2
    pub fn new(k1: &str, k2: &str) -> Self {
        let plain = Self::make_square("");
        Self {
            top_left: plain,
            top_right: Self::make_square(k1),
            bottom_left: Self::make_square(k2),
            bottom_right: plain,
        }
    }

    /// Find (row,col) of ch in given square
    fn find_pos(square: &[[char;5];5], ch: char) -> Option<(usize,usize)> {
        let target = if ch=='J' {'I'} else { ch };
        for r in 0..5 {
            for c in 0..5 {
                if square[r][c] == target {
                    return Some((r,c));
                }
            }
        }
        None
    }

    /// Encrypt plaintext (automatically pads odd length with 'X')
    pub fn encrypt(&self, plaintext: &str) -> String<256> {
        // Prepare only letters, uppercase, I/J → I, pad to even length
        let mut clean: Vec<char, 256> = plaintext
            .chars()
            .filter(|c| c.is_ascii_alphabetic())
            .map(|c| {
                let uc = c.to_ascii_uppercase();
                if uc=='J' { 'I' } else { uc }
            })
            .collect();

        if clean.len() % 2 != 0 {
            clean.push('X');
        }

        let mut out: String<256> = String::new();

        for pair in clean.chunks(2) {
            let a = pair[0];
            let b = pair[1];
            // a from top-left → find row
            let (r1, _) = Self::find_pos(&self.top_left, a)
                .unwrap_or((0,0));
            // b from bottom-right → find column
            let (_, c2) = Self::find_pos(&self.bottom_right, b)
                .unwrap_or((0,0));

            // cipher chars:
            // C1 = top_right[r1][c2]
            let c1 = self.top_right[r1][c2];

            // And for second letter:
            let (_, c3) = Self::find_pos(&self.bottom_left, self.bottom_left[0][0])
                .map(|(_,c)| (0,c)).unwrap_or((0,0)); // dummy

            // But correct:
            let (_, c2b) = Self::find_pos(&self.bottom_left, b)
                .unwrap_or((0,0));
            let c2 = self.bottom_left[r1][c2b];

            let _ = out.push(c1);
            let _ = out.push(c2);
        }

        out
    }
}

pub fn four_square(text: &str, key1: &KeyBuf, key2: &KeyBuf) -> TextBuf {
    // build a 5×5 square from a key
    fn build(key: &KeyBuf) -> [char;25] {
        let mut seen = [false;26];
        let mut sq = ['A';25];
        let mut idx = 0;
        for &b in key.as_bytes() {
            let mut c = (b as char).to_ascii_uppercase();
            if c == 'J' { c = 'I'; }
            let i = (c as u8 - b'A') as usize;
            if i<26 && !seen[i] {
                seen[i] = true;
                sq[idx] = c;
                idx+=1;
            }
        }
        for b in b'A'..=b'Z' {
            let mut c = b as char;
            if c == 'J' { c = 'I'; }
            let i = (c as u8 - b'A') as usize;
            if !seen[i] && idx<25 {
                seen[i] = true;
                sq[idx] = c;
                idx+=1;
            }
        }
        sq
    }
    let std  = build(&KeyBuf::new()); // plain A–Z
    let sq1  = build(key1);
    let sq2  = build(key2);

    // find (row,col)
    fn pos(sq: &[char;25], c: char) -> (usize,usize) {
        let mut cc = c;
        if cc=='J' { cc='I'; }
        let idx = sq.iter().position(|&x|x==cc).unwrap_or(0);
        (idx/5, idx%5)
    }

    // prepare digraphs
    let mut pts: TextBuf = TextBuf::new();
    for c in text.chars() {
        if c.is_ascii_alphabetic() {
            let mut cc = c.to_ascii_uppercase();
            if cc=='J' { cc='I'; }
            pts.push(cc).ok();
        }
    }
    if pts.len() % 2 == 1 { pts.push('X').ok(); }

    // encrypt
    let mut out = TextBuf::new();
    let chars: Vec<char, 64> = pts.chars().collect();
    for pair in chars.chunks(2) {
        let (r1,c1) = pos(&std,  pair[0]);
        let (r2,c2) = pos(&std,  pair[1]);
        out.push(sq1[r1*5 + c2]).ok();
        out.push(sq2[r2*5 + c1]).ok();
    }
    out
}


pub fn caesar(text: &str, shift: u8) -> TextBuf {
    let mut out = TextBuf::new();
    for c in text.chars() {
        if c.is_ascii_alphabetic() {
            let base = if c.is_ascii_uppercase() { b'A' } else { b'a' };
            let idx  = (c as u8 - base + shift) % 26;
            out.push((base + idx) as char).ok();
        } else {
            out.push(c).ok();
        }
    }
    out
}

// ——— Vigenère ———
pub fn vigenere(text: &str, key: &KeyBuf) -> TextBuf {
    // build shift table
    let mut key_bytes: ShiftBuf = ShiftBuf::new();
    for &b in key.as_bytes() {
        if b'A' <= b && b <= b'Z' {
            key_bytes.push(b - b'A').ok();
        }
    }
    // if no valid key, just echo
    if key_bytes.is_empty() {
        let mut out = TextBuf::new();
        out.push_str(text).ok();
        return out;
    }
    // apply
    let mut out = TextBuf::new();
    let mut ki = 0;
    for c in text.chars() {
        if c.is_ascii_alphabetic() {
            let base = if c.is_ascii_uppercase() { b'A' } else { b'a' };
            let p    = c as u8 - base;
            let shift = key_bytes[ki % key_bytes.len()];
            ki += 1;
            out.push((base + (p + shift) % 26) as char).ok();
        } else {
            out.push(c).ok();
        }
    }
    out
}

pub const M94_DISCS: [&str; 26] = [
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    "BCDEFGHIJKLMNOPQRSTUVWXYZA",
    "CDEFGHIJKLMNOPQRSTUVWXYZAB",
    "DEFGHIJKLMNOPQRSTUVWXYZABC",
    "EFGHIJKLMNOPQRSTUVWXYZABCD",
    "FGHIJKLMNOPQRSTUVWXYZABCDE",
    "GHIJKLMNOPQRSTUVWXYZABCDEF",
    "HIJKLMNOPQRSTUVWXYZABCDEFG",
    "IJKLMNOPQRSTUVWXYZABCDEFGH",
    "JKLMNOPQRSTUVWXYZABCDEFGHI",
    "KLMNOPQRSTUVWXYZABCDEFGHIJ",
    "LMNOPQRSTUVWXYZABCDEFGHIJK",
    "MNOPQRSTUVWXYZABCDEFGHIJKL",
    "NOPQRSTUVWXYZABCDEFGHIJKLM",
    "OPQRSTUVWXYZABCDEFGHIJKLMN",
    "PQRSTUVWXYZABCDEFGHIJKLMNO",
    "QRSTUVWXYZABCDEFGHIJKLMNOP",
    "RSTUVWXYZABCDEFGHIJKLMNOPQ",
    "STUVWXYZABCDEFGHIJKLMNOPQR",
    "TUVWXYZABCDEFGHIJKLMNOPQRS",
    "UVWXYZABCDEFGHIJKLMNOPQRST",
    "VWXYZABCDEFGHIJKLMNOPQRSTU",
    "WXYZABCDEFGHIJKLMNOPQRSTUV",
    "XYZABCDEFGHIJKLMNOPQRSTUVQ",
    "YZABCDEFGHIJKLMNOPQRSTUVWX",
    "ZABCDEFGHIJKLMNOPQRSTUVWXY",
];

pub fn m94(text: &str, config: &KeyBuf) -> TextBuf {
    // Parse the “row index” from the config string, default to 1
    let row: usize = config
        .as_str()
        .parse::<usize>()
        .unwrap_or(1)
        .min(25); // clamp to [0..25]

    let mut out = TextBuf::new();
    for (i, ch) in text.chars().enumerate() {
        if !ch.is_ascii_alphabetic() {
            // pass through non-letters
            out.push(ch).ok();
            continue;
        }
        let disc = M94_DISCS[i % M94_DISCS.len()];
        let p = ch.to_ascii_uppercase();
        // find plaintext letter on the disc
        if let Some(pos) = disc.find(p) {
            // select the letter at (pos + row) mod 26
            let idx = (pos + row) % 26;
            if let Some(cc) = disc.chars().nth(idx) {
                out.push(cc).ok();
                continue;
            }
        }
        // fallback if something went wrong
        out.push(ch).ok();
    }
    out
}
