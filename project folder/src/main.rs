// +---------------------------------------------------------------------------+
// |                             Project's Main                                |
// +---------------------------------------------------------------------------+

//! src/main.rs

#![no_std]
#![no_main]

use heapless::{String, Vec};
type KeyBuf    = String<16>;
type TextBuf   = String<64>;
type ShiftBuf  = Vec<u8, 16>;



use defmt::*;
use defmt_rtt as _;
use panic_probe as _;

mod irqs;
use crate::irqs::Irqs;
use embassy_executor::Spawner;
use embassy_time::{Duration, Instant, Timer};
use static_cell::StaticCell;

use embassy_rp::init;
use embassy_rp::gpio::{Level, Output};
use embassy_rp::i2c::{I2c, Async, Config as I2cConfig,};
use embassy_rp::peripherals::I2C1;


use embedded_hal_async::i2c::I2c as _;

use embassy_net::{
    tcp::TcpSocket,
    Config as NetConfig,
    IpAddress,
    IpEndpoint,
    StaticConfigV4,
    StackResources,
};

use embassy_lab_utils::{init_wifi, init_network_stack};
use cyw43::JoinOptions;

/// 7-bit I²C address of the Si4713 (when SEN/CS is high)
const SI4713_ADDR: u8 = 0x63;

const SOCKS: usize = 4;
static RESOURCES: StaticCell<StackResources<SOCKS>> = StaticCell::new();

/// “dot” = 200 ms
const UNIT: Duration = Duration::from_millis(200);

/// ASCII → “.-” lookup table
const MORSE_TABLE: &[(char, &str)] = &[
    ('A', ".-"),    ('B', "-..."),  ('C', "-.-."),  ('D', "-.."),
    ('E', "."),     ('F', "..-."),  ('G', "--."),   ('H', "...."),
    ('I', ".."),    ('J', ".---"),  ('K', "-.-"),   ('L', ".-.."),
    ('M', "--"),    ('N', "-."),    ('O', "---"),   ('P', ".--."),
    ('Q', "--.-"),  ('R', ".-."),   ('S', "..."),   ('T', "-"),
    ('U', "..-"),   ('V', "...-"),  ('W', ".--"),   ('X', "-..-"),
    ('Y', "-.--"),  ('Z', "--.."),
    ('0', "-----"), ('1', ".----"), ('2', "..---"), ('3', "...--"),
    ('4', "....-"), ('5', "....."), ('6', "-...."), ('7', "--..."),
    ('8', "---.."), ('9', "----."),
    (' ', " "),     // word gap
];





/// Plays a square wave of `freq_hz` on `pin` for `duration`.
async fn play_tone<'d>(duration: Duration, freq_hz: f32, pin: &mut Output<'d>) {
    // half period in µs
    let half = Duration::from_micros((1_000_000.0 / (freq_hz * 2.0)) as u64);
    let deadline = Instant::now() + duration;
    while Instant::now() < deadline {
        pin.set_high();
        Timer::after(half).await;
        pin.set_low();
        Timer::after(half).await;
    }
}

#[derive(Clone)]
enum CipherMode {
    None,
    Caesar { shift: u8 },
    Vigenere { key: KeyBuf },
    FourSquare { key1: KeyBuf, key2: KeyBuf },
    M94 { config: KeyBuf },
}

impl CipherMode {
    fn apply(&self, plaintext: &str) -> TextBuf {
        match self {
            CipherMode::None => {
                let mut out = TextBuf::new();
                out.push_str(plaintext).ok();
                out
            }
            CipherMode::Caesar { shift } => caesar(plaintext, *shift),
            CipherMode::Vigenere { key }  => vigenere(plaintext, key),
            CipherMode::FourSquare { key1, key2 } => {
                four_square(plaintext, key1, key2)
            }
            CipherMode::M94 { config } => m94(plaintext, config)
        }
    }
}

// ——— Caesar ———
fn caesar(text: &str, shift: u8) -> TextBuf {
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
fn vigenere(text: &str, key: &KeyBuf) -> TextBuf {
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

// ——— Four‐Square ———
fn four_square(text: &str, key1: &KeyBuf, key2: &KeyBuf) -> TextBuf {
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

// ——— M-94 ———
const M94_DISCS: [&str; 26] = [
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

fn m94(text: &str, config: &KeyBuf) -> TextBuf {
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

#[embassy_executor::main]
async fn main(spawner: Spawner) {
    // — Wi-Fi setup
    let p = init(Default::default());
    let (net_device, mut control) = init_wifi!(&spawner, p).await;

    // Join WPA2
    const SSID: &str = "Charlie's hotpot";
    const PASS: &str = "strongrugbyarms";
    loop {
        if control.join(SSID, JoinOptions::new(PASS.as_bytes())).await.is_ok() {
            info!("✔ joined {}", SSID);
            break;
        }
        warn!("join failed, retrying in 2s…");
        Timer::after(Duration::from_secs(2)).await;
    }

    // DHCPv4 network stack
    let config = NetConfig::dhcpv4(Default::default());
    let stack = init_network_stack(&spawner, net_device, &RESOURCES, config);

    info!("waiting for DHCP…");
    while !stack.is_config_up() {
        Timer::after(Duration::from_millis(100)).await;
    }
    if let Some(StaticConfigV4 { address: cidr, .. }) = stack.config_v4() {
        let ip = cidr.address();
        let o = ip.octets();
        info!("IP = {}.{}.{}.{}", o[0], o[1], o[2], o[3]);
    }


    // status LED
    let mut tx_led = Output::new(p.PIN_2, Level::Low);
    let mut audio_out = Output::new(p.PIN_3, Level::Low);


    // reset line
    let mut rst = Output::new(p.PIN_8, Level::Low);
    Timer::after(Duration::from_millis(5)).await;
    rst.set_high();
    Timer::after(Duration::from_millis(50)).await;



    // async I2C1
    let mut i2c = I2c::new_async(
        p.I2C1,
        p.PIN_7,       // SDA
        p.PIN_6,       // SCL
        Irqs,
        I2cConfig::default(),
    );
    // anywhere in your async code, before you re-init the Si-4713:

// hold it low long enough for a complete reset:
Timer::after(Duration::from_micros(100)).await;
// bring it back up:
rst.set_high();
// wait for it to wake up
Timer::after(Duration::from_micros(100)).await;

Timer::after(Duration::from_millis(500)).await;

    // 1) POWER_UP for analog TX
    si47x_write_cmd(&mut i2c, &[0x01, 0b10010010, 0x10]).await
        .expect("POWER_UP failed");

    // Enable stereo + pilot in the composite signal:
    si47x_write_cmd(&mut i2c, &[
        0x12,       // CMD = SET_PROPERTY
        0x00,       // ARG1 = group
        0x21, 0x00, // ARG2/3 = prop 0x2100 TX_COMPONENT_ENABLE
        0x00, 0x01, // ARG4/5 = count = 1
        0x00, 0x03, // ARG6/7 = value = 0x0003 (stereo+pilot)
    ]).await
    .expect("SET_PROPERTY TX_COMPONENT_ENABLE failed");

    // Set pilot tone frequency to 600 Hz:
    si47x_write_cmd(&mut i2c, &[
        0x12,
        0x00,
        0x21, 0x07, // prop 0x2107 TX_PILOT_FREQUENCY
        0x00, 0x01, // count = 1
        0x02, 0x58, // value = 600 (0x0258)
    ]).await
    .expect("SET_PROPERTY TX_PILOT_FREQUENCY failed");

    // 4) Set TX power = 100 dBµV, auto-antenna cap
    si47x_write_cmd(&mut i2c, &[0x31, 0x00, 0x00, 0x64, 0x00]).await
        .expect("TX_TUNE_POWER failed");

    // 5) Tune to 100.00 MHz
    si47x_write_cmd(&mut i2c, &[0x30, 0x00, 0x27, 0x10]).await
        .expect("TX_TUNE_FREQ failed");
    // clear STC
    si47x_write_cmd(&mut i2c, &[0x14]).await
        .expect("GET_INT_STATUS failed");

    //read status
    let _ = read_tune_status(&mut i2c).await;


    let server = IpEndpoint::new(IpAddress::v4(192,168,241,175), 7000);
    info!("connecting to {}…", server);

    let mut rx = [0u8; 128];
    let mut tx = [0u8;   0];
    let mut socket = TcpSocket::new(stack, &mut rx, &mut tx);
    socket.set_timeout(None);



    loop {
        if socket.connect(server).await.is_ok() {
            info!("TCP connected");
            break;
        }
        warn!("connect failed, retrying…");
        Timer::after(Duration::from_secs(5)).await;
    }
    let mut current_freq_mhz: Option<f32> = None;
    let mut cipher_mode = CipherMode::None;

    // Read & broadcast in Morse
loop {
    let mut buf = [0u8; 64];

    loop {
        match socket.read(&mut buf).await {
            Ok(0) => {
                info!("server closed; will reconnect");
                break;
            }
            Ok(n) => {
                let msg = core::str::from_utf8(&buf[..n]).unwrap_or("<err>");
                info!("got raw `{}`", msg.trim_end());

                for raw_line in msg.lines() {
                    let line = raw_line.trim();
                    info!("line `{}`", line);
                    if line.is_empty() { continue; }

                    //Frequency command
                    if let Some(freq_str) = line.strip_prefix("FREQ=") {
                        if let Ok(mhz) = freq_str.parse::<f32>() {
                            let need_tune = match current_freq_mhz {
                                Some(prev) if (prev - mhz).abs() < f32::EPSILON => false,
                                _ => true,
                            };
                            if need_tune {
                                let [hi, lo] = mhz_to_bytes(mhz);
                                si47x_write_cmd(&mut i2c, &[0x30,0x00,hi,lo])
                                    .await.expect("TX_TUNE_FREQ failed");
                                si47x_write_cmd(&mut i2c, &[0x14])
                                    .await.expect("GET_INT_STATUS failed");
                                info!("Tuned to {} MHz", mhz);
                                current_freq_mhz = Some(mhz);
                            } else {
                                info!("Freq {} MHz unchanged, skipping retune", mhz);
                            }
                        } else {
                            warn!("Bad FREQ= value: {:?}", freq_str);
                        }
                        continue;
                    }

                    //Cipher selection command
                    if let Some(cmd) = line.strip_prefix("CIPHER=") {
                        // split into method and param
                        let mut parts = cmd.splitn(2, ':');
                        let method = parts.next().unwrap_or("");
                        let param  = parts.next().unwrap_or("");

                        cipher_mode = if method.eq_ignore_ascii_case("none") {
                            CipherMode::None

                        // —— Caesar: param is a numeric shift
                        } else if method.eq_ignore_ascii_case("caesar") {
                            let shift = param.parse().unwrap_or_else(|_| {
                                warn!("Caesar shift `{}` invalid, defaulting to 0", param);
                                0
                            });
                            CipherMode::Caesar { shift }

                        // —— Vigenère: param is the keyword (up to 16 chars)
                        } else if method.eq_ignore_ascii_case("vigenere") {
                            let mut key = KeyBuf::new();
                            key.push_str(param).ok();
                            CipherMode::Vigenere { key }

                        // —— Four-Square: param must be “KEY1,KEY2”
                        } else if method.eq_ignore_ascii_case("four-square") {
                            let mut kv = param.splitn(2, ',');
                            let mut k1 = KeyBuf::new();
                            let mut k2 = KeyBuf::new();
                            let s1 = kv.next().unwrap_or("");
                            let s2 = kv.next().unwrap_or("");
                            k1.push_str(s1).ok();
                            k2.push_str(s2).ok();

                            // if they forgot the second key, fall back or reuse the first
                            if k2.is_empty() {
                                warn!(
                                    "Four-Square needs two keys (e.g. CIPHER=four-square:KEY1,KEY2); \
                                    using KEY1 for both"
                                );
                                k2 = k1.clone();
                            }
                            CipherMode::FourSquare { key1: k1, key2: k2 }

                        // —— M-94: param is config string
                        } else if method.eq_ignore_ascii_case("m94") {
                        let mut cfg = KeyBuf::new();
                        cfg.push_str(param).ok();
                        CipherMode::M94 { config: cfg }

                        } else {
                            warn!("Unknown cipher `{}`", method);
                            cipher_mode.clone()
                        };

                        info!("Cipher set to `{}`", method);
                        continue;  // don’t treat this as a Morse message
                    }

                    // ——— Encrypt & send as Morse ———
                    let plaintext  = line;
                    let ciphertext = cipher_mode.apply(plaintext);
                    info!("Encrypted → `{}`", ciphertext);

                    // emit Morse for `ciphertext`
                    for ch in ciphertext.chars() {
                        if ch == '\r' || ch == '\n' { continue; }

                        if ch == ' ' {
                            Timer::after(UNIT * 7).await; // word gap
                            continue;
                        }

                        let pat = MORSE_TABLE.iter()
                            .find(|&&(c, _)| c == ch.to_ascii_uppercase())
                            .map(|&(_, p)| p)
                            .unwrap_or("");

                        for s in pat.chars() {
                            let dur = if s == '.' { UNIT } else { UNIT * 3 };
                            tx_led.set_high();
                            play_tone(dur, 600.0, &mut audio_out).await;
                            tx_led.set_low();
                            Timer::after(UNIT).await; // intra-symbol gap
                        }
                        Timer::after(UNIT * 2).await; // inter-letter gap
                    }
                }
            }
            Err(e) => {
                warn!("read error {:?}, retry shortly", e);
                Timer::after(Duration::from_millis(100)).await;
            }
        }
    }

    // reconnect on clean close
    Timer::after(Duration::from_secs(5)).await;
    loop {
        if socket.connect(server).await.is_ok() {
            info!("TCP reconnected");
            break;
        }
        warn!("reconnect failed…");
        Timer::after(Duration::from_secs(5)).await;
    }
}

}

fn mhz_to_bytes(mhz: f32) -> [u8; 2] {
    // round to nearest 10 kHz step
    let steps: u16 = libm::roundf(mhz * 100.0) as u16;
    [ (steps >> 8) as u8, (steps & 0xFF) as u8 ]
}

async fn read_tune_status(i2c: &mut I2c<'_, I2C1, Async>) -> [u8; 9] {
    let mut resp = [0u8; 9];
    // 0x22 = GET_TUNE_STATUS, 0x00 = INTACK (no interrupts to clear)
    i2c.write_read(SI4713_ADDR, &[0x22, 0x00], &mut resp)
        .await
        .unwrap();
    resp
}

/// Send a raw command and wait for CTS=1
async fn si47x_write_cmd(
    i2c: &mut I2c<'_, I2C1, Async>,
    cmd: &[u8],
) -> Result<(), embassy_rp::i2c::Error> {
    i2c.write(SI4713_ADDR, cmd).await?;
    // now wait for CTS=1
    let mut cts = [0u8];
    loop {
        i2c.write_read(SI4713_ADDR, &[0x0F], &mut cts).await?;
        if cts[0] & 0x80 != 0 { break; }
    }
    Ok(())
}

