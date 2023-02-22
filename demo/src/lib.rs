#![no_std]

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
pub fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[no_mangle]
pub fn hello(nice: bool) -> &'static str {
    if nice {
        "you can have anything you want!"
    } else {
        "this is what you asked for so this is what you're getting"
    }
}

pub enum Tag {
    One,
    Two,
    Three(u32)
}

#[no_mangle]
pub fn tagliatelle(tag: Tag) -> i32 {
    let val = 321i32;
    match tag {
        Tag::One => add(1234, val),
        Tag::Two => add(5678, val),
        Tag::Three(v) => add(v as i32, val)
    }
}
