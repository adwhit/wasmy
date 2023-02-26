#![no_std]

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
#[inline(never)]
pub fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[no_mangle]
pub fn do_add() -> i32 {
    add(add(add(add(1, 3), 5), 7), 9)
}

#[no_mangle]
pub fn loop_add() -> i32 {
    let mut inc = 0;
    for _ in 0..500 {
        inc = add(inc, 1);
    }
    inc
}

#[no_mangle]
#[inline(never)]
pub fn hello(nice: bool) -> &'static str {
    if nice {
        "you can have anything you want!"
    } else {
        "this is what you asked for so this is what you're getting"
    }
}

#[no_mangle]
#[inline(never)]
pub fn goodbye(nasty: bool) -> &'static str {
    if nasty {
        "you can have anything you want! Or maybe not"
    } else {
        "this is what you asked for so this is what you're getting"
    }
}

pub enum Tag {
    One,
    Two,
    Three(u32),
}

#[no_mangle]
pub fn tagliatelle(tag: Tag) -> i32 {
    let val = 321i32;
    match tag {
        Tag::One => add(1234, val),
        Tag::Two => add(5678, val),
        Tag::Three(v) => add(v as i32, val),
    }
}

#[no_mangle]
pub fn whatcha_get(tag: i32) -> &'static str {
    if tag < 0 {
        "some thing some thing"
    } else {
        hello(tag > 50)
    }
}

#[no_mangle]
#[inline(never)]
pub fn get_tuple(x: i32) -> (i32, i32) {
    (x - 1, x + 1)
}

#[no_mangle]
pub fn do_get_tuple() -> i32 {
    get_tuple(10).0
}
