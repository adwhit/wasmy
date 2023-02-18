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
