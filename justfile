blob := "demo/target/wasm32-unknown-unknown/release/demo.wasm"

exec func="whatcha_get" opt="":
    cargo run --bin iii {{opt}} {{blob}} exec {{func}}

show:
    cargo run --bin iii {{blob}} show

validate:
    cargo run --bin iii {{blob}} validate

build:
    cd demo && cargo build --target wasm32-unknown-unknown --release
    cargo build

dis:
    wasm-dis {{blob}}

dump:
    xxd {{blob}}

clean:
    cargo clean
    cd demo && cargo clean

fmt:
    cargo fmt
    cd demo && cargo fmt
