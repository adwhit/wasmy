blob := "demo/target/wasm32-unknown-unknown/release/demo.wasm"

run opt="":
    cargo run --bin iii {{opt}} {{blob}} exec whatcha_get

show:
    cargo run --bin iii {{blob}} show

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
