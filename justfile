
run:
    cargo run --bin iii demo/target/wasm32-unknown-unknown/release/demo.wasm


build:
    cd demo && cargo build --target wasm32-unknown-unknown --release
    cargo build

dis:
    wasm-dis demo/target/wasm32-unknown-unknown/release/demo.wasm

dump:
    xxd demo/target/wasm32-unknown-unknown/release/demo.wasm

clean:
    cargo clean
    cd demo && cargo clean

fmt:
    cargo fmt
    cd demo && cargo fmt
