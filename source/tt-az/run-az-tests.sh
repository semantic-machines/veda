#cargo build
#cp target/debug/libtarantool_veda.so tt/
cargo build --release
cp target/release/libtarantool_veda.so tt/db/tarantool/
cd tt
#./clean-db.sh
tarantool init-az.lua