#!/bin/sh

set -e

cd "$(dirname "$0")"

APP_NAME=Nemu
BUILD_DIR=build
APP_DIR="$BUILD_DIR/Nemu.app"

echo "Creating app structure"
rm -rf "$APP_DIR"
mkdir -p "$APP_DIR/Contents/MacOS"
mkdir -p "$APP_DIR/Contents/Resources"

echo "Creating icon"
# for size in 16 32 64 128 256 512; do
#     sips -z "$size" "$size" "$MACOS_APP_DIR/Contents/Resources"
# done
cp ../../../misc/logo.png "$APP_DIR/Contents/Resources/Icon.png"

echo "Compiling nemu application"
cargo build --release

echo "Copying executable"
cp ../../../target/release/nemu-gui "$APP_DIR/Contents/MacOS/nemu"

echo "Copying app info"
cp ./Info.plist "$APP_DIR/Contents/Info.plist"

ln -s /Applications "$BUILD_DIR/Applications"

echo "Creating DMG"
hdiutil create "$APP_NAME.dmg" -srcfolder "$BUILD_DIR" -ov

echo "Cleaning up"
rm -rf "$BUILD_DIR"
