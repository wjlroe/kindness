#!/bin/sh

lein cljsbuild once release

rm -rf build
if [ -f kindness.zip ]; then
    rm kindness.zip
fi
mkdir -p build/js/compiled/out-adv
cp resources/public/js/compiled/out-adv/kindness.min.js build/js/compiled/out-adv/
cp -r resources/public/fonts build/
cp -r resources/public/images build/
cp -r resources/public/music build/
cp resources/public/index_release.html build/index.html

pushd build
zip ../kindness.zip -r .
popd

rm -f *-init.clj
