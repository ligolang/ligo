# Maintainer: LigoLang <infra at marigold dot dev>
pkgname=ligo-bin
pkgver=LIGO_VERSION_PLACEHOLDER
pkgrel=1
pkgdesc="High Level Smart Contract Language for Tezos (binary)"
url="https://gitlab.com/ligolang/ligo"

arch=('x86_64')
license=('MIT')

provides=('ligo')
conflicts=('ligo' 'ligo-next')

source=(DEB_PKG_URL_PLACEHOLDER)
md5sums=('SKIP')

prepare() {
        cd "$srcdir/"
        tar xvf data.tar.xz -C .
}

package() {
        cd "$srcdir/"
        cp -r usr ${pkgdir}
}
