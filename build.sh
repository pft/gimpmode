#!/bin/sh

VERSION=`head -1 gimp-mode.el|awk -F " " '{print $6}'`

emacs --batch --script build.el
mkdir gimp-mode-v$VERSION
cp -R emacs-interaction.scm gimp-mode.el gimp-install.el gimp-init.el gimpmode.pdf related INSTALL COPYING ChangeLog gimp-mode-v$VERSION
find gimp-mode-v$VERSION -regex ".*\/\(CVS\|.cvsignore\|.*\(~\|\.elc\)\)"|xargs rm -Rf
echo "Building tar file..."
tar -cf gimp-mode-v$VERSION.tar gimp-mode-v$VERSION
bzip2 gimp-mode-v$VERSION.tar
echo "Building zip file..."
zip -r gimp-mode-v$VERSION.zip gimp-mode-v$VERSION
echo "Removing temporary directory..."
rm -R gimp-mode-v$VERSION
TMPDIR=`mktemp -d`
cp gimpmode.html gimp-mode-v$VERSION.tar.bz2 gimp-mode-v$VERSION.zip $TMPDIR
mv gimp-mode-v$VERSION.tar.bz2 $TMPDIR/gimp-mode.tar.bz2
mv gimp-mode-v$VERSION.zip $TMPDIR/gimp-mode.zip
echo "Uploading files..."
scp $TMPDIR/* skrinka:/var/www/public/GimpMode

