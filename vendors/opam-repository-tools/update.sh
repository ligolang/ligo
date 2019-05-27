MESSAGE=$1
if [ -z "$MESSAGE" ] ; then
    echo "needs a commit message"
    exit 1
fi
opam admin index
opam admin cache
git add .
git commit -m "$MESSAGE"
git push
opam update new-galfour-tezos
