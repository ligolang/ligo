sudo apt-get install -y make \
     m4 \
     gcc \
     patch \
     bubblewrap \
     rsync \
     curl \
     
if [ -n "`uname -a | grep -i ubuntu`" ]
then
    sudo add-apt-repository -y ppa:avsm/ppa
    sudo apt-get update
    sudo apt-get install -y opam
else
    # I'm going to assume here that we're on x86_64, 32-bit users should be basically
    # extinct at this point right?
    curl -L https://github.com/ocaml/opam/releases/download/2.0.4/opam-2.0.4-x86_64-linux \
	 --output opam
    if [ `openssl sha256 -r opam` = \
	 "373e34f92f282273d482537f8103caad0d17b6f2699ff504bed77f474cb0c951 *opam" ]
    then
	   sudo mv opam /usr/local/bin/opam
	   sudo chmod +x /usr/local/bin/opam
    else
	echo "opam file hash doesn't match what was recorded at time of signature verification!"
	echo "(If you actually get this message, you should probably file an issue)"
	exit 1
    fi	   	   
fi

opam init -a --bare
