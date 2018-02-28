FROM alpine

COPY docker-rootfs/nix /nix
COPY docker-rootfs/root /root
COPY docker-rootfs/bin /bin

ENTRYPOINT /bin/cyanide
