;; Guix has wlroots 14.1 & I need wlroots 15.0
;; Guix is still using wlroots from github!
(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (gnu packages wm)
             (gnu packages xdisorg)
             (gnu packages freedesktop)
             (gnu packages pkg-config)
             (gnu packages base)
             (gnu packages commencement))

;; Dependency libdrm found: NO found 2.4.109 but need: '>=2.4.113'
(define libdrm
  (package
    (inherit libdrm)
    (version "2.4.113")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1qg54drng3mxm64dsxgg0l6li4yrfzi50bgj0r3fnfzncwlypmvz"))))))

;; Dependency wayland-protocols found: NO found 1.24 but need: '>=1.26'
(define wayland-protocols
  (package
    (inherit wayland-protocols)
    (version "1.26")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "04vgllmpmrv14x3x64ns01vgwx4hriljayjkz9idgbv83i63hly5"))))))

;; Dependency wayland-server found: NO found 1.20.0 but need: '>=1.21'
(define wayland
  (package
    (inherit wayland)
    (version "1.21.0")
    (source (origin
              (method url-fetch)
              (uri "https://gitlab.freedesktop.org/wayland/wayland/-/releases/1.21.0/downloads/wayland-1.21.0.tar.xz")
              (sha256
               (base32
                "1b0ixya9bfw5c9jx8mzlr7yqnlyvd3jv5z8wln9scdv8q5zlvikd"))))))

(define wlroots-next
  (let ((commit "11192e69308ff48c0f3ec40fb572c4e8e4ad13d8")
        (revision "0"))
    (package
      (inherit wlroots)
      (name "wlroots")
      (version (git-version "0.15.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11kwiqksl9fq6lq2zrygva2igcdn000l8s3a57dwnn57wwggzdk5"))))
      (propagated-inputs (modify-inputs (package-propagated-inputs wlroots)
                           (append libdrm)
                           (replace "wayland-protocols" wayland-protocols)
                           (replace "wayland" wayland))))))

(packages->manifest (list wlroots-next
                          wayland-protocols
                          gnu-make
                          pkg-config
                          gcc-toolchain))
