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

;; Dependency libdrm found: NO found 2.4.107 but need: '>=2.4.109'
(define libdrm
  (package
    (inherit libdrm)
    (version "2.4.109")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "09kzrdsd14zr0i3izvi5mck4vqccl3c9hr84r9i4is0zikh554v2"))))))

;; Dependency wayland-protocols found: NO found 1.23 but need: '>=1.24'
(define wayland-protocols
  (package
    (inherit wayland-protocols)
    (version "1.24")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "1hlb6gvyqlmsdkv5179ccj07p04cn6xacjkgklakbszczv7xiw5z"))))))

(define wlroots-15
  (package
    (inherit wlroots)
    (name "wlroots")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wdzs0wpv61pxgy3mx3xjsndyfmbj30v47d3w9ymmnd4r479n41n"))))
    (propagated-inputs (modify-inputs (package-propagated-inputs wlroots)
                         (append libdrm)
                         (replace "wayland-protocols" wayland-protocols)))))

(packages->manifest (list wlroots-15
                          wayland-protocols
                          gnu-make
                          pkg-config
                          gcc-toolchain))
