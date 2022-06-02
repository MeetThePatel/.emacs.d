(use-package smudge
  :straight t
  :config
  (setq smudge-oauth2-client-id "e0de5180115f4f80a1acd75563789c56"
	smudge-oauth2-client-secret "4bc036679f4c415c8db41e9703ec42ec"
	smudge-transport 'connect)
  (defhydra hydra-spotify (:hint nil)
    "
^Search^                  ^Control^               ^Manage^
^^^^^^^^-----------------------------------------------------------------
_t_: Track               _SPC_: Play/Pause        _k_: Volume up
_m_: My Playlists        _l_  : Next Track        _j_: Volume down
_f_: Featured Playlists  _h_  : Previous Track    _x_: Mute
_u_: User Playlists      _r_  : Repeat            _d_: Device
^^                       _s_  : Shuffle           _q_: Quit
"
    ("t" smudge-track-search :exit t)
    ("m" smudge-my-playlists :exit t)
    ("f" smudge-featured-playlists :exit t)
    ("u" smudge-user-playlists :exit t)
    ("SPC" smudge-controller-toggle-play :exit nil)
    ("l" smudge-controller-next-track :exit nil)
    ("h" smudge-controller-previous-track :exit nil)
    ("r" smudge-controller-toggle-repeat :exit nil)
    ("s" smudge-controller-toggle-shuffle :exit nil)
    ("k" smudge-controller-volume-up :exit nil)
    ("j" smudge-controller-volume-down :exit nil)
    ("x" smudge-controller-volume-mute-unmute :exit nil)
    ("d" smudge-select-device :exit nil)
    ("q" quit-window "quit" :color blue))
  (tyrant-def
    :states 'normal
    :keymaps 'override

    "<f6>" '(hydra-spotify/body :which-key "Spotify")))
(provide 'init-misc)
