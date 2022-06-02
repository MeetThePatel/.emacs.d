(require 'init-package-management)
(require 'init-macos)
(require 'init-evil)
(require 'init-ui)
(require 'init-projectile)

(require 'init-org)
(require 'init-company)
(require 'init-flycheck)
(require 'init-lsp)

(require 'init-elisp)
(require 'init-misc)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2da6d7d38f7cd34253eb95f851ea42ca45699a0431d67a8fca77a7be79f16108" "6a5a600d3b2d8f8041958968724311f57cad64d973dfc4f7415f8893fbb8e4e8" "7dbaf030ee1a5106a9c3147b1c398d681a31dccde53c21e527ac01c89c6b9c6d" "84cbe717046277b5bae558b4a29274a993ca1867e310945913a81012a46110cd" "ed2a4e1b19b0e6b6656ec42dd4986e66c9bc411e9608d1abb5d603797a11ebb8" "1f901761f213924fff1eb6b9f40e726772d789bf57c4a0bf28b44fe53b16fb4f" "a94595ee0e321f6654f651c9003af4684cb2dd845cb4cbaecf0339381041b3e6" "cc3150430b3d031f72d339698f11afe69b0da39468fd9fa5c1afd1964e58baa6" "4f49121ff45246154617bb089d11f33f52845b4276b57a31f44153f444ce9d0a" "5e1a0f51ff049477b37142778db1045deb68fe15eee4fb0fffd9419a019ad84b" "d6ab1b477328336934da02cd97ba597671583933c9a96367d5c24232f47bec55" "5e0c9d72144e952a4a909897c4c87789cae1b99ca248336300d21653c310e5f0" "23b273472ddc80bf30402adbdfc08bcd1bf5b4e5204b9b1c489620d824e07df3" "1cb0c8c9a50bd4db9780133aed7915257bd2a568ffa67371cfadffcd9171bd73" "d56c2137fa4f957cf507a0bb37956fb9c6e88508eeccab49a5386a6a029d4f94" "2ad92c983b8c8e71703a7fe80dc3b5005a8c780326e04105a7d967a9535c6c9c" "f3aa50f199c8f79a56c729bcd4962197ff79745394f9c3b170d30b3b0295a688" "b62f735a8701c6d9aa5acb64c92862fc90539548bc341534de3d693ac0a8136e" "5bc3759f7ba3a395f22d8b2ecc1123e4c6485c8d48b45842a945a27be0429e17" "c20e7571709f3d44e7d72cbf7512d3baab79f7df2c54c1b0c727b22746a2d254" "911362a72eaee436ad7bb711c07aaf8440e6303fc71dcbd773bd236ac8c69995" "9d0726ba35cdfb4d4eaa756920c041f9a345fdc8247e2be86cea2d185a3abb07" "8f9822a297052b2b8bcc1282bde7d3cd06c31508984783b0174c72d02d31d122" "2f8d724dd4539c2fda14def4fd2b7910f839f7c87c7c98907d1b29d27a683f69" "5646d77de311d5b7309258e1107e0e2f174d0f15663558bc73bbb528dfc89f26" "50544fe33feff6adc20dbaeeb514f6f56b6d439a1ada511e737f999d1d5af70e" "b5f1b71cc2c3f442809ec360d77213945da177e56d011a6959909ca19b9e9250" "e3bda7df8018cedce3306ae6244e62cad41a02f4082570b541912c7d695c9283" "9ec65287f4425ac9ff28b19511ce88d7c73bba29af196c9ab9cdeeff64276732" "fa59051b013f6113b5778356fd386191d02fcba457027efede30016e19184f67" "8738787d8e982eefe68d220e324af26e1c0eaac65ca4f6c5c08af5fea32a8817" "67e8f18b4f332f38f5cf8f9bb8ea24132fce09e4ade0cf09655c066e417055be" "6e7cd2b168f8da63b421707bc3435fdb13b8f04eebdc041e09cb10418e873916" "5e9891ed8d69b7ab11abcc2262faa1c19f69df984a536abfb6db5e23f0137d48" "1a0a4efef8d722894e391987128f8d30ea711279669a5c91530f67effc789e03" "f4f7fe95e16c488c3431df9808032c55b174dd3962339975f9dd4c99776ed028" "a76b5c1370a902626c868f91e5d0f1d3b2a709e9a30d2302b556723c448e6c7d" "274db8bb5d4898a3f8ea648827290b83e7022384ce9b5832331740b9e2ff1420" "d6c47ade87530145a621d08880af31c7e4efc7d1f1f910064e3bcf0e01d803e1" "5cb2a37edf486a2af24c3b75495dde835b3c8661ac758c5f5bec00782c416c91" "d8bf6b7fcff25d8340243e8eab17afa8c2ccc35a7e20afc1c05eefb9d7f1d350" "d2cc421ae37e2ece119a7e1fe8f79ef3d7c71c67b7df6495be7f24ca39b7fc40" "fae0a0eae3345249ed8b3245861fdfa4425324f9872abb9737bbe77dc028f2fb" "5c267fb340bd16bbc88e4535131703cb057f5dddcb9d8be9b419fff00c3579c8" "79a1e4e2cbea8c4ea16fa9a34e2d6fe52acb0135b7ef7a03988e222d948014dc" "be62fa2bf34cf32add4e8f5bc5068f21b8a0f220a3603335a255ab1220a62de5" "97b8848481c914daa09f119814a698dccea6fdde1204f971d9411d5c20b15c6a" "4c0088f254cdce578fd300ac3f8b6deb4fa6a3571ebfbae078cda5c34d1cfb35" "6200f91f582274763e22768dac6c3b630e2f5846ab79618ddac3412493a3738e" "46b8ce795c3a6873e45640361c75d28df39035111347a95e5cb9fcbe39e860d3" "541764649c6431807849def0f91b56715fde4c1d9db15b85a5c0e4731634ab35" default))
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(org-default ((t (:background "#FFFFFF" :foreground "#000000" :family "SF Pro Rounded")))))
(put 'narrow-to-region 'disabled nil)
