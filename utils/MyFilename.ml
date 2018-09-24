open Core

let has_extension ~ext file =
  snd (Filename.split_extension file) = Some ext
