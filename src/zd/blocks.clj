(ns zd.blocks
  (:require
   ;; by content type
   [zd.blocks.content]

   ;; by document keys
   [zd.blocks.keys]

   [zd.blocks.widgets]

   ;; blocks defined by zentext content type
   [zd.blocks.zentext]

   ;; blocks defined with :zd/ prefix
   [zd.blocks.zd]))

