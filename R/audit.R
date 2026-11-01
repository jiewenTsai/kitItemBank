# R/audit.R — 題庫內容的檢查（不碰 Shiny）
#
# bank_audit() 找出要人看一下的地方，一列一個問題；只找、不改。
# check_bank() 管「能不能用」（重複鍵、孤兒列…），這裡管「內容對不對」（遺漏代碼撞號、
# 反向計分、簡體字…）。題庫編修頁的「要注意」表就是這裡的結果。

# 每一種檢查：代碼、等級、畫面上的名稱（照這個順序排）
AUDIT_CHECKS <- data.frame(
  check = c("noresponse_clash", "noresponse_shared", "reverse_mismatch", "dim_not_inherited",
            "value_empty", "simplified_char", "value_scale_only", "itemtype_empty",
            "versions_same_wave", "itemver_gap", "dim_single_item", "dim_only_deprecated",
            "text_newline", "text_space", "text_brackets"),
  level = c("todo", "todo", "todo", "todo",
            "todo", "todo", "note", "note",
            "note", "note", "note", "note",
            "note", "note", "note"),
  # 原文：遺漏代碼和選項撞號 | 兩種遺漏值用同一個代碼 | 反向計分和「反向題」構面不一致
  label = c("\u907a\u6f0f\u4ee3\u78bc\u548c\u9078\u9805\u649e\u865f", "\u5169\u7a2e\u907a\u6f0f\u503c\u7528\u540c\u4e00\u500b\u4ee3\u78bc", "\u53cd\u5411\u8a08\u5206\u548c\u300c\u53cd\u5411\u984c\u300d\u69cb\u9762\u4e0d\u4e00\u81f4",
            # 原文：新版本沒有構面（舊版有）
            "\u65b0\u7248\u672c\u6c92\u6709\u69cb\u9762\uff08\u820a\u7248\u6709\uff09",
            # 原文：選項空白 | 簡體字 | 選項只寫量表點數 | 題型空白
            "\u9078\u9805\u7a7a\u767d", "\u7c21\u9ad4\u5b57", "\u9078\u9805\u53ea\u5beb\u91cf\u8868\u9ede\u6578", "\u984c\u578b\u7a7a\u767d",
            # 原文：同一題兩版在同一波 | 版本號不連號 | 構面只有 1 題 | 構面只剩停用的題
            "\u540c\u4e00\u984c\u5169\u7248\u5728\u540c\u4e00\u6ce2", "\u7248\u672c\u865f\u4e0d\u9023\u865f", "\u69cb\u9762\u53ea\u6709 1 \u984c", "\u69cb\u9762\u53ea\u5269\u505c\u7528\u7684\u984c",
            # 原文：文字有換行 | 文字前後有空白 | 括號不成對
            "\u6587\u5b57\u6709\u63db\u884c", "\u6587\u5b57\u524d\u5f8c\u6709\u7a7a\u767d", "\u62ec\u865f\u4e0d\u6210\u5c0d"),
  stringsAsFactors = FALSE
)

# 只在簡體出現的字 → 繁體（台灣不會這樣寫的字；了 布 干 斗 沉 周 后 這類兩邊都用的不放）
AUDIT_SIMPLIFIED <- c(
  # 原文：髋 | 髖 | 内 | 內 | 这 | 這 | 个 | 個 | 们 | 們 | 来 | 來 | 时 | 時
  "\u9acb" = "\u9ad6", "\u5185" = "\u5167", "\u8fd9" = "\u9019", "\u4e2a" = "\u500b", "\u4eec" = "\u5011", "\u6765" = "\u4f86", "\u65f6" = "\u6642",
  # 原文：说 | 說 | 为 | 為 | 对 | 對 | 学 | 學 | 发 | 發 | 问 | 問 | 么 | 麼
  "\u8bf4" = "\u8aaa", "\u4e3a" = "\u70ba", "\u5bf9" = "\u5c0d", "\u5b66" = "\u5b78", "\u53d1" = "\u767c", "\u95ee" = "\u554f", "\u4e48" = "\u9ebc",
  # 原文：关 | 關 | 动 | 動 | 实 | 實 | 现 | 現 | 应 | 應 | 过 | 過 | 还 | 還
  "\u5173" = "\u95dc", "\u52a8" = "\u52d5", "\u5b9e" = "\u5be6", "\u73b0" = "\u73fe", "\u5e94" = "\u61c9", "\u8fc7" = "\u904e", "\u8fd8" = "\u9084",
  # 原文：进 | 進 | 样 | 樣 | 经 | 經 | 让 | 讓 | 认 | 認 | 识 | 識 | 请 | 請
  "\u8fdb" = "\u9032", "\u6837" = "\u6a23", "\u7ecf" = "\u7d93", "\u8ba9" = "\u8b93", "\u8ba4" = "\u8a8d", "\u8bc6" = "\u8b58", "\u8bf7" = "\u8acb",
  # 原文：话 | 話 | 语 | 語 | 读 | 讀 | 写 | 寫 | 买 | 買 | 卖 | 賣 | 东 | 東
  "\u8bdd" = "\u8a71", "\u8bed" = "\u8a9e", "\u8bfb" = "\u8b80", "\u5199" = "\u5beb", "\u4e70" = "\u8cb7", "\u5356" = "\u8ce3", "\u4e1c" = "\u6771",
  # 原文：车 | 車 | 长 | 長 | 门 | 門 | 间 | 間 | 题 | 題 | 页 | 頁 | 见 | 見
  "\u8f66" = "\u8eca", "\u957f" = "\u9577", "\u95e8" = "\u9580", "\u95f4" = "\u9593", "\u9898" = "\u984c", "\u9875" = "\u9801", "\u89c1" = "\u898b",
  # 原文：视 | 視 | 觉 | 覺 | 亲 | 親 | 爱 | 愛 | 儿 | 兒 | 妈 | 媽 | 电 | 電
  "\u89c6" = "\u8996", "\u89c9" = "\u89ba", "\u4eb2" = "\u89aa", "\u7231" = "\u611b", "\u513f" = "\u5152", "\u5988" = "\u5abd", "\u7535" = "\u96fb",
  # 原文：脑 | 腦 | 网 | 網 | 饭 | 飯 | 头 | 頭 | 体 | 體 | 医 | 醫 | 药 | 藥
  "\u8111" = "\u8166", "\u7f51" = "\u7db2", "\u996d" = "\u98ef", "\u5934" = "\u982d", "\u4f53" = "\u9ad4", "\u533b" = "\u91ab", "\u836f" = "\u85e5",
  # 原文：疗 | 療 | 环 | 環 | 欢 | 歡 | 乐 | 樂 | 级 | 級 | 线 | 線 | 给 | 給
  "\u7597" = "\u7642", "\u73af" = "\u74b0", "\u6b22" = "\u6b61", "\u4e50" = "\u6a02", "\u7ea7" = "\u7d1a", "\u7ebf" = "\u7dda", "\u7ed9" = "\u7d66",
  # 原文：习 | 習 | 惯 | 慣 | 难 | 難 | 帮 | 幫 | 带 | 帶 | 备 | 備 | 选 | 選
  "\u4e60" = "\u7fd2", "\u60ef" = "\u6163", "\u96be" = "\u96e3", "\u5e2e" = "\u5e6b", "\u5e26" = "\u5e36", "\u5907" = "\u5099", "\u9009" = "\u9078",
  # 原文：择 | 擇 | 检 | 檢 | 历 | 歷 | 产 | 產 | 务 | 務 | 员 | 員 | 总 | 總
  "\u62e9" = "\u64c7", "\u68c0" = "\u6aa2", "\u5386" = "\u6b77", "\u4ea7" = "\u7522", "\u52a1" = "\u52d9", "\u5458" = "\u54e1", "\u603b" = "\u7e3d",
  # 原文：统 | 統 | 计 | 計 | 设 | 設 | 论 | 論 | 记 | 記 | 忆 | 憶 | 运 | 運
  "\u7edf" = "\u7d71", "\u8ba1" = "\u8a08", "\u8bbe" = "\u8a2d", "\u8bba" = "\u8ad6", "\u8bb0" = "\u8a18", "\u5fc6" = "\u61b6", "\u8fd0" = "\u904b",
  # 原文：练 | 練 | 场 | 場 | 园 | 園 | 区 | 區 | 鸡 | 雞 | 鱼 | 魚 | 鸟 | 鳥
  "\u7ec3" = "\u7df4", "\u573a" = "\u5834", "\u56ed" = "\u5712", "\u533a" = "\u5340", "\u9e21" = "\u96de", "\u9c7c" = "\u9b5a", "\u9e1f" = "\u9ce5",
  # 原文：龙 | 龍 | 马 | 馬 | 试 | 試 | 谁 | 誰 | 谈 | 談 | 调 | 調 | 负 | 負
  "\u9f99" = "\u9f8d", "\u9a6c" = "\u99ac", "\u8bd5" = "\u8a66", "\u8c01" = "\u8ab0", "\u8c08" = "\u8ac7", "\u8c03" = "\u8abf", "\u8d1f" = "\u8ca0",
  # 原文：责 | 責 | 费 | 費 | 资 | 資 | 质 | 質 | 钱 | 錢 | 错 | 錯 | 队 | 隊
  "\u8d23" = "\u8cac", "\u8d39" = "\u8cbb", "\u8d44" = "\u8cc7", "\u8d28" = "\u8cea", "\u94b1" = "\u9322", "\u9519" = "\u932f", "\u961f" = "\u968a",
  # 原文：际 | 際 | 随 | 隨 | 须 | 須 | 预 | 預 | 领 | 領 | 颜 | 顏 | 饿 | 餓
  "\u9645" = "\u969b", "\u968f" = "\u96a8", "\u987b" = "\u9808", "\u9884" = "\u9810", "\u9886" = "\u9818", "\u989c" = "\u984f", "\u997f" = "\u9913",
  # 原文：惊 | 驚 | 愿 | 願 | 态 | 態 | 众 | 眾 | 兴 | 興 | 举 | 舉 | 书 | 書
  "\u60ca" = "\u9a5a", "\u613f" = "\u9858", "\u6001" = "\u614b", "\u4f17" = "\u773e", "\u5174" = "\u8208", "\u4e3e" = "\u8209", "\u4e66" = "\u66f8",
  # 原文：乱 | 亂 | 从 | 從 | 价 | 價 | 优 | 優 | 伤 | 傷 | 养 | 養 | 决 | 決
  "\u4e71" = "\u4e82", "\u4ece" = "\u5f9e", "\u4ef7" = "\u50f9", "\u4f18" = "\u512a", "\u4f24" = "\u50b7", "\u517b" = "\u990a", "\u51b3" = "\u6c7a",
  # 原文：别 | 別 | 办 | 辦 | 劳 | 勞 | 单 | 單 | 压 | 壓 | 变 | 變 | 号 | 號
  "\u522b" = "\u5225", "\u529e" = "\u8fa6", "\u52b3" = "\u52de", "\u5355" = "\u55ae", "\u538b" = "\u58d3", "\u53d8" = "\u8b8a", "\u53f7" = "\u865f",
  # 原文：吗 | 嗎 | 听 | 聽 | 响 | 響 | 图 | 圖 | 坏 | 壞 | 块 | 塊 | 护 | 護
  "\u5417" = "\u55ce", "\u542c" = "\u807d", "\u54cd" = "\u97ff", "\u56fe" = "\u5716", "\u574f" = "\u58de", "\u5757" = "\u584a", "\u62a4" = "\u8b77",
  # 原文：报 | 報 | 换 | 換 | 数 | 數 | 断 | 斷 | 旧 | 舊 | 显 | 顯 | 杂 | 雜
  "\u62a5" = "\u5831", "\u6362" = "\u63db", "\u6570" = "\u6578", "\u65ad" = "\u65b7", "\u65e7" = "\u820a", "\u663e" = "\u986f", "\u6742" = "\u96dc",
  # 原文：条 | 條 | 极 | 極 | 没 | 沒 | 满 | 滿 | 灯 | 燈 | 点 | 點 | 热 | 熱
  "\u6761" = "\u689d", "\u6781" = "\u6975", "\u6ca1" = "\u6c92", "\u6ee1" = "\u6eff", "\u706f" = "\u71c8", "\u70b9" = "\u9ede", "\u70ed" = "\u71b1",
  # 原文：爷 | 爺 | 画 | 畫 | 确 | 確 | 礼 | 禮 | 离 | 離 | 种 | 種 | 称 | 稱
  "\u7237" = "\u723a", "\u753b" = "\u756b", "\u786e" = "\u78ba", "\u793c" = "\u79ae", "\u79bb" = "\u96e2", "\u79cd" = "\u7a2e", "\u79f0" = "\u7a31",
  # 原文：笔 | 筆 | 紧 | 緊 | 红 | 紅 | 约 | 約 | 纸 | 紙 | 组 | 組 | 细 | 細
  "\u7b14" = "\u7b46", "\u7d27" = "\u7dca", "\u7ea2" = "\u7d05", "\u7ea6" = "\u7d04", "\u7eb8" = "\u7d19", "\u7ec4" = "\u7d44", "\u7ec6" = "\u7d30",
  # 原文：织 | 織 | 终 | 終 | 结 | 結 | 绝 | 絕 | 继 | 繼 | 续 | 續 | 绿 | 綠
  "\u7ec7" = "\u7e54", "\u7ec8" = "\u7d42", "\u7ed3" = "\u7d50", "\u7edd" = "\u7d55", "\u7ee7" = "\u7e7c", "\u7eed" = "\u7e8c", "\u7eff" = "\u7da0",
  # 原文：职 | 職 | 联 | 聯 | 肤 | 膚 | 脸 | 臉 | 节 | 節 | 虑 | 慮 | 虽 | 雖
  "\u804c" = "\u8077", "\u8054" = "\u806f", "\u80a4" = "\u819a", "\u8138" = "\u81c9", "\u8282" = "\u7bc0", "\u8651" = "\u616e", "\u867d" = "\u96d6",
  # 原文：补 | 補 | 装 | 裝 | 规 | 規 | 讲 | 講 | 许 | 許 | 证 | 證 | 评 | 評
  "\u8865" = "\u88dc", "\u88c5" = "\u88dd", "\u89c4" = "\u898f", "\u8bb2" = "\u8b1b", "\u8bb8" = "\u8a31", "\u8bc1" = "\u8b49", "\u8bc4" = "\u8a55",
  # 原文：该 | 該 | 误 | 誤 | 课 | 課 | 谢 | 謝 | 购 | 購 | 转 | 轉 | 较 | 較
  "\u8be5" = "\u8a72", "\u8bef" = "\u8aa4", "\u8bfe" = "\u8ab2", "\u8c22" = "\u8b1d", "\u8d2d" = "\u8cfc", "\u8f6c" = "\u8f49", "\u8f83" = "\u8f03",
  # 原文：输 | 輸 | 边 | 邊 | 达 | 達 | 远 | 遠 | 连 | 連 | 迟 | 遲 | 邮 | 郵
  "\u8f93" = "\u8f38", "\u8fb9" = "\u908a", "\u8fbe" = "\u9054", "\u8fdc" = "\u9060", "\u8fde" = "\u9023", "\u8fdf" = "\u9072", "\u90ae" = "\u90f5",
  # 原文：钟 | 鐘 | 镜 | 鏡 | 闻 | 聞 | 阅 | 閱 | 阳 | 陽 | 阴 | 陰 | 险 | 險
  "\u949f" = "\u9418", "\u955c" = "\u93e1", "\u95fb" = "\u805e", "\u9605" = "\u95b1", "\u9633" = "\u967d", "\u9634" = "\u9670", "\u9669" = "\u96aa",
  # 原文：静 | 靜 | 顶 | 頂 | 项 | 項 | 顾 | 顧 | 饮 | 飲 | 饱 | 飽 | 龄 | 齡
  "\u9759" = "\u975c", "\u9876" = "\u9802", "\u9879" = "\u9805", "\u987e" = "\u9867", "\u996e" = "\u98f2", "\u9971" = "\u98fd", "\u9f84" = "\u9f61",
  # 原文：齿 | 齒 | 岁 | 歲 | 双 | 雙 | 气 | 氣 | 汤 | 湯 | 训 | 訓 | 议 | 議
  "\u9f7f" = "\u9f52", "\u5c81" = "\u6b72", "\u53cc" = "\u96d9", "\u6c14" = "\u6c23", "\u6c64" = "\u6e6f", "\u8bad" = "\u8a13", "\u8bae" = "\u8b70",
  # 原文：询 | 詢 | 广 | 廣 | 卫 | 衛 | 宝 | 寶 | 师 | 師 | 当 | 當 | 录 | 錄
  "\u8be2" = "\u8a62", "\u5e7f" = "\u5ee3", "\u536b" = "\u885b", "\u5b9d" = "\u5bf6", "\u5e08" = "\u5e2b", "\u5f53" = "\u7576", "\u5f55" = "\u9304",
  # 原文：怀 | 懷 | 战 | 戰 | 妇 | 婦 | 孙 | 孫 | 厕 | 廁 | 睁 | 睜
  "\u6000" = "\u61f7", "\u6218" = "\u6230", "\u5987" = "\u5a66", "\u5b59" = "\u5b6b", "\u5395" = "\u5ec1", "\u7741" = "\u775c"
)

# 檢查會看的文字欄：畫面上的名稱
# 原文：題目文字 | 題幹（stemID） | 選項
AUDIT_TEXT_COLS <- c(label = "\u984c\u76ee\u6587\u5b57", stemID = "\u984c\u5e79\uff08stemID\uff09", value = "\u9078\u9805",
                     # 原文：遺漏值 | 題組 | 填答者
                     noResponse = "\u907a\u6f0f\u503c", block = "\u984c\u7d44", respondent = "\u586b\u7b54\u8005",
                     # 原文：備註
                     note = "\u5099\u8a3b")

# 把一段文字縮到 n 個字，換行換成空白
audit_short <- function(x, n = 30L) {
  x <- gsub("[\r\n]+", " ", x)
  # 原文：…
  ifelse(nchar(x) > n, paste0(substr(x, 1L, n), "\u2026"), x)
}

# value 解析成選項代碼：每個 entry（用 ; 或換行分開）開頭是整數，後面接空白、:、：、= 或結束
# 回傳 list，每一列一個整數向量（不是編碼格式時是 integer(0)）
audit_value_codes <- function(value) {
  # 原文：[;；\r\n]+
  parts <- strsplit(as.character(ifelse(is.na(value), "", value)), "[;\uff1b\r\n]+", perl = TRUE)
  lapply(parts, function(p) {
    p <- trimws(p)
    # 原文：^-?[0-9]+([ \t:：=]|$)
    p <- p[grepl("^-?[0-9]+([ \t:\uff1a=]|$)", p, perl = TRUE)]
    unique(as.integer(sub("^(-?[0-9]+).*$", "\\1", p, perl = TRUE)))
  })
}

# noResponse 解析成 data.frame(row, label, code, text)：「不適用:9995; 不知道:8888」、「拒答:5」
audit_noresponse <- function(nr) {
  nr <- as.character(ifelse(is.na(nr), "", nr))
  # 原文：[;；\r\n]+
  parts <- strsplit(nr, "[;\uff1b\r\n]+", perl = TRUE)
  row <- rep(seq_along(parts), lengths(parts))
  p <- trimws(unlist(parts, use.names = FALSE))
  # 原文：^[^:：]*[:：][ \t]*-?[0-9]+$
  ok <- grepl("^[^:\uff1a]*[:\uff1a][ \t]*-?[0-9]+$", p, perl = TRUE)
  p <- p[ok]
  data.frame(
    row = row[ok],
    # 原文：[:：].*$
    label = trimws(sub("[:\uff1a].*$", "", p, perl = TRUE)),
    # 原文：^.*[:：][ \t]*(-?[0-9]+)$
    code = as.integer(sub("^.*[:\uff1a][ \t]*(-?[0-9]+)$", "\\1", p, perl = TRUE)),
    text = p,
    stringsAsFactors = FALSE
  )
}

# 某個字元在字串裡出現幾次（[(（] 這種 regex）
audit_count <- function(x, pattern) {
  nchar(x) - nchar(gsub(pattern, "", x, perl = TRUE))
}

#' Check the content of an item bank
#'
#' Looks for content problems that need a person to decide: missing-value codes that clash with
#' an answer code, two missing-value labels sharing one code, reverse scoring that disagrees
#' with the 「反向題」 dimension, new versions that lost their dimensions, two versions of one
#' item in the same wave, gaps in `itemVer`, empty or unexpanded `value`, empty `itemType`,
#' dimensions with one item or only deprecated items, stray newlines, spaces and unbalanced
#' brackets, and simplified Chinese characters. Nothing is changed.
#'
#' @param bank A bank from [load_bank()] or [read_bank_xlsx()].
#' @return A data frame, one row per problem, with columns `check` (short code), `level`
#'   (`"todo"`: should be fixed; `"note"`: worth a look), `itemID`, `itemVer` (integer; `NA`
#'   when the problem is about the item as a whole or a dimension), `dimID` (`NA` unless the
#'   problem is about a dimension) and `message` (one line in Traditional Chinese).
#' @seealso [bank_audit_summary()] for counts per check, [check_bank()].
#' @examples
#' audit <- bank_audit(load_bank("parent"))
#' bank_audit_summary(audit)
#' head(audit[audit$check == "noresponse_clash", ])
#' @export
bank_audit <- function(bank) {
  ib <- bank$itemBank
  n <- nrow(ib)
  txt <- function(col) {
    v <- if (col %in% names(ib)) as.character(ib[[col]]) else rep("", n)
    v[is.na(v)] <- ""
    v
  }
  int <- function(col) {
    v <- if (col %in% names(ib)) suppressWarnings(as.integer(ib[[col]])) else rep(0L, n)
    v[is.na(v)] <- 0L
    v
  }
  id <- txt("itemID")
  ver <- suppressWarnings(as.integer(ib$itemVer))
  dep <- int("deprecated")
  rev <- int("reverseScored")
  out <- list()
  add <- function(check, item = NA_character_, v = NA_integer_, dim = NA_character_, msg) {
    k <- length(msg)
    if (k == 0L || length(item) == 0L) return(invisible())
    out[[length(out) + 1L]] <<- data.frame(
      check = rep(check, k), itemID = rep_len(as.character(item), k),
      itemVer = rep_len(as.integer(v), k), dimID = rep_len(as.character(dim), k),
      message = msg, stringsAsFactors = FALSE
    )
  }

  # ---- 遺漏值 ----------------------------------------------------------------
  codes <- audit_value_codes(txt("value"))
  nr <- audit_noresponse(txt("noResponse"))
  if (nrow(nr) > 0L) {
    # 和有效選項撞號：一題一版一個代碼一列
    hit <- mapply(function(r, cd) cd %in% codes[[r]], nr$row, nr$code)
    x <- nr[hit, , drop = FALSE]
    if (nrow(x) > 0L) {
      g <- split(x, paste(x$row, x$code, sep = "\r"))
      g <- g[order(vapply(g, function(z) z$row[1], 1L))]
      r <- vapply(g, function(z) z$row[1], 1L)
      msg <- vapply(g, function(z) {
        # 原文：遺漏值 | 「 | 」 | 和選項 
        paste0("\u907a\u6f0f\u503c", paste0("\u300c", z$text, "\u300d", collapse = ""), "\u548c\u9078\u9805 ", z$code[1],
               # 原文： 同一個代碼
               " \u540c\u4e00\u500b\u4ee3\u78bc")
      }, "")
      add("noresponse_clash", id[r], ver[r], msg = unname(msg))
    }
    # 同一題兩種標籤同一個代碼
    k <- paste(nr$row, nr$code, sep = "\r")
    labs <- tapply(nr$label, k, function(z) unique(z), simplify = FALSE)
    many <- labs[lengths(labs) > 1L]
    if (length(many) > 0L) {
      r <- as.integer(sub("\r.*$", "", names(many)))
      cd <- sub("^.*\r", "", names(many))
      o <- order(r)
      msg <- vapply(seq_along(many), function(i) {
        # 原文：遺漏值 | 「 | 」 | 和 | 都用代碼 
        paste0("\u907a\u6f0f\u503c", paste0("\u300c", many[[i]], "\u300d", collapse = "\u548c"), "\u90fd\u7528\u4ee3\u78bc ", cd[i])
      }, "")
      add("noresponse_shared", id[r[o]], ver[r[o]], msg = msg[o])
    }
  }

  # ---- 構面、波次矩陣 -------------------------------------------------------------
  key <- paste(id, ver, sep = "\r")
  mat <- function(tbl) {
    df <- bank[[tbl]]
    if (is.null(df) || nrow(df) == 0L) return(NULL)
    cols <- get_matrix_cols(df)
    m <- matrix(0L, nrow = n, ncol = length(cols), dimnames = list(NULL, cols))
    j <- match(key, paste(df$itemID, df$itemVer, sep = "\r"))
    ok <- !is.na(j)
    for (cl in cols) m[ok, cl] <- as.integer(df[[cl]][j[ok]] == 1L)
    m[is.na(m)] <- 0L
    m
  }
  D <- mat("itemDim")
  W <- mat("itemWave")
  by_id <- split(seq_len(n), id)
  multi <- by_id[lengths(by_id) > 1L]

  # 反向計分 vs 「反向題」構面
  # 原文：反向題
  if (!is.null(D) && "\u53cd\u5411\u984c" %in% colnames(D)) {
    # 原文：反向題
    in_rev <- D[, "\u53cd\u5411\u984c"] == 1L
    x <- which(in_rev & rev != 1L)
    add("reverse_mismatch", id[x], ver[x],
        # 原文：在「反向題」構面，但 reverseScored 是 0
        msg = rep("\u5728\u300c\u53cd\u5411\u984c\u300d\u69cb\u9762\uff0c\u4f46 reverseScored \u662f 0", length(x)))
    x <- which(!in_rev & rev == 1L)
    add("reverse_mismatch", id[x], ver[x],
        # 原文：reverseScored 是 1，但不在「反向題」構面
        msg = rep("reverseScored \u662f 1\uff0c\u4f46\u4e0d\u5728\u300c\u53cd\u5411\u984c\u300d\u69cb\u9762", length(x)))
  }

  # 啟用的版本沒有構面，舊版有
  if (!is.null(D) && ncol(D) > 0L && length(multi) > 0L) {
    nd <- rowSums(D)
    for (rows in multi) {
      act <- rows[dep[rows] == 0L & nd[rows] == 0L]
      old <- rows[nd[rows] > 0L]
      if (length(act) == 0L || length(old) == 0L) next
      src <- old[which.max(ver[old])]
      dims <- colnames(D)[D[src, ] == 1L]
      # 原文： 等  |  個
      more <- if (length(dims) > 3L) paste0(" \u7b49 ", length(dims), " \u500b") else ""
      # 原文：這一版沒有構面；第  |  版有：
      msg <- paste0("\u9019\u4e00\u7248\u6c92\u6709\u69cb\u9762\uff1b\u7b2c ", ver[src], " \u7248\u6709\uff1a",
                    # 原文：、
                    paste(col_display_labels(utils::head(dims, 3L)), collapse = "\u3001"), more)
      add("dim_not_inherited", id[act], ver[act], msg = rep(msg, length(act)))
    }
  }

  # 同一題兩版在同一波
  if (!is.null(W) && ncol(W) > 0L && length(multi) > 0L) {
    for (rows in multi) {
      sub_w <- W[rows, , drop = FALSE]
      both <- which(colSums(sub_w) > 1L)
      if (length(both) == 0L) next
      vs <- sort(ver[rows][rowSums(sub_w[, both, drop = FALSE]) > 0L])
      waves <- colnames(W)[both]
      # 原文： 等  |  個波次
      more <- if (length(waves) > 3L) paste0(" \u7b49 ", length(waves), " \u500b\u6ce2\u6b21") else ""
      add("versions_same_wave", id[rows[1]],
          # 原文：第  | 、 |  版都在 
          msg = paste0("\u7b2c ", paste(vs, collapse = "\u3001"), " \u7248\u90fd\u5728 ",
                       # 原文：、
                       paste(col_display_labels(utils::head(waves, 3L)), collapse = "\u3001"), more))
    }
  }

  # itemVer 不連號
  for (rows in by_id) {
    vs <- sort(ver[rows])
    if (anyNA(vs) || identical(vs, seq_along(vs))) next
    miss <- setdiff(seq_len(max(vs)), vs)
    add("itemver_gap", id[rows[1]],
        # 原文：版本是  | 、
        msg = paste0("\u7248\u672c\u662f ", paste(vs, collapse = "\u3001"),
                     # 原文：，少了第  | 、 |  版
                     if (length(miss)) paste0("\uff0c\u5c11\u4e86\u7b2c ", paste(miss, collapse = "\u3001"), " \u7248")
                     # 原文：，有重複的版本號
                     else "\uff0c\u6709\u91cd\u8907\u7684\u7248\u672c\u865f"))
  }

  # 構面：只有 1 題、只剩停用的題
  if (!is.null(D) && ncol(D) > 0L) {
    for (cl in colnames(D)) {
      rows <- which(D[, cl] == 1L)
      if (length(rows) == 0L) next
      ids <- unique(id[rows])
      if (length(ids) == 1L) {
        add("dim_single_item", ids, if (length(rows) == 1L) ver[rows] else NA_integer_, cl,
            # 原文：構面「 | 」只有 1 題：
            msg = paste0("\u69cb\u9762\u300c", col_display_labels(cl), "\u300d\u53ea\u6709 1 \u984c\uff1a", ids))
      } else if (all(dep[rows] == 1L)) {
        add("dim_only_deprecated", dim = cl,
            # 原文：構面「 | 」的 
            msg = paste0("\u69cb\u9762\u300c", col_display_labels(cl), "\u300d\u7684 ", length(ids),
                         # 原文： 題都是停用的版本
                         " \u984c\u90fd\u662f\u505c\u7528\u7684\u7248\u672c"))
      }
    }
  }

  # ---- 選項、題型 --------------------------------------------------------------
  value <- txt("value")
  x <- which(!nzchar(trimws(value)))
  # 原文：選項（value）是空的
  add("value_empty", id[x], ver[x], msg = rep("\u9078\u9805\uff08value\uff09\u662f\u7a7a\u7684", length(x)))
  scale_only <- nzchar(trimws(value)) & lengths(codes) == 0L &
    # 原文：點量表|^\\s*[0-9]+\\s*[~～-]\\s*[0-9]+\\s*$
    grepl("\u9ede\u91cf\u8868|^\\s*[0-9]+\\s*[~\uff5e-]\\s*[0-9]+\\s*$", value, perl = TRUE)
  x <- which(scale_only)
  add("value_scale_only", id[x], ver[x],
      # 原文：選項只寫「 | 」，沒有每個選項的文字
      msg = paste0("\u9078\u9805\u53ea\u5beb\u300c", audit_short(trimws(value[x])), "\u300d\uff0c\u6c92\u6709\u6bcf\u500b\u9078\u9805\u7684\u6587\u5b57"))
  x <- which(!nzchar(trimws(txt("itemType"))))
  # 原文：題型（itemType）是空的
  add("itemtype_empty", id[x], ver[x], msg = rep("\u984c\u578b\uff08itemType\uff09\u662f\u7a7a\u7684", length(x)))

  # ---- 文字 ------------------------------------------------------------------
  for (col in c("label", "stemID")) {
    v <- txt(col)
    nm <- AUDIT_TEXT_COLS[[col]]
    x <- which(grepl("[\r\n]", v))
    # 原文：裡有換行
    add("text_newline", id[x], ver[x], msg = rep(paste0(nm, "\u88e1\u6709\u63db\u884c"), length(x)))
    # 原文：^[ \t　]|[ \t　]$
    x <- which(grepl("^[ \t\u3000]|[ \t\u3000]$", v, perl = TRUE))
    # 原文：前後有空白
    add("text_space", id[x], ver[x], msg = rep(paste0(nm, "\u524d\u5f8c\u6709\u7a7a\u767d"), length(x)))
    # 原文：[(（] | [)）]
    x <- which(audit_count(v, "[(\uff08]") != audit_count(v, "[)\uff09]"))
    add("text_brackets", id[x], ver[x],
        # 原文：的括號不成對：「 | 」
        msg = paste0(nm, "\u7684\u62ec\u865f\u4e0d\u6210\u5c0d\uff1a\u300c", audit_short(v[x]), "\u300d"))
  }
  simp_re <- paste0("[", paste(names(AUDIT_SIMPLIFIED), collapse = ""), "]")
  for (col in intersect(names(AUDIT_TEXT_COLS), names(ib))) {
    v <- txt(col)
    x <- which(grepl(simp_re, v, perl = TRUE))
    if (length(x) == 0L) next
    msg <- vapply(x, function(i) {
      ch <- unique(regmatches(v[i], gregexpr(simp_re, v[i], perl = TRUE))[[1]])
      # 原文：有簡體字
      paste0(AUDIT_TEXT_COLS[[col]], "\u6709\u7c21\u9ad4\u5b57",
             # 原文：「 | 」（應為「 | 」） | 、
             paste0("\u300c", ch, "\u300d\uff08\u61c9\u70ba\u300c", AUDIT_SIMPLIFIED[ch], "\u300d\uff09", collapse = "\u3001"))
    }, "")
    add("simplified_char", id[x], ver[x], msg = msg)
  }

  if (length(out) == 0L) {
    return(data.frame(check = character(0), level = character(0), itemID = character(0),
                      itemVer = integer(0), dimID = character(0), message = character(0),
                      stringsAsFactors = FALSE))
  }
  res <- do.call(rbind, out)
  res$level <- AUDIT_CHECKS$level[match(res$check, AUDIT_CHECKS$check)]
  res <- res[order(match(res$check, AUDIT_CHECKS$check), res$itemID, res$itemVer,
                   res$dimID, method = "radix"), c("check", "level", "itemID", "itemVer",
                                                   "dimID", "message")]
  rownames(res) <- NULL
  res
}

#' Count item bank problems per check
#'
#' @param audit The result of [bank_audit()].
#' @return A data frame with `check`, `level`, `label` (Traditional Chinese name of the check)
#'   and `n` (number of rows), one row per check that found something, `todo` first.
#' @examples
#' bank_audit_summary(bank_audit(load_bank("parent")))
#' @export
bank_audit_summary <- function(audit) {
  n <- table(factor(audit$check, levels = AUDIT_CHECKS$check))
  out <- data.frame(check = AUDIT_CHECKS$check, level = AUDIT_CHECKS$level,
                    label = AUDIT_CHECKS$label, n = as.integer(n), stringsAsFactors = FALSE)
  out <- out[out$n > 0L, , drop = FALSE]
  rownames(out) <- NULL
  out
}
