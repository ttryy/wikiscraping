library("lexRankr")
library("WikipediR")
library("rvest")

wikipedia_title <- "Nupedia"

wikipedia_page <- page_content(language="DE", project="wikipedia", page_name=wikipedia_title)
text <- html_text(html_nodes(read_html(pluck(wikipedia_page$parse$text["*"], 1, type="STRING")), css = ".mw-parser-output p"))

top_3 = lexRank(text, docId = rep(1, length(text)), n = 3, continuous = TRUE, damping = 0.85)

order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
ordered_top_3 = append(text[1], top_3[order_of_appearance, "sentence"])
print(ordered_top_3)