# preprocess_emojis.R
# Script para pré-processar arquivos .qmd e envolver emojis com \emojitext{}

library(stringi)
library(yaml) # Para ler o arquivo _quarto.yml

# Função para verificar se um caractere é um emoji
# Baseia-se na propriedade Unicode "Emoji"
is_emoji_char <- function(char) {
  if (is.na(char) || char == "") return(FALSE)
  # Obtém a propriedade Unicode do caractere
  prop <- stri_trans_general(char, "Any-Scpt")
  # Uma forma mais robusta é usar a propriedade Emoji diretamente via ICU
  # stringi não expõe diretamente a propriedade Emoji, mas podemos tentar uma abordagem baseada em blocos ou categorias.
  # A propriedade "Emoji" é uma propriedade booleana do Unicode.
  # Vamos usar uma abordagem baseada em detectar caracteres em blocos típicos de emojis.
  # Outra opção é usar pacotes como 'unmash' ou 'utf8' de forma mais específica.
  # Usaremos uma verificação baseada em blocos comum e na propriedade "Extended_Pictographic".
  # A função stri_unescape_unicode pode ser usada para verificação, mas o ideal é stri_enc_isutf8 combined with detection.
  # A forma mais confiável é usar a função stri_extract_all_char_class com a classe "emoji".
  # A partir do stringi 1.7.0+, isso é mais direto com expressões regulares ICU que reconhecem emojis.
  # Expressão regular ICU para um componente de emoji (pode ser complexo, simplificando para o ponto de código base).
  # Exemplo de regex ICU para sequências de emoji (simplificado):
  # "\p{Emoji}" detecta caracteres com a propriedade Emoji.
  # "\p{Extended_Pictographic}" detecta pictográficos estendidos (muitos emojis são isso).
  # Podemos usar stri_detect_regex com "\p{Emoji}" ou "\p{Extended_Pictographic}" ou uma combinação.

  # Combinação de propriedades para pegar a maioria dos emojis
  is_emoji_prop <- stri_detect_regex(char, "[\\p{Emoji}]") # Caractere base do emoji
  is_extended_picto <- stri_detect_regex(char, "[\\p{Extended_Pictographic}]") # Pictográficos estendidos (muitos emojis são isso)
  # Alguns emojis são sequências, então \p{Emoji} pode pegar a base e variações.
  # Para um único caractere, \p{Emoji} é um bom indicador.
  # Exemplo: U+1F44D (👍) tem a propriedade Emoji.
  # Exemplo: U+1F466 (👦) tem a propriedade Emoji.
  # Exemplo: U+2764 (❤) tem a propriedade Emoji.
  # Exemplo: U+1F3FB (肤色 tom) é um modificador, pode ou não ser capturado sozinho, mas é parte de sequências.
  # A propriedade \p{Emoji} é mais apropriada para caracteres base.
  # \p{Extended_Pictographic} pode capturar alguns símbolos que não são estritamente "emoji" mas são usados como tal.
  # Vamos usar \p{Emoji} como critério principal para este script.
  return(stri_detect_regex(char, "\\p{Emoji}"))
}

# Lê o arquivo _quarto.yml
config_file <- "_quarto.yml"
if (!file.exists(config_file)) {
  stop("Arquivo de configuração _quarto.yml não encontrado no diretório atual.")
}

config <- read_yaml(config_file)

# Extrai a lista de capítulos
chapters <- config$book$chapters

if (is.null(chapters)) {
  stop("Chave 'book: chapters' não encontrada em _quarto.yml")
}

# Filtra apenas arquivos .qmd
qmd_files <- chapters[endsWith(chapters, ".qmd")]

if (length(qmd_files) == 0) {
  message("Nenhum arquivo .qmd encontrado nas 'chapters' do _quarto.yml.")
  quit(status = 0) # Sai sem erro se não houver arquivos
}

message("Processando os seguintes arquivos .qmd:")
print(qmd_files)

for (file in qmd_files) {
  if (!file.exists(file)) {
    warning("Arquivo não encontrado: ", file, ". Pulando...")
    next
  }

  message("\nLendo arquivo: ", file)
  content <- readLines(file, warn = FALSE, encoding = "UTF-8")

  # Junta o conteúdo em uma string única para facilitar a substituição
  full_content <- paste(content, collapse = "\n")

  # Encontra todos os caracteres que são emojis
  # stringi pode extrair caracteres baseados em classes Unicode
  # Usamos stri_extract_all_chars para extrair caracteres e depois verificar
  # Ou usamos stri_replace_all_regex com uma captura para substituir

  # Regex para capturar um caractere que é um emoji
  # \X tenta capturar um "grapheme cluster", o que é mais próximo de como um humano vê um emoji (pode incluir modificadores de tom de pele)
  # Mas \p{Emoji} sozinho pode não capturar sequências como emoji + modificador de tom de pele como uma unidade.
  # Vamos usar uma regex que tenta capturar um gráfico estendido que contenha um emoji.
  # A regex "(?[\p{Emoji}\p{Emoji_Modifier}\p{Emoji_Component}])(?[\p{Emoji}\p{Emoji_Modifier}\p{Emoji_Component}]*)"
  # pode ser usada com ICU para capturar sequências de componentes de emoji.
  # No stringi R, usamos stri_*_regex.
  # A regex ICU para um gráfico estendido de emoji é mais complexa.
  # Para simplificar, usaremos \p{Emoji} e assumiremos que emojis compostos são capturados corretamente pelo mecanismo de gráfico estendido do ICU se necessário,
  # mas para maioria dos casos básicos, \p{Emoji} em um contexto de captura de caractere único ou cluster pode funcionar com \X.
  # Exemplo de tentativa com \X (Grapheme Cluster) e \p{Emoji}:
  # "(?>(?:\X)(?=\p{Emoji}))" ou simplesmente aplicar a lógica a cada caractere extraído.
  # A abordagem mais robusta é extrair gráficos estendidos e verificar se o primeiro ou algum componente é um emoji.
  # stri_extract_all_regex(full_content, "\\X") extrai gráficos estendidos.
  # Mas substituir pode ser complicado.
  # Vamos tentar uma abordagem direta com stri_replace_all_regex e a classe \p{Emoji}.
  # A documentação do stringi indica que \p{Emoji} é suportado pelo ICU.
  # A regex deve capturar o emoji e substituir por \emojitext{o_emoji_capturado}.
  # Exemplo: stri_replace_all_regex(full_content, "(\\p{Emoji})", "\\\\emojitext{\\\\1}")

  # Importante: No R, para representar um \ literal em uma string, você precisa escapar: "\\"
  # Portanto, para gerar um literal \emojitext{...} na saída, a string de substituição deve ser "\\\\emojitext{\\1}"
  # Onde \1 é o grupo capturado (o emoji).

  # Regex para capturar um gráfico estendido que contenha pelo menos um caractere Emoji
  # Isso é mais robusto para emojis compostos (como pessoa + tom de pele + cabelo)
  emoji_grapheme_regex <- "(?>(?:\\p{Emoji_Modifier_Base}|\\p{Emoji}|\\p{Emoji_Presentation}|\\p{Emoji_Component})[\\p{Emoji_Modifier}?\\p{Variant_Selector}?\\p{Tag_Sequences}?\\p{Regional_Indicator}?]?)"

  # Substitui o gráfico estendido do emoji por \emojitext{o_gráfico_estendido}
  new_content <- stri_replace_all_regex(
    full_content,
    emoji_grapheme_regex,
    "\\\\emojitext{\\1}",
    vectorize_all = FALSE
  )

  # Se a substituição não alterar o conteúdo, significa que nenhum emoji foi encontrado
  if (identical(full_content, new_content)) {
    message("Nenhum emoji encontrado em ", file, ". Arquivo não modificado.")
  } else {
    message("Emojis encontrados e substituídos em ", file, ". Salvando alterações...")
    # Escreve o conteúdo modificado de volta ao arquivo
    writeLines(new_content, file, useBytes = TRUE) # useBytes=TRUE pode ajudar a preservar UTF-8
  }
}

message("\nPré-processamento de emojis concluído.")