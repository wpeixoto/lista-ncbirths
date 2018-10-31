# Lista de referências bibliográficas                                   ====
refs = list()
refs[["duracao_gravidez"]] =  list(
    url="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3777570/",
    title="Length of human pregnancy and contributors to its natural variation",
    name="duracao_gravidez"
  )
refs[["gravidez_wkpt"]] = list(
    url="https://pt.wikipedia.org/wiki/Gravidez",
    title="Gravidez",
    name="gravidez_wkpt"
  )
refs[["birth_canal"]] = list(
  url="http://rspb.royalsocietypublishing.org/content/285/1889/20181807",
  title="Human variation in the shape of the birth canal is significant and geographically structured",
  name="birth_canal"
)
refs[["cincinatti"]] = list(
  url="https://soumamae.com.br/o-peso-da-gravida-elemento-chave-nos-partos-prematuros/",
  title="O peso da grávida: elemento-chave nos partos prematuros",
  name="cincinatti"
)
refs[["gesta_mult"]] = list(
  url="http://millarhidrata.com.br/artigos/gestacao-multipla/",
  title="Gestação múltipla",
  name="gesta_multi"
)
refs[["vix_m"]] = list(
  url="https://www.vix.com/pt/bdm/bebe/gravida/materia/o-que-acontece-com-o-bebe-se-fumar-na-gravidez-riscos",
  title="O que acontece com o bebê se você fumar na gravidez? ",
  name="vix_m"
)


# Ref template
# refs[["NOME"]] = list(
#   url="",
#   title="",
#   name="NOME"
# )


# Variáveis auxiliares para geração da lista de referências             ====
current_ref_num = 0    # Zero significa: nenhuma referência foi feita ainda

refs_by_ord = integer(length(refs))  # Vetor vazio de inteiros

# Funções auxiliares para exibição de referências                       ====

ref_link <- function(ref_name) {
  ref = refs[[ref_name]]
  if (is.null(ref)) {
    return(paste("<!-- Referência", ref_name, "inválida -->"))
  }
  current_ref_num = current_ref_num + 1
  ref$ord = current_ref_num
  refs_by_ord[current_ref_num] = ref$name
  paste(
    paste0(      #  Abertura da tag <a>
      '<a href="#',
      ref$name, '">'
    ),
    paste0("[", current_ref_num, "]"),    # Número de referência
    "</a>"
  )
}

ref_list <- function() {
  ll = ""
  for(ind in 1:length(refs)) {
    ref = refs[[refs_by_ord[ind]]]
    ll = paste(
      ll,
      paste0(
        '<span id="', ref$name, '"> ',
        '<a href="', ref$url, '">',
        ref$title,
        '</a></span>'
      )
    )
  }
  return(ll)
}
