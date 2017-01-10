modalBusy <- function(id, title, ...){
    msgHandler =  singleton(tags$head(tags$script('Shiny.addCustomMessageHandler("jsCode",
    function(message) {
        console.log(message)
        eval(message.code);
    });'
    )
    )
    )
    
    label_id = paste(id, "label", sep='-')
    modal_tag <- div(id=id,
    class="modal hide fade",
    "aria-hidden"=FALSE,
    "aria-labelledby"=label_id,
    "role"="dialog",
    "tabindex"="-1",
    "data-keyboard"=FALSE,
    "data-backdrop"="static")
    header_tag <- div(class="modal-header",
    h3(id=label_id, title))
    body_tag <- div(class="modal-body",
    Row(...))
    footer_tag <- div(class="modal-footer")
    modal_tag <- tagAppendChildren(modal_tag, header_tag, body_tag, footer_tag)
    tagList(msgHandler, modal_tag)
}