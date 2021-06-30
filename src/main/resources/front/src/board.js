const ceilSize = 100

function boardWrapper(html, dim) {
    let width = dim*ceilSize + 10 // + borders
    return "<div style='width: "+width+"px; margin: auto; margin-top: 5%'>" + html + "</div>"
}

function getRow(dim) {
    let html = ""
    for (let i=0; i<dim; i++) {
        html+=getCeil()
    }
    return html
}

function getCeil() {
    return "<div class='ceil' style='width: "+ceilSize+"px; height: "+ceilSize+"px'></div>"
}

function getBoard(dim) {
    let html = ""
    for(let i=0; i<dim; i++) {
        html+=getRow(dim)
    }
    return boardWrapper(html, dim)
}

export function initBoard(dim) {
    document.addEventListener('DOMContentLoaded', function() {
        document.getElementsByClassName("board")[0].outerHTML = getBoard(dim)

        let cells = document.getElementsByClassName("ceil");

        function sendQuery() {
            console.log("ceil clicked")
        }

        for (let i=0; i<cells.length; i++) {
            cells[i].addEventListener("click", sendQuery, false)
        }
    })
}