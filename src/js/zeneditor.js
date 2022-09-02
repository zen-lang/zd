function renderPreview() {
  var prNode = document.getElementById("edit-preview")
  var editorNode = document.getElementById("edit-page")
  var text = editorNode.value
  fetch(document.URL, {method: 'POST',
                       body: text})
    .then(response => {
      return response.text()
    })
    .then(data => {
      prNode.innerHTML = data
    })
}

// function savePreview() {
//   var prNode = document.getElementById("edit-preview")
//   var editorNode = document.getElementById("edit-page")
//   var text = editorNode.value
//   fetch(document.URL, {method: 'PUT',
//                        body: text})
//     .then(response => {
//       return response.text()
//       location.href = document.URL + '_save';
//     })

// }

document.addEventListener('DOMContentLoaded', function() {
  renderPreview()
  var editorNode = document.getElementById("edit-page")
  editorNode.addEventListener("input", renderPreview)
})
