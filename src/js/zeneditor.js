function renderPreview() {
  const prNode = document.getElementById("edit-preview")
  const editorNode = document.getElementById("edit-page")

  const text = editorNode.value
  fetch(document.URL, {method: 'POST',
                       body: text})
    .then(response => {
      return response.text()
    })
    .then(data => {
      prNode.innerHTML = data
    })
}

function savePreview() {
  const editorNode = document.getElementById("edit-page")
  const text = editorNode.value

  const spinner = document.getElementById("spinner")
  spinner.classList.add("show-spinner")

  fetch(document.URL, {method: 'PUT',
                       body: text})
    .then(response => {
      return response.text()
    })
    .then(data => {
      location.href = data;
    })
    .catch(() => {
      spinner.classList.remove("show-spinner")
      alert("Что то сломалось. Сори, это пока бета, можете попробовать еще раз.")

    })
}

document.addEventListener('DOMContentLoaded', function() {
  renderPreview()
  const editorNode = document.getElementById("edit-page")
  editorNode.addEventListener("input", renderPreview)
})
